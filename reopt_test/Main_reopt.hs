{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import qualified Data.BitVector as BV
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Elf
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Version
import           Data.Word
import           Debug.Trace
import           System.Console.CmdArgs.Explicit as CmdArgs
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO

import           Numeric (readHex)

import           System.Posix.Waitpid as W
import           System.Posix.Types
import           System.Posix.Process
import           System.Linux.Ptrace.Syscall
import           System.Linux.Ptrace.Types
import           System.Linux.Ptrace.X86_64Regs
import           System.Linux.Ptrace.X86_64FPRegs

import           Paths_reopt (version)

import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import qualified Data.Parameterized.Map as MapF
import           Flexdis86 (InstructionInstance(..), ppInstruction,
                  ByteReader(..), defaultX64Disassembler,
                  disassembleInstruction, LockPrefix(..))

import           Reopt.CFG.Representation
import           Reopt.Concrete.BitVector
import           Reopt.Concrete.MachineState as MS
import           Reopt.Concrete.MachineState ()
import           Reopt.Concrete.Semantics
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory
import           Reopt.Object.Loader
import           Reopt.Semantics.FlexdisMatcher
import qualified Reopt.Semantics.Monad as SM

import           SignalUtils (statusToString)

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = Application    -- * Exectue and simulate in parallel, printing errors
   | Test            -- * Execute a single-instruction test in parallel
   | Instr
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version

-- | Command line arguments.
data Args = Args { _reoptAction :: !Action
                 , _programPath :: !FilePath
                 , _loadStyle   :: !LoadStyle
                 }

-- | How to load Elf file.
data LoadStyle
   = LoadBySection
     -- ^ Load loadable sections in Elf file.
   | LoadBySegment
     -- ^ Load segments in Elf file.

-- | Action to perform when running.
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path to load
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Application
                   , _programPath = ""
                   , _loadStyle = LoadBySection
                   }

------------------------------------------------------------------------
-- Argument processing

arguments :: Mode Args
arguments = mode "reopt_test" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ instrFlag
                , testFlag
                , applicationFlag
                , flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                ]

applicationFlag :: CmdArgs.Flag Args
applicationFlag = flagNone [ "application", "a"] upd help
  where upd = reoptAction .~ Application
        help = "Test concrete semantics by executing in parallel with a binary"

testFlag :: CmdArgs.Flag Args
testFlag = flagNone [ "test", "t"] upd help
  where upd = reoptAction .~ Test
        help = "Test concrete semantics by executing in parallel with a single-instruction test"

instrFlag :: CmdArgs.Flag Args
instrFlag = flagNone [ "instructions", "i"] upd help
  where upd = reoptAction .~ Instr
        help = "Print disassembly of executed instructions in a binary"

reoptVersion :: String
reoptVersion = "Reopt semantics verifier (reopt_test) "
             ++ versionString ++ ", June 2014."
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r

copyrightNotice :: String
copyrightNotice = "Copyright 2014 Galois, Inc. All rights reserved."

filenameArg :: Arg Args
filenameArg = Arg { argValue = setFilename
                  , argType = "FILE"
                  , argRequire = False
                  }
  where setFilename :: String -> Args -> Either String Args
        setFilename nm a = Right (a & programPath .~ nm)

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      putStrLn msg
      exitFailure
    Right v -> return v

------------------------------------------------------------------------
-- Execution

showUsage :: IO ()
showUsage = do
  putStrLn "For help on using reopt, run \"reopt --help\"."

readElf64 :: FilePath -> IO (Elf Word64)
readElf64 path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  bs <- B.readFile path
  case parseElf bs of
    Left (_,msg) -> do
      putStrLn $ "Error reading " ++ path ++ ":"
      putStrLn $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      putStrLn "32-bit executables are not yet supported."
      exitFailure
    Right (Elf64 e) ->
      return e

readStaticElf :: FilePath -> IO (Elf Word64)
readStaticElf path = do
  e <- readElf64 path
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  return e

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e

------------------------------------------------------------------------
-- Tracers.

traceFile :: FilePath -> IO CPid
traceFile path = do
  child <- forkProcess $ traceChild path
  waitpid child []
  return child

traceChild :: FilePath -> IO ()
traceChild file = do
  ptrace_traceme
  executeFile file False [] Nothing
  fail "EXEC FAILED"

traceInner :: (CPid -> StateT s IO ()) -> CPid -> StateT s IO ()
traceInner act pid = do
  act pid
  lift $ ptrace_singlestep pid Nothing
  status <- lift $ waitForRes pid
  case status of        W.Stopped _ -> traceInner act pid
                        Signaled _ -> traceInner act pid
                        Continued -> traceInner act pid
                        W.Exited _ -> return ()

------------------------------------------------------------------------
-- Tests.

-- | Test builder.
mkTest :: Args
  -> WriterT [String] (ConcreteStateT PTraceMachineState) ()
  -> IO ()
mkTest args test = do
  child <- traceFile $ args^.programPath
  procMem <- openChildProcMem child
  procMaps <- openChildProcMaps child
  (((), out), _) <- runPTraceMachineState (PTraceInfo {cpid = child, memHandle = procMem, mapHandle = procMaps}) $ do
    regs <- dumpRegs
    runConcreteStateT (runWriterT test) Map.empty regs
  mapM_ putStrLn out
  where
    openChildProcMem :: CPid -> IO Handle
    openChildProcMem pid = do
      openFile ("/proc/" ++ (show pid) ++ "/mem") ReadWriteMode

    openChildProcMaps :: CPid -> IO Handle
    openChildProcMaps pid = do
      openFile ("/proc/" ++ (show pid) ++ "/maps") ReadMode

------------------------------------------------------------------------

testApplication :: Args -> IO ()
testApplication args = mkTest args test
  where
    test = runInParallel checkAndClear

testSingleInstruction :: Args -> IO ()
testSingleInstruction args = mkTest args instTest

printExecutedInstructions :: Args -> IO ()
printExecutedInstructions args = do
  e <- readStaticElf (args^.programPath)
  let Identity mem = mkElfMem (args^.loadStyle) e
  child <- traceFile $ args^.programPath
  runStateT (traceInner (printInstr mem) child) ()
  return ()
  where
    printInstr :: Memory Word64 -> CPid -> StateT s IO ()
    printInstr mem pid = do
      regs <- lift $ ptrace_getregs pid
      case regs
        of X86 _ -> fail "X86Regs! only 64 bit is handled"
           X86_64 regs64 -> do
             let rip_val = rip regs64
             case readInstruction mem rip_val
               of Left err -> lift $ putStrLn $ "Couldn't disassemble instruction " ++ show err
                  Right (ii, nextAddr) -> lift $ putStrLn $ show $ ppInstruction nextAddr ii

------------------------------------------------------------------------
-- Register translation.

-- | Translate PTrace register format to Reopt register format.
translatePtraceRegs :: X86_64Regs -> X86_64FPRegs -> X86State MS.Value
translatePtraceRegs ptraceRegs ptraceFPRegs =
  mkX86State fillReg
  where
    fillReg :: N.RegisterName cl -> MS.Value (N.RegisterType cl)
    fillReg N.IPReg = mkLit64 (rip ptraceRegs)
    fillReg (N.GPReg 0)  = mkLit64 (rax ptraceRegs)
    fillReg (N.GPReg 1)  = mkLit64 (rcx ptraceRegs)
    fillReg (N.GPReg 2)  = mkLit64 (rdx ptraceRegs)
    fillReg (N.GPReg 3)  = mkLit64 (rbx ptraceRegs)
    fillReg (N.GPReg 4)  = mkLit64 (rsp ptraceRegs)
    fillReg (N.GPReg 5)  = mkLit64 (rbp ptraceRegs)
    fillReg (N.GPReg 6)  = mkLit64 (rsi ptraceRegs)
    fillReg (N.GPReg 7)  = mkLit64 (rdi ptraceRegs)
    fillReg (N.GPReg 8)  = mkLit64 (r8 ptraceRegs)
    fillReg (N.GPReg 9)  = mkLit64 (r9 ptraceRegs)
    fillReg (N.GPReg 10) = mkLit64 (r10 ptraceRegs)
    fillReg (N.GPReg 11) = mkLit64 (r11 ptraceRegs)
    fillReg (N.GPReg 12) = mkLit64 (r12 ptraceRegs)
    fillReg (N.GPReg 13) = mkLit64 (r13 ptraceRegs)
    fillReg (N.GPReg 14) = mkLit64 (r14 ptraceRegs)
    fillReg (N.GPReg 15) = mkLit64 (r15 ptraceRegs)
    fillReg (N.SegmentReg 0) = mkLit16 (es ptraceRegs)
    fillReg (N.SegmentReg 1) = mkLit16 (cs ptraceRegs)
    fillReg (N.SegmentReg 2) = mkLit16 (ds ptraceRegs)
    fillReg (N.SegmentReg 3) = mkLit16 (ss ptraceRegs)
    fillReg (N.SegmentReg 4) = mkLit16 (fs ptraceRegs)
    fillReg (N.SegmentReg 5) = mkLit16 (gs ptraceRegs)
    fillReg (N.FlagReg n) = Literal $ bitVector knownNat $ bitVec 1
                            (if testBit (eflags ptraceRegs) n
                               then 1
                               else 0 :: Int)
    fillReg (N.ControlReg _) = Undefined $ BVTypeRepr  knownNat
    fillReg (N.X87ControlReg n) = Literal $ bitVector knownNat $ BV.extract n n cwd'
    fillReg (N.X87StatusReg n) = Literal $ bitVector knownNat $ BV.extract n n swd'
    fillReg N.X87TopReg = Literal $ bitVector knownNat $
                            BV.extract (13 :: Int) 11 swd'
    fillReg N.X87PC =  Literal $ bitVector knownNat $ bitVec 2 $
                            BV.extract (9 :: Int) 8 $ cwd'
    fillReg N.X87RC =  Literal $ bitVector knownNat $ bitVec 2 $
                            BV.extract (11 :: Int) 10 $ cwd'
    fillReg (N.X87TagReg _) = Undefined $ BVTypeRepr  knownNat
    fillReg (N.X87FPUReg 0) = mkLit80 $ st0 ptraceFPRegs
    fillReg (N.X87FPUReg 1) = mkLit80 $ st1 ptraceFPRegs
    fillReg (N.X87FPUReg 2) = mkLit80 $ st2 ptraceFPRegs
    fillReg (N.X87FPUReg 3) = mkLit80 $ st3 ptraceFPRegs
    fillReg (N.X87FPUReg 4) = mkLit80 $ st4 ptraceFPRegs
    fillReg (N.X87FPUReg 5) = mkLit80 $ st5 ptraceFPRegs
    fillReg (N.X87FPUReg 6) = mkLit80 $ st6 ptraceFPRegs
    fillReg (N.X87FPUReg 7) = mkLit80 $ st7 ptraceFPRegs
    fillReg (N.XMMReg 0) = mkLit128 (xmm0 ptraceFPRegs)
    fillReg (N.XMMReg 1) = mkLit128 (xmm1 ptraceFPRegs)
    fillReg (N.XMMReg 2) = mkLit128 (xmm2 ptraceFPRegs)
    fillReg (N.XMMReg 3) = mkLit128 (xmm3 ptraceFPRegs)
    fillReg (N.XMMReg 4) = mkLit128 (xmm4 ptraceFPRegs)
    fillReg (N.XMMReg 5) = mkLit128 (xmm5 ptraceFPRegs)
    fillReg (N.XMMReg 6) = mkLit128 (xmm6 ptraceFPRegs)
    fillReg (N.XMMReg 7) = mkLit128 (xmm7 ptraceFPRegs)
    fillReg (N.XMMReg 8) = mkLit128 (xmm8 ptraceFPRegs)
    fillReg (N.XMMReg 9) = mkLit128 (xmm9 ptraceFPRegs)
    fillReg (N.XMMReg 10) = mkLit128 (xmm10 ptraceFPRegs)
    fillReg (N.XMMReg 11) = mkLit128 (xmm11 ptraceFPRegs)
    fillReg (N.XMMReg 12) = mkLit128 (xmm12 ptraceFPRegs)
    fillReg (N.XMMReg 13) = mkLit128 (xmm13 ptraceFPRegs)
    fillReg (N.XMMReg 14) = mkLit128 (xmm14 ptraceFPRegs)
    fillReg (N.XMMReg 15) = mkLit128 (xmm15 ptraceFPRegs)
    fillReg (N.DebugReg _) = Undefined $ BVTypeRepr  knownNat

    mkLit16 :: Word64 -> MS.Value (BVType 16)
    mkLit16 = Literal . bitVector knownNat . bitVec 16

    mkLit128 :: (Word64, Word64) -> MS.Value (BVType 128)
    mkLit128 (high, low) = Literal $ bitVector knownNat $ bitVec 128 $
      ((fromIntegral high) :: Integer) * 2^64 + fromIntegral low

    mkLit80 :: (Word16, Word64) -> MS.Value (BVType 80)
    mkLit80 (high, low) = Literal $ bitVector knownNat $ bitVec 80 $
      ((fromIntegral high) :: Integer) * 2^64 + fromIntegral low
    cwd' = bitVec 16 $ cwd ptraceFPRegs
    swd' = bitVec 16 $ swd ptraceFPRegs

mkLit64 :: Word64 -> MS.Value (BVType 64)
mkLit64 = Literal . bitVector knownNat . bitVec 64

------------------------------------------------------------------------
-- The 'PTraceMachineState' monad.

newtype PTraceMachineState a = PTraceMachineState {unPTraceMachineState ::
   ReaderT PTraceInfo IO a}
   deriving (Applicative, Monad, MonadReader PTraceInfo, MonadIO, Functor)

data PTraceInfo = PTraceInfo {cpid :: CPid, memHandle :: Handle, mapHandle :: Handle}

-- TODO(conathan): make a new @Monad*Read*MachineState@ and make
-- 'PTraceMachineState' an instance of that instead, since it doens't
-- define any of the write operations, primitives, or exceptions.
instance MonadMachineState PTraceMachineState where
  setMem = fail "setMem unimplemented for PTraceMachineState"
  getMem (Address width bv) = do
    memH <- asks memHandle
    bs <- liftIO $ readFileOffset memH  (fromIntegral $ nat bv) (fromIntegral (widthVal width) `div` 8)
    return $ Literal $ toBitVector width bs
    where
      readFileOffset :: Handle -> Word64 -> Word64 -> IO B.ByteString
      readFileOffset h addr width = do
        hSeek h AbsoluteSeek $ fromIntegral addr
        B.hGet h $ fromIntegral width

      toBitVector :: NatRepr n -> B.ByteString -> BitVector n
      toBitVector n bs =
        bitVector n $ bitVec (widthVal n) (B.foldl (\acc b -> acc*(2^(8::Integer)) + fromIntegral b) (0::Integer) bs)

  getReg regname = do
    regs <- dumpRegs
    return $ regs^.(register regname)
  setReg = fail "setReg unimplemented for PTraceMachineState"
  dumpRegs = do
     pid <- asks cpid
     regs <-liftIO $ ptrace_getregs pid
     fpregs <- liftIO $ ptrace_getfpregs pid
     case (regs, fpregs) of
       (X86_64 regs', X86_64FP fpregs') -> return $ translatePtraceRegs regs' fpregs'
       _ -> fail "64-bit only!"
  primitive = fail "primitive unimplemented for PTraceMachineState"
  getSegmentBase seg = do
    pid <- asks cpid
    regs <- liftIO $ ptrace_getregs pid
    case regs of
      X86_64 regs'
        | seg == SM.fs -> return $ mkLit64 (fs_base regs')
        | seg == SM.gs -> return $ mkLit64 (gs_base regs')
        -- We already treated the (trivial) segments other than FS and
        -- GS in 'Reopt.Semantics.FlexdisMatcher.getBVAddress'.
        | otherwise -> fail $ "getSegmentBase: bug: unexpected segment: " ++
                         show seg
      _ -> fail "getSegmentBase: 64-bit only!"

instance FoldableMachineState PTraceMachineState where
  foldMem8 f x = do
    memH <- asks memHandle
    mapH <- asks mapHandle
    memMap <- liftIO $ exploreMem memH mapH
    Map.foldrWithKey (\k v m -> do m' <- m; f k v m') (return x) memMap
    where
      exploreMem :: Handle -> Handle -> IO (Map Address8 MS.Value8)
      exploreMem memH mapH =
        exploreInner memH mapH Map.empty
        where
          exploreInner memH mapH acc = do
            line <- hGetLine mapH
            let (s1, rest) = splitFirst '-' line
            let (s2, _) = splitFirst ' ' rest
            a1 <- case readHex s1 of ((a1, _) : _) -> return a1
                                     _ -> fail "couldn't parse /proc/pid/map"
            a2 <- case readHex s2 of ((a2, _) : _) -> return a2
                                     _ -> fail "couldn't parse /proc/pid/map"
            acc' <- trace line $ trace ("loading range " ++ s1 ++ " - " ++ s2) (loadMem memH acc a1 a2)
            trace ("loaded range " ++ s1 ++ " - " ++ s2) (return ())
            ready <- hReady mapH
            if ready then exploreInner memH mapH acc else return acc'

          splitFirst c (c' : rest)
            | c == c' = ([], rest)
            | otherwise =
              case splitFirst c rest of (l1, l2) -> (c' : l1, l2)
          splitFirst c [] = ([], [])

      loadMem :: Handle
              -> Map Address8 MS.Value8
              -> Integer
              -> Integer
              -> IO (Map Address8 MS.Value8)
      loadMem memH map start stop = do
        hSeek memH AbsoluteSeek start
        buf <- B.hGet memH $ fromInteger $ stop - start
        return $ populateMap map start buf
        where
          populateMap map start buf =
            if B.null buf
            then map
            else populateMap
                   (Map.insert
                     (Address n8 $ bitVector n64 $ bitVec 64 start)
                     (Literal $ bitVector n8 $ bitVec 8 (B.head buf))
                     map)
                   (start + 1)
                   (B.tail buf)

runPTraceMachineState :: PTraceInfo -> PTraceMachineState a -> IO a
runPTraceMachineState info (PTraceMachineState {unPTraceMachineState = m}) = runReaderT m info

------------------------------------------------------------------------
-- Instruction disassembly.
--
-- We need a 'ByteReader' to run 'disassembleInstruction'.

newtype MachineByteReader m a = MachineByteReader (StateT (Address8, Int) m a) deriving (MonadTrans, MonadState (Address8, Int), Functor, Applicative, Monad)

instance (Functor m, MonadMachineState m) =>
  ByteReader (MachineByteReader m) where
    readByte = do
      (addr, disp) <- get
      put $ (modifyAddr (+ bitVec 64 (1 :: Int)) addr, disp + 1)
      val <- lift $ getMem addr
      case asBV val of Nothing -> fail $ "Could not read byte at " ++ show addr
                       Just bv -> return $ fromIntegral bv

runMachineByteReader :: Monad m => MachineByteReader m a -> Address8 ->  m (Int, a)
runMachineByteReader (MachineByteReader s) addr = do
  (v, (_, l)) <- runStateT s (addr, 0)
  return (l,v)

-- | Disassemble, from HW, the instruction at the given address.
getInstruction :: MonadMachineState m
  => Address8 -> m (Int, InstructionInstance)
getInstruction instrAddr =
  runMachineByteReader (disassembleInstruction defaultX64Disassembler)
                       instrAddr

------------------------------------------------------------------------

-- | Simulate one instruction using the concrete evaluator.
--
-- - read the current instruction from HW
-- - compute its semantics
-- - evaluate its semantics (@[Stmt]@) using the concrete evaluator
--
-- Returns a bool indicating whether the instruction's semantics were
-- defined or not.
stepConcrete :: (Functor m, MonadMachineState m)
             => WriterT [String] (ConcreteStateT m) (Bool, InstructionInstance)
stepConcrete = do
  rip' <- getReg N.rip
  bv <- case rip' of Literal bv -> return bv
                     Undefined _ -> fail "Undefined rip!"

  let instrAddr = Address knownNat bv
  (w, ii) <- getInstruction instrAddr
  tell [show ii]
  case execInstruction (fromIntegral $ (nat bv) +
                        fromIntegral w) ii
    of Just s -> do tell [show $ ppStmts  $ execSemantics s]
                    evalStateT (mapM_ evalStmt $ execSemantics s) MapF.empty
                    return (True, ii)
       Nothing -> do tell ["could not exec instruction at " ++ show rip']
                     return (False, ii)

runInParallel :: ((Bool, InstructionInstance) -> WriterT [String] (ConcreteStateT PTraceMachineState) ())
              -> WriterT [String] (ConcreteStateT PTraceMachineState) ()
runInParallel updater = do
  tell ["runInParallel stepping concrete semantics"]
  (execSuccess, ii) <-  stepConcrete
  pid <-lift $ lift $ asks cpid
  status <- case (iiLockPrefix ii)
    of RepPrefix -> step_to_next_inst pid
       RepZPrefix -> step_to_next_inst pid
       RepNZPrefix -> step_to_next_inst pid
       NoLockPrefix -> single_step pid
       LockPrefix -> single_step pid
  case status of        W.Exited _ -> return ()
                        W.Stopped 5 -> do updater (execSuccess, ii)
                                          runInParallel updater
                        _ -> do
                          str <- liftIO' $ statusToString status
                          tell ["Main_reopt.runInParallel: unexpected status: " ++ str]
                          return ()
  where
    -- | Single step until the IP changes.
    --
    -- We might get a more efficient implementation of
    -- 'step_to_next_inst' by replacing the subsequent instruction
    -- with INT3 (breakpoint, opcode 0xCC) and then running until we
    -- reach it. See
    -- http://stackoverflow.com/questions/3747852/int-3-0xcc-x86-asm.
    step_to_next_inst pid = do
      addr <- get_addr
      go addr
      where
        go addr = do
          liftIO' $ ptrace_singlestep pid Nothing
          status <- liftIO' $ waitForRes pid
          case status
             of W.Exited _ -> return status
                W.Stopped 5 -> do
                  addr' <- get_addr
                  if addr' == addr
                    then go addr
                    else return status
                _ -> do
                  str <- liftIO' $ statusToString status
                  tell ["Main_reopt.runInParallel.step_while_inst: unexpected status: " ++ str]
                  return status
        get_addr = liftIO' $ do
          X86_64 regs <- ptrace_getregs pid
          return $ rip regs
    single_step pid = liftIO' $ do
      ptrace_singlestep pid Nothing
      waitForRes pid
    liftIO' = lift . lift . liftIO

-- | Test a single instruction.
--
-- TODO: could we make this a special case of testing a whole
-- application? I.e., can we make this a special case of
-- 'runInParallel'.
instTest :: WriterT [String] (ConcreteStateT PTraceMachineState) ()
instTest = do
  pid <- lift $ lift $ asks cpid
  lift $ lift $ liftIO $ ptrace_cont pid Nothing
  status <- lift $ lift $ liftIO $ waitForRes pid
  case status
      of W.Exited _ -> return ()
         W.Stopped 5 -> do
           tell ["child stopped: " ++ show status]
           trapRegs <- lift $ lift $ liftIO $ ptrace_getregs pid
           trapFPRegs <- lift $ lift $ liftIO $ ptrace_getfpregs pid
           (modRegs, modFPRegs) <- case (trapRegs, trapFPRegs) of
             (X86_64 regs', X86_64FP fpregs')-> return (regs', fpregs')
             _ -> fail "64-bit only!"
           --lift $ liftIO $ ptrace_setregs pid $ X86_64 modRegs
           put (Map.empty, translatePtraceRegs modRegs modFPRegs)
           (execSuccess, ii) <- trace "instTest stepping concrete semantics" stepConcrete
           lift $ lift $ liftIO $ ptrace_singlestep pid Nothing
           status <- lift $ lift $ liftIO $ waitForRes pid
           case status
               of W.Exited _ -> return ()
                  W.Stopped 5 -> checkAndClear (execSuccess, ii)
                  _ -> tell ["Exception while executing instruction " ++ show ii]
         _ -> do
           tell ["child stopped: " ++ show status]
           instTest

-- | Check that emulated and real states agree and then clear state.
--
-- If the 'Bool' arg is false, then nothing is checked and the state
-- is cleared.
--
-- TODO(conathan): split this into a check, and a separate state
-- clearing function. I think the state clearing happens often.
checkAndClear :: (Bool, InstructionInstance) -> WriterT [String] (ConcreteStateT PTraceMachineState) ()
checkAndClear (True, ii) = do
  realRegs <- lift $ lift dumpRegs
  emuRegs <- dumpRegs
  let regCmp = compareRegs realRegs emuRegs
  memCmp <- compareMems
  case regCmp ++ memCmp of [] -> tell ["instruction " ++ show ii ++ " executed successfully"]
                           l -> tell (("After executing instruction " ++ show ii) : l)
  put (Map.empty, realRegs)
  where
    compareMems = lift $ foldMem8 (\addr val errs -> do
      realVal <- lift $ getMem addr
      if realVal `equalOrUndef` val
        then return errs
        else return $ errs ++ [show addr ++ " did not match"]) []
    compareRegs :: X86State MS.Value -> X86State MS.Value -> [String]
    compareRegs real emu =
      catMaybes $ map (viewSome (\reg ->
        let lens = register reg
            realVal = real^.lens
            emuVal = emu^.lens
         in if realVal `equalOrUndef` emuVal
              then Nothing
              else Just $ show reg ++ " did not match.  real:  " ++ show realVal ++ "   emulated: " ++ show emuVal))
         x86StateRegisters
checkAndClear (False, ii) = do
  realRegs <- lift $ lift dumpRegs
  put (Map.empty, realRegs)

-- | Do @waitpid@ on child with given PID and return status.
--
-- The child PID should be greater than zero, to specify a valid
-- child. The @waitpid@ supports other values for the PID, but we do
-- not support them here. See @man waitpid@.
waitForRes :: CPid -> IO Status
waitForRes pid = do
  when (not $ pid > 0) $
    error "waitForRes: non positive PID!"
  res <- waitpid pid []
  case res of
    Just (pid', status) -> do
      when (pid /= pid') $
        error "waitForRes: 'waitpid' returned the wrong child PID!"
      return status
    -- We call @waitpid@ without any flags, in particular without
    -- @WNOHANG@, so we expect it to succeed.
    Nothing -> error "waitForRes: 'waitpid' failed unexpectedly!"

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    Application -> testApplication args
    Test -> testSingleInstruction args
    Instr -> printExecutedInstructions args
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
