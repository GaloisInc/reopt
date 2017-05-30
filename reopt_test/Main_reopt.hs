{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -w #-} -- Disable warnings
module Main (main) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import qualified Data.BitVector as BV
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ElfEdit
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Version
import           Data.Word
import           Debug.Trace
import           Numeric (readHex, showHex)
import           System.CPUTime
import           System.Console.CmdArgs.Explicit as CmdArgs
import qualified System.Console.Terminal.Size as Terminal
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitWith, ExitCode(..))
import           System.IO
import           System.Linux.Ptrace.Syscall
import           System.Linux.Ptrace.Types
import           System.Linux.Ptrace.X86_64FPRegs
import           System.Linux.Ptrace.X86_64Regs
import           System.Posix.Process
import           System.Posix.Signals (Signal, sigFPE, sigTRAP, sigSEGV)
import           System.Posix.Types
import           System.Posix.Waitpid as W
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))
import           Text.Printf
import           Text.Read (readMaybe)

import           Paths_reopt (version)

import           Flexdis86 ( InstructionInstance
                           , InstructionInstanceF(..)
                           , ppInstruction
                           , ByteReader(..)
                           , disassembleInstruction
                           , LockPrefix(..)
                           , mmxRegNo
                           , xmmRegNo
                           )
import qualified Flexdis86 as F

import           Data.Macaw.CFG
import           Data.Macaw.Memory
import           Data.Macaw.Memory.ElfLoader
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.Flexdis
import qualified Data.Macaw.X86.Monad as SM
import           Data.Macaw.X86.Semantics
import           Data.Macaw.X86.X86Reg (X86Reg(..), x86StateRegs)

import           Reopt (readElf64)
import           Reopt.Concrete.BitVector hiding (modify)
import           Reopt.Concrete.MachineState (MonadMachineState(..), FoldableMachineState(..)
                                             , Address8, modifyAddr, asBV, ConcreteStateT)
import qualified Reopt.Concrete.MachineState as MS
import           Reopt.Concrete.Semantics

import           SignalUtils (signalToString, statusToString)

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
                 , _reoptTrace  :: !(Maybe Integer)
                 , _reoptFragile  :: !Bool
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

-- | Whether to trace execution (print a '.' for each n instructions)
reoptTrace :: Simple Lens Args (Maybe Integer)
reoptTrace = lens _reoptTrace (\s v -> s { _reoptTrace = v })

-- | Whether to stop when we get an error
reoptFragile :: Simple Lens Args Bool
reoptFragile = lens _reoptFragile (\s v -> s { _reoptFragile = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Application
                   , _programPath = ""
                   , _loadStyle = LoadBySection
                   , _reoptTrace = Nothing
                   , _reoptFragile = False
                   }

------------------------------------------------------------------------
-- Argument processing

arguments :: Mode Args
arguments = mode "reopt_test" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ instrFlag
                , testFlag
                , applicationFlag
                , traceFlag
                , fragileFlag
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


fragileFlag :: CmdArgs.Flag Args
fragileFlag = flagNone [ "fragile", "f"] upd help
  where upd = reoptFragile .~ True
        help = "Stop execution at the first mismatch"

traceFlag :: CmdArgs.Flag Args
traceFlag = flagOpt "100" [ "trace", "T"] upd "N" help
  where upd s old = case readMaybe s of
          Just n -> Right $ (reoptTrace .~ Just n) old
          _      -> Left "Could not parse trace number"
        help = "Trace execution by printing a '.' for each N instructions"

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

mkElfMem :: Monad m => LoadStyle -> Elf Word64 -> m (Memory 64)
mkElfMem LoadBySection e = either fail (return . snd) $ memoryForElfSections Addr64 e
mkElfMem LoadBySegment e = either fail (return . snd) $ memoryForElfSegments Addr64 e

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
data MessageType
   = FailureRecord (Maybe (RegState X86Reg MS.Value)) InstructionInstance [String]
   | UnknownInstruction InstructionInstance
   | NoDecode
   | Impossible String
   | UnexpectedStatus String String
   | SuccessfulExecution (Maybe InstructionInstance)
   | Segfault
   | Info String

instance Show MessageType where
  show (FailureRecord m_regs ii msgs) =
    unlines (["Mismatch after executing instruction " ++ show ii]
             ++ [show (pretty m_regs)]
             ++ map (" - " ++) msgs)
  show (UnknownInstruction ii) = "Unknown instruction: " ++ show ii
  show NoDecode = "Unable to decode instruction"
  show (Impossible msg) = "Impossible: " ++ msg
  show (UnexpectedStatus name status) = name ++ ": unexpected status: " ++ status
  show (SuccessfulExecution ii) = "Instruction " ++ show ii ++ " executed successfully"
  show (Segfault) = "Test executable segfaulted"
  show (Info msg) = "Info: " ++ msg

type TestM m a = ConcreteStateT (StateT [MessageType] m) a

runTestM :: Monad m => TestM m a -> RegState X86Reg MS.Value
            -> m ([MessageType], a, MS.ConcreteMemory, RegState X86Reg MS.Value)
runTestM m regs = do ((a, (mem, regs')), logs) <- runStateT (MS.runConcreteStateT m Map.empty regs) []
                     return (logs, a, mem, regs')

logMessage :: Monad m => MessageType -> TestM m ()
logMessage msg = lift (modify ((:) msg))

-- | Test builder.
mkTest :: Args
  -> (Args -> TestM PTraceMachineState ())
  -> IO ()
mkTest args test = do
  child <- traceFile $ args^.programPath
  procMem <- openChildProcMem child
  procMaps <- openChildProcMaps child
  (out, _, _, _) <-
    runPTraceMachineState (PTraceInfo {cpid = child, memHandle = procMem, mapHandle = procMaps}) $ do
      regs <- dumpRegs
      runTestM (test args) regs
  let isMismatch m = case m of
                       FailureRecord {} -> True
                       _                 -> False
      isSegfault m = case m of
                       Segfault -> True
                       _        -> False
      numMismatches = length (filter isMismatch out)
      numSegfaults  = length (filter isSegfault out)

  mapM_ print (filter isMismatch $ reverse out) -- only print mis-matches

  case (numMismatches, numSegfaults) of
    (0,0) -> putStrLn "No mismatches!"
    (0,_) -> do putStrLn "Segfault was observed."
                exitWith (ExitFailure 2)
    (_,_) -> do putStrLn (show numMismatches ++ " mismatches were found.")
                exitFailure
  --
  where
    openChildProcMem :: CPid -> IO Handle
    openChildProcMem pid = do
      openFile ("/proc/" ++ (show pid) ++ "/mem") ReadWriteMode

    openChildProcMaps :: CPid -> IO Handle
    openChildProcMaps pid = do
      openFile ("/proc/" ++ (show pid) ++ "/maps") ReadMode

------------------------------------------------------------------------

testApplication :: Args -> IO ()
testApplication args = mkTest args fullApplicationTest

testSingleInstruction :: Args -> IO ()
testSingleInstruction args = mkTest args singleInstructionTest

printExecutedInstructions :: Args -> IO ()
printExecutedInstructions args = do
  e <- readElf64 (args^.programPath)
  let Identity mem = mkElfMem (args^.loadStyle) e
  child <- traceFile $ args^.programPath
  runStateT (traceInner (printInstr mem) child) ()
  return ()
  where
    printInstr :: Memory 64 -> CPid -> StateT s IO ()
    printInstr mem pid = do
      regs <- lift $ ptrace_getregs pid
      case regs of
        X86 _ -> fail "X86Regs! only 64 bit is handled"
        X86_64 regs64 -> do
          let Just rip_val = absoluteAddrSegment mem (fromIntegral (rip regs64))
          case readInstruction mem rip_val of
            Left err -> lift $ putStrLn $ "Couldn't disassemble instruction " ++ show err
            Right (ii, nextAddr) -> do
              let addr_word = fromIntegral (addrValue nextAddr)
              lift $ putStrLn $ show $ ppInstruction addr_word ii

------------------------------------------------------------------------
-- Register translation.

-- | Translate PTrace register format to Reopt register format.
translatePtraceRegs :: X86_64Regs -> X86_64FPRegs -> RegState X86Reg MS.Value
translatePtraceRegs ptraceRegs ptraceFPRegs = mkRegState fillReg
  where
    fillReg :: X86Reg tp -> MS.Value tp
    fillReg X86_IP = mkLit64 (rip ptraceRegs)

    fillReg (X86_GP r) = mkLit64 (s ptraceRegs)
            -- Map between flexdis registers and the ptrace struct
      where s = case r of
                  F.RAX -> rax
                  F.RCX -> rcx
                  F.RDX -> rdx
                  F.RBX -> rbx
                  F.RSP -> rsp
                  F.RBP -> rbp
                  F.RSI -> rsi
                  F.RDI -> rdi
                  F.R8  -> r8
                  F.R9  -> r9
                  F.R10 -> r10
                  F.R11 -> r11
                  F.R12 -> r12
                  F.R13 -> r13
                  F.R14 -> r14
                  F.R15 -> r15

    fillReg (X86_FlagReg n) = if testBit (eflags ptraceRegs) n
                              then MS.true
                              else MS.false
    fillReg (X87_StatusReg  n) = MS.Literal $ bitVector knownNat $ BV.extract n n swd'
    fillReg X87_TopReg = MS.Literal $ bitVector knownNat $
                            BV.extract (13 :: Int) 11 swd'
    fillReg (X87_TagReg _) = MS.Undefined $ BVTypeRepr  knownNat
    fillReg (X87_FPUReg r) = mkLit80 $ s ptraceFPRegs
      where s = case mmxRegNo r of
                  0 -> st0
                  1 -> st1
                  2 -> st2
                  3 -> st3
                  4 -> st4
                  5 -> st5
                  6 -> st6
                  7 -> st7
    fillReg (X86_XMMReg i) = mkLit128 $ s ptraceFPRegs
      where s = case xmmRegNo i of
                  0  -> xmm0
                  1  -> xmm1
                  2  -> xmm2
                  3  -> xmm3
                  4  -> xmm4
                  5  -> xmm5
                  6  -> xmm6
                  7  -> xmm7
                  8  -> xmm8
                  9  -> xmm9
                  10 -> xmm10
                  11 -> xmm11
                  12 -> xmm12
                  13 -> xmm13
                  14 -> xmm15
                  15 -> xmm15
                  _ -> error $ "Unexpected XMM reg"

    mkLit16 :: Word64 -> MS.Value (BVType 16)
    mkLit16 = MS.Literal . bitVector knownNat . bitVec 16

    mkLit128 :: (Word64, Word64) -> MS.Value (BVType 128)
    mkLit128 (high, low) = MS.Literal $ bitVector knownNat $ bitVec 128 $
      ((fromIntegral high) :: Integer) * 2^64 + fromIntegral low

    mkLit80 :: (Word16, Word64) -> MS.Value (BVType 80)
    mkLit80 (high, low) = MS.Literal $ bitVector knownNat $ bitVec 80 $
      ((fromIntegral high) :: Integer) * 2^64 + fromIntegral low
    cwd' = bitVec 16 $ cwd ptraceFPRegs
    swd' = bitVec 16 $ swd ptraceFPRegs

mkLit64 :: Word64 -> MS.Value (BVType 64)
mkLit64 = MS.Literal . bitVector knownNat . bitVec 64

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
  getMem (MS.Address width bv) = do
    memH <- asks memHandle
    bs <- liftIO $ readFileOffset memH  (fromIntegral $ nat bv) (fromIntegral (widthVal width) `div` 8)
    return $ MS.Literal $ toBitVector width bs
    where
      readFileOffset :: Handle -> Word64 -> Word64 -> IO B.ByteString
      readFileOffset h addr width = do
        hSeek h AbsoluteSeek $ fromIntegral addr
        B.hGet h $ fromIntegral width

      toBitVector :: NatRepr n -> B.ByteString -> BitVector n
      toBitVector n bs =
        bitVector n $ bitVec (widthVal n) (B.foldl (\acc b -> acc*(2^(8::Integer)) + fromIntegral b) (0::Integer) bs)

  getReg r = do
    regs <- dumpRegs
    return $ regs^.boundValue r
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
        | seg == SM.FS -> return $ mkLit64 (fs_base regs')
        | seg == SM.GS -> return $ mkLit64 (gs_base regs')
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
                     (MS.Address n8 $ bitVector n64 $ bitVec 64 start)
                     (MS.Literal $ bitVector n8 $ bitVec 8 (B.head buf))
                     map)
                   (start + 1)
                   (B.tail buf)

runPTraceMachineState :: PTraceInfo -> PTraceMachineState a -> IO a
runPTraceMachineState info (PTraceMachineState {unPTraceMachineState = m}) = runReaderT m info

------------------------------------------------------------------------
-- Instruction disassembly.
--
-- We need a 'ByteReader' to run 'disassembleInstruction'.

newtype MachineByteReader m a = MachineByteReader (StateT (Address8, Int) (ExceptT String m) a)
                              deriving (MonadError String, MonadState (Address8, Int), Functor, Applicative)

instance MonadTrans MachineByteReader where
  lift m = MachineByteReader (lift (lift m))

instance Monad m => Monad (MachineByteReader m) where
  return  = MachineByteReader . return
  MachineByteReader m >>= f = MachineByteReader $ m >>= \x -> case f x of MachineByteReader m' -> m'
  fail = MachineByteReader . throwError

instance (Functor m, MonadMachineState m) =>
  ByteReader (MachineByteReader m) where
    readByte = do
      (addr, disp) <- get
      put $ (modifyAddr (+ bitVec 64 (1 :: Int)) addr, disp + 1)
      val <- lift $ getMem addr
      case asBV val of Nothing -> fail $ "Could not read byte at " ++ show addr
                       Just bv -> return $ fromIntegral bv

runMachineByteReader :: Monad m => MachineByteReader m a -> Address8 ->  m (Either String (Int, a))
runMachineByteReader (MachineByteReader s) addr =
  runExceptT (runStateT s (addr, 0)) >>= return . (fmap $ \(v, (_, l)) -> (l,v))

-- | Disassemble, from HW, the instruction at the given address.
getInstruction :: MonadMachineState m
  => Address8 -> m (Maybe (Int, InstructionInstance))
getInstruction instrAddr = do
  r <- runMachineByteReader disassembleInstruction instrAddr
  case r of
    Left _e -> return Nothing
    -- Left e -> do r' <- runMachineByteReader (sequence $ replicate 10 readByte) instrAddr
    --              case r' of
    --                Left e'  -> -- fail $ "Really couldn't decode bytes (" ++ e ++ " and " ++ e' ++ ") " ++ show instrAddr

    --                Right (_, bs) -> fail $ "Couldn't decode bytes (" ++ e ++ ") " ++ show instrAddr ++ ": "
    --                                 ++ concat (map (flip showHex " ") bs)
    Right v -> return (Just v)

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
             => TestM m (Either SM.ExceptionClass Bool, Maybe InstructionInstance)
stepConcrete = do
  rip' <- getReg ip_reg
  bv <- case rip' of MS.Literal bv -> return bv
                     MS.Undefined _ -> fail "MS.Undefined rip!"

  let instrAddr = MS.Address knownNat bv
  m_r <- getInstruction instrAddr
  case m_r of
    Nothing -> return (Right False, Nothing)
    Just (w, ii) -> do
      let inst_addr = ValueExpr (MS.Literal (bitVector n64 (fromIntegral $ nat bv + fromIntegral w)))
      case execInstruction inst_addr ii of
        Just s -> do
           let stmts = execSemantics s
           eitherExceptionUnit <- evalStateT (runExceptT $ evalStmts stmts) MapF.empty
           case eitherExceptionUnit of
             Right () -> return (Right True, Just ii)
             Left exception -> do
               logMessage $ Info $ "Got an exception: " ++ (show exception)
               return (Left exception, Just ii)
        Nothing -> do
          logMessage $ UnknownInstruction ii
          return (Right False, Just ii)

reportProgress :: Maybe Integer -> Integer -> IO ()
reportProgress Nothing _              = return ()
reportProgress (Just n) ninstructions
  | (ninstructions + 1) `mod` n /= 0  = return ()
reportProgress (Just n) ninstructions = do
  sz <- Terminal.size
  let ndots = ninstructions `div` n
      countBarWidth = fromIntegral $ length "[XXX.XXS] "
  case sz of
    Nothing  -> return ()
    Just (Terminal.Window { Terminal.width = w }) -> do
      let rowWidth = ndots `mod` (w - countBarWidth - 10) -- 10 for a margin
      if rowWidth == 0
        then
           let (digits, suffix) = makeHumanReadable ninstructions
           in printf "\n[%6.2f%s] ." digits suffix
         else
           printf "."
      hFlush stdout

-- up to 999 tera is probably enough
makeHumanReadable :: Integer -> (Float, String)
makeHumanReadable n
  | n < 1000             = (fromIntegral n, "")
  | n < 1000000          = (fromIntegral n / 1000, "k")
  | n < 1000000000       = (fromIntegral n / 1000000, "M")
  | n < 1000000000000    = (fromIntegral n / 1000000000, "G")
  | otherwise            = (fromIntegral n / 1000000000000, "T")

-- | Run an application in lock step with the concrete semantics.
fullApplicationTest :: Args -> TestM PTraceMachineState ()
fullApplicationTest args = do start <- lift . liftIO $ getCPUTime
                              ninstruction <- fullApplicationTest' args 0 sigTRAP
                              end <- lift . liftIO $ getCPUTime
                              let diff = (fromIntegral (end - start)) / (10^12)
                                  instrsPerSecond = fromIntegral ninstruction / diff
                              lift . liftIO $ printf "Simulated %d instructions in %0.3f seconds (%0.3f instructions/second)\n"
                                                     ninstruction
                                                     diff
                                                     (instrsPerSecond :: Double)

-- | The signal, if not equal to 'sigTRAP', is passed to the
-- traced process on resumption.
fullApplicationTest' :: Args -> Integer -> Signal -> TestM PTraceMachineState Integer
fullApplicationTest' args ninstructions sig = do
  (execSuccess, ii) <-  stepConcrete

  -- Trace that we executed another instruction
  liftIO' $ reportProgress (args ^. reoptTrace) ninstructions

  pid <- {- lift $ -} lift $ asks cpid
  preRegs <- lift dumpRegs
  status <- case iiLockPrefix <$> ii of
    Nothing -> single_step pid
    Just RepPrefix -> step_to_next_inst pid
    Just RepZPrefix -> step_to_next_inst pid
    Just RepNZPrefix -> step_to_next_inst pid
    Just NoLockPrefix -> single_step pid
    Just LockPrefix -> single_step pid
  case status of        W.Exited _ -> return ninstructions
                        W.Stopped sig' -> do
                          canContinue <- checkAndClear (args ^. reoptFragile) (Just preRegs) sig' (execSuccess, ii)
                          if canContinue
                          then fullApplicationTest' args (ninstructions + 1) sig'
                          else do logMessage $ Impossible "Main_reopt.fullApplicationTest: 'checkAndClear' failed!"
                                  return ninstructions

                        W.Signaled sig' -> do
                          str <- liftIO' $ signalToString sig'
                          logMessage $ Info $ "Application terminated by signal: " ++ str
                          return ninstructions
                        _ -> do
                          str <- liftIO' $ statusToString status
                          logMessage $ UnexpectedStatus "Main_reopt.runInParallel" str -- ["Main_reopt.runInParallel: unexpected status: " ++ str]
                          return ninstructions
  where
    -- | Single step a "rep" instruction until the IP changes.
    --
    -- MAYBE TODO: this code fails if a non-TRAP signal arrives. In
    -- particular, it always passes 'Nothing' as the 'Maybe Signal'
    -- argument to 'ptrace_singlestep'.
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
                W.Stopped sig' | sig' == sigTRAP -> do
                  addr' <- get_addr
                  if addr' == addr
                    then go addr
                    else return status
                _ -> do
                  str <- liftIO' $ statusToString status
                  logMessage $ UnexpectedStatus "Main_reopt.runInParallel.step_while_inst" str
                  return status
        get_addr = liftIO' $ do
          X86_64 regs <- ptrace_getregs pid
          return $ rip regs
    single_step pid = liftIO' $ do
      -- If the pending signal is not 'sigTRAP', then we may need to
      -- send it to the child process on resumption. Motivation: for
      -- 'sigFPE' from division by zero, the instruction gets
      -- restarted if the 'sigFPE' is not passed here.
      let maybeSignal = if sig == sigTRAP
                        then Nothing
                        else Just sig
      ptrace_singlestep pid maybeSignal
      waitForRes pid
    liftIO' = {- lift . -} lift . liftIO

-- | Test a single instruction.
--
-- TODO: could we make this a special case of testing a whole
-- application? I.e., can we make this a special case of
-- 'fullApplicationtest'?
singleInstructionTest :: Args -> TestM PTraceMachineState ()
singleInstructionTest args = do
  pid <- {- lift $ -} lift  $ asks cpid
  lift $ {- lift $ -} liftIO $ ptrace_cont pid Nothing
  status <- {- lift $ -} lift $ liftIO $ waitForRes pid
  case status
      of W.Exited _ -> return ()
         W.Stopped sig | sig == sigTRAP -> do
           trapRegs <-  liftIO' $ ptrace_getregs pid
           trapFPRegs <- liftIO' $ ptrace_getfpregs pid
           (modRegs, modFPRegs) <- case (trapRegs, trapFPRegs) of
             (X86_64 regs', X86_64FP fpregs')-> return (regs', fpregs')
             _ -> fail "64-bit only!"
           --lift $ liftIO $ ptrace_setregs pid $ X86_64 modRegs
           put (Map.empty, translatePtraceRegs modRegs modFPRegs)
           (execSuccess, ii) <- trace "instTest stepping concrete semantics" stepConcrete
           liftIO' $ ptrace_singlestep pid Nothing
           status' <- liftIO' $ waitForRes pid
           case status'
               of W.Exited _ -> return ()
                  W.Stopped sig' -> do
                    canContinue <- checkAndClear (args ^. reoptFragile) Nothing sig' (execSuccess, ii)
                    unless canContinue $
                      logMessage $ Impossible "Main_reopt.singleInsructionTest: 'checkAndClear' failed!"
                  _ -> do str <- liftIO' $ statusToString status'
                          logMessage $ UnexpectedStatus "Main_reopt.instTest" str
         _ -> do
           logMessage $ Impossible $ "child stopped: " ++ show status
  where
    liftIO' = {- lift . -} lift . liftIO


-- | Check that emulated and real states agree and then clear state.
--
-- Any disagreements in the check, and failed instructions, are
-- reported.
--
-- Return value indicates whether the enclosing test should continue
-- or not (would like to simply raise an error, but then all the
-- 'tell' messages get lost, which makes debugging harder).
checkAndClear :: Bool -> Maybe (RegState X86Reg MS.Value) -> Signal
  -> (Either SM.ExceptionClass Bool, Maybe InstructionInstance)
  -> TestM PTraceMachineState Bool
checkAndClear fragile m_regs sig (eitherExceptionBool, ii) = do
  (canContinue, disagreements) <- check eitherExceptionBool
  sawError <- report ii disagreements
  clear
  return (canContinue && not (sawError && fragile))
  where
    check (Left exception) = checkException exception
    check (Right bool) = checkBool bool

    -- | Check that exception from concrete semantics agrees with signal.
    --
    -- As we are able to handle more exceptions and signals we will
    -- add more cases here. We will also want to distinguish between
    -- non-matching and unimplemented combos.
    checkException SM.DivideError | sig == sigFPE = return (True, [])
    checkException exception = do
      sig' <- liftIO' $ signalToString sig
      let msg = "Exception/signal did not match or are unimplemented! Exception: " ++
                 show exception ++ ", signal: " ++ sig'
      return (False, [msg])

    -- | Check that concrete-semantics-simulator state agrees with HW state.
    --
    -- The bool arg is false for instructions that not have concrete
    -- semantics.
    checkBool False | sig == sigTRAP = do
      rip' <- getReg ip_reg
      return (True, ["No concrete semantics for instruction at " ++ show rip'])
    checkBool True | sig == sigTRAP = do
      realRegs <- {- lift $ -} lift dumpRegs
      emuRegs <- dumpRegs
      let regCmp = compareRegs realRegs emuRegs
      memCmp <- compareMems
      return (True, regCmp ++ memCmp)
      where
        compareMems = {- lift $ -} foldMem8 (\addr val errs -> do
          realVal <- {- lift $ -} getMem addr
          if realVal `MS.equalOrUndef` val
            then return errs
            else return $ errs ++ [show addr ++ " did not match"]) []
        compareRegs :: RegState X86Reg MS.Value -> RegState X86Reg MS.Value -> [String]
        compareRegs real emu =
          catMaybes $ map (viewSome (\reg ->
            let lens = boundValue reg
                realVal = real^.lens
                emuVal = emu^.lens
             in if realVal `MS.equalOrUndef` emuVal
                  then Nothing
                  else Just $ show reg ++ " did not match.  real:  " ++ show realVal ++ "   emulated: " ++ show emuVal))
                          x86StateRegs
    checkBool True | sig == sigSEGV = do
      logMessage Segfault
      return (False, [])
    checkBool _ = do
      sig' <- liftIO' $ signalToString sig
      -- XXX: this should be an error? But we still want to see the
      -- tells.
      return (False, ["checkAndClear.checkBool: unexpected signal: " ++ sig'])

    -- | Clear the simulator state.
    clear = do
      realRegs <- {- lift $ -} lift dumpRegs
      put (Map.empty, realRegs)

    report ii [] = return False -- tell [SuccessfulExecution ii]
    report Nothing    _ = do logMessage NoDecode
                             return False -- keep going even if we can't decode
    report (Just ii) ls = do logMessage $ FailureRecord m_regs ii ls
                             return True

    -- TODO(conathan): make 'WriterT ...' an instance of 'MonadIO'?
    liftIO' :: MonadIO m => IO a -> TestM m a
    liftIO' = {- lift . -} lift . liftIO

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
      {-
      -- DEBUG
      s <- statusToString status
      hPutStrLn stderr $ "waitForRes: status = " ++ s
      -}
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
