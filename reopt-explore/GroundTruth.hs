module GroundTruth where

import CommandLine (GroundTruthOptions (..), Options)
import Common (createLogger, findAllElfFilesInDirs)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (newIORef)
import Data.Macaw.Discovery.State
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (viewSome)
import Reopt (LoadOptions (..), ReoptOptions, emptyAnnDeclarations, parseElfHeaderInfo64, recoverX86Elf, runReoptM)
import Reopt.Events (initReoptSummary)
import Reopt.Utils.Exit (checkedReadFile, handleEitherStringWithExit, handleEitherWithExit)
import System.IO (hPutStrLn, stderr)

import Control.Lens
import Control.Lens.Internal.CTypes (Word64)
import Control.Monad (zipWithM_)
import Data.Macaw.CFG
import Data.Maybe (fromMaybe)
import Data.ProtoLens
import GroundTruth.Blocks as B
import GroundTruth.Blocks_Fields as B
import System.Directory (createDirectoryIfMissing)
import System.FilePath

exploreBinary :: ReoptOptions -> Int -> (Int, FilePath) -> IO B.Module
exploreBinary opts totalCount (idx, fPath) = do
    hPutStrLn stderr
        $ "["
        ++ show idx
        ++ " of "
        ++ show totalCount
        ++ "] Recovering protobuf of "
        ++ fPath
        ++ " ..."

    bs <- checkedReadFile fPath

    summaryRef <- newIORef $ initReoptSummary fPath
    statsRef <- newIORef mempty
    logger <- createLogger opts summaryRef statsRef

    let annDecl = emptyAnnDeclarations

    hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs

    -- discoveryState: Macaw IR
    -- recoveredModule: Reopt IP (aka "FnRep")
    --
    -- We only use Macaw IR for this analysis. Could we be missing some data from Reopt?
    (_os, discoveryState, _recovOut, _recoveredModule, _constraints) <-
        -- (os, _, recMod, constraints, _, logEvents) <-
        handleEitherWithExit
            =<< runReoptM logger (recoverX86Elf lOpts opts annDecl unnamedFunPrefix hdrInfo)

    -- Encode results in protobuf schema
    let functions = map (viewSome encodeFunction) (exploredFunctions discoveryState)
    return
        ( defMessage
            & B.fuc .~ functions
        )
  where
    lOpts = LoadOptions{loadOffset = Nothing}
    unnamedFunPrefix = BSC.pack "reopt"

encodeFunction :: ArchConstraints arch => DiscoveryFunInfo arch ids -> B.Function
encodeFunction function =
    defMessage
        & B.va .~ segmentOffsetAsInt (discoveredFunAddr function)
        & B.bb .~ map encodeBlock bbs
  where
    bbs = Map.elems $ function ^. parsedBlocks

encodeBlock :: ArchConstraints arch => ParsedBlock arch ids -> B.BasicBlock
encodeBlock block =
    defMessage
        & B.va .~ blockAddress
        & B.instructions .~ encodeInstruction block blockAddress
        & B.child .~ encodeChild (pblockTermStmt block)
  where
    blockAddress = segmentOffsetAsInt (pblockAddr block) -- Use 0 for invalid address for now

encodeChild :: ArchConstraints arch => ParsedTermStmt arch ids -> [B.Child]
encodeChild term =
    map
        ( \childAddr ->
            defMessage
                & B.va .~ segmentOffsetAsInt childAddr
        )
        (parsedTermSucc term)

-- Note: Instruction addresses in `InstructionStart` are relative to the basic block
--       We add `bbStart` as the offset to get their full address
encodeInstruction :: ArchConstraints arch => ParsedBlock arch ids -> Word64 -> [B.Instruction]
encodeInstruction block bbStart =
    map
        ( \ins ->
            defMessage
                & B.va .~ (bbStart + ins)
        )
        (instructionsIn block) -- Use Lenses traversal?
  where
    -- Note that assembly instruction can consist of multiple Macaw instructions
    -- Therefore, we use `InstructionStart` to determine addresses of unique assembly instruction
    instructionsIn b = [memWordValue addr | (InstructionStart addr _asm) <- pblockStmts b]

runGroundTruth :: Options -> GroundTruthOptions -> ReoptOptions -> IO ()
runGroundTruth _opts gtopts ropts = do
    elfFiles <- findAllElfFilesInDirs (gtPaths gtopts)
    results <- mapM (exploreBinary ropts (length elfFiles)) elfFiles

    -- Pretty print results
    let messages = map showMessage results
    mapM_ (hPutStrLn stderr) messages

    let outputDir = fromMaybe "ReoptExplorePB" $ gtExportDir gtopts
    createDirectoryIfMissing False outputDir

    let protoBuffs = map encodeMessage results
    let outputFiles = map (prepFileName outputDir . snd) elfFiles

    -- Write protobuf results into files
    zipWithM_ BSC.writeFile outputFiles protoBuffs
  where
    prepFileName dir f = dir </> replaceExtension (takeFileName f) "pb"

-- Address conversion helpers
--

-- 0 if memory is a relocation. Offset otherwise
segmentOffsetAsInt :: MemWidth w => MemSegmentOff w -> Word64
segmentOffsetAsInt mem = maybe 0 memWordValue (segoffAsAbsoluteAddr mem)
