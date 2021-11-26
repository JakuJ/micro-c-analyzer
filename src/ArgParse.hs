module ArgParse
( Args(..)
, AnalysisType(..)
, AlgorithmType(..)
, parseArgs
) where

import           Options.Applicative

data AnalysisType
  = FaintVariables
  | DangerousVariables
  | DetectionOfSigns
  | IntervalAnalysis
  | LiveVariables
  | ReachingDefinitions

data AlgorithmType
  = NaiveAlgorithm
  | ChaoticWorklist
  | QueueWorklist
  | StackWorklist
  | PostOrderWorklist
  | PendingSetWorklist
    deriving (Eq)

data Args = Benchmark | Analyse AnalysisType AlgorithmType FilePath

parseArgs :: IO Args
parseArgs = customExecParser (prefs showHelpOnEmpty) $ opts `withInfo` "Program analysis module for the MicroC language"

opts :: Parser Args
opts = subparser $
  command "analyse" (parseAnalysisArgs `withInfo` "Run an analysis on a MicroC source file") <>
  command "benchmark" (pure Benchmark `withInfo` "Execute benchmarks")

parseAnalysisArgs :: Parser Args
parseAnalysisArgs = Analyse <$> parseAnalysis <*> parseAlgorithm <*> parsePath

parsePath :: Parser FilePath
parsePath = argument str $ help "Path to the MicroC source file" <> metavar "PATH"

-- ANALYSIS

parseAnalysis :: Parser AnalysisType
parseAnalysis = argument analysis $ help analysisHelp <> metavar "ANALYSIS"

analysis :: ReadM AnalysisType
analysis = str >>= \case
  "dangerous" -> pure DangerousVariables
  "faint"     -> pure FaintVariables
  "interval"  -> pure IntervalAnalysis
  "live"      -> pure LiveVariables
  "reaching"  -> pure ReachingDefinitions
  "signs"     -> pure DetectionOfSigns
  _ -> readerError "Accepted analyses: dangerous, faint, interval, live, reaching, signs"

analysisHelp :: String
analysisHelp = "One of: dangerous, faint, interval, live, reaching, signs"

-- ALGORITHM

parseAlgorithm ::  Parser AlgorithmType
parseAlgorithm = argument algo $ help algoHelp <> metavar "ALGORITHM"

algo :: ReadM AlgorithmType
algo = str >>= \case
  "chaotic"      -> pure ChaoticWorklist
  "naive"        -> pure NaiveAlgorithm
  "pending-set"  -> pure PendingSetWorklist
  "post-order"   -> pure PostOrderWorklist
  "queue"        -> pure QueueWorklist
  "stack"        -> pure StackWorklist
  _ -> readerError "Accepted algorithms: chaotic, naive, pending-set, post-order, queue, stack"

algoHelp :: String
algoHelp = "One of: chaotic, naive, pending-set, post-order, queue, stack"

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = info (helper <*> p) $ progDesc desc
