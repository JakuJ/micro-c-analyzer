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

data Args = Benchmark | Analyse AnalysisType AlgorithmType FilePath

parseArgs :: IO Args
parseArgs = execParser $ info (opts <**> helper) (fullDesc <> progDesc "MicroC analysis module.")

opts :: Parser Args
opts = subparser
    ( command "analyse" (info (parseSource <**> helper) (fullDesc <> progDesc "Analyse a MicroC file." ))
    <> command "benchmark" (info (parseBenchmark <**> helper) (fullDesc <> progDesc "Execute benchmarks." ))
    )
parseSource :: Parser Args
parseSource = Analyse <$> parseAnalysis <*> parseAlgorithm <*> parsePath

parsePath :: Parser FilePath
parsePath = argument str $ help "Path to the MicroC source file." <> metavar "path"

parseAnalysis :: Parser AnalysisType
parseAnalysis = argument analysis $ help "The analysis to perform." <> metavar "analysis"

analysis :: ReadM AnalysisType
analysis = str >>= \case
  "dangerous" -> pure DangerousVariables
  "live"      -> pure LiveVariables
  "interval"  -> pure IntervalAnalysis
  "signs"     -> pure DetectionOfSigns
  "faint"     -> pure FaintVariables
  "reaching"  -> pure ReachingDefinitions
  _ -> readerError "Accepted algorithm types are 'dangerous', 'live', 'interval', 'signs', 'faint' and 'reaching'."

parseAlgorithm ::  Parser AlgorithmType
parseAlgorithm = argument algo $ help "The algorithm to use." <> metavar "algorithm"

algo :: ReadM AlgorithmType
algo = str >>= \case
  "naive"        -> pure NaiveAlgorithm
  "chaotic"      -> pure ChaoticWorklist
  "pending-set"  -> pure PendingSetWorklist
  "post-order"   -> pure PostOrderWorklist
  "queue"        -> pure QueueWorklist
  "stack"        -> pure StackWorklist
  _ -> readerError "Accepted algorithm types are 'naive', 'stack', 'queue', 'chaotic', 'post-order' and 'pending-set'."

parseBenchmark :: Parser Args
parseBenchmark = (Benchmark <$) . switch $ long "benchmark" <> short 'b' <> help "Execute benchmarks."


