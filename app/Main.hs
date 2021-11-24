{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           ArgParse
import           Benchmark   (benchmark)
import           MicroC
import           RunAnalysis (analyseFile)

main :: IO ()
main = parseArgs >>= \case
  Benchmark             -> benchmark
  Analyse an algo path  -> case an of
    FaintVariables      -> analyse @FV
    DangerousVariables  -> analyse @DV
    DetectionOfSigns    -> analyse @DS
    IntervalAnalysis    -> analyse @IA
    LiveVariables       -> analyse @LV
    ReachingDefinitions -> analyse @RD
    where
      analyse :: forall m. (Analysis m, Show (Result m)) => IO ()
      analyse = analyseFile @m algo path
