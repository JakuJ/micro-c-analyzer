module Main where

import           ArgParse    (parseArgs)
import           RunAnalysis (runAnalysis)

main :: IO ()
main = parseArgs >>= runAnalysis
