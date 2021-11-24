module Common where

import           Control.Monad           (forM)
import           Data.List               (intercalate)
import           Data.String.Interpolate (i)
import           MicroC.Parser           (parseFile)
import           MicroC.ProgramGraph     (PG, toPG)

-- | A list of names of the example programs from the "sources" folder.
programs :: [String]
programs =  [ "arr_range"
            , "dangerEven"
            , "dfs"
            , "division"
            , "even"
            , "factorial"
            , "faint"
            , "faintEven"
            , "fibonacci"
            , "ifte"
            , "insertion_sort"
            , "intervals"
            , "monte_carlo"
            , "pos_avg"
            , "precedence"
            , "records"
            , "sum_arr"
            ]

-- | Returns a list of program graphs of the example programs from the "sources" folder.
programGraphs :: IO [PG]
programGraphs = do
  asts <- mapM (\p -> parseFile [i|sources/#{p}.c|]) programs
  forM asts $ \case
    Right a   -> pure (toPG a)
    Left errs -> fail (intercalate "\n" errs)
