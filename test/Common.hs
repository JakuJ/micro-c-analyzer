module Common where

import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.List                 (intercalate)
import           Data.String.Interpolate   (i)
import           MicroC.AST                (Program)
import           MicroC.Interpreter
import           MicroC.Parser             (parseFile)
import           MicroC.ProgramGraph       (PG, toPG)

-- | 'MonadEval' instance for testing.
-- The __read__ operation is implemented by the 'ReaderT' transformer.
-- The __write__ operation is implemented by the 'Writer' monad.
newtype TestEval a = TestEval {unTest :: ReaderT String (Writer String) a}
    deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String)

instance MonadEval TestEval where
    evalRead = asks read
    evalWrite = tell . show

-- | Interpret a program, providing 'input' every time there's a __read__ operation.
--   Return the memory at the end of the program and the output.
runWithInput :: Program -> String -> (ProgramMemory, String)
runWithInput program = runWriter . runReaderT (unTest (evalProgram program))

-- | A list of names of the example programs from the "sources" folder.
programs :: [String]
programs =  [ "arr_range"
            , "dangerEven"
            , "even"
            , "factorial"
            , "faint"
            , "faintEven"
            , "fibonacci"
            , "ifte"
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
