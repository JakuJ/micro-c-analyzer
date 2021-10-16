module InterpreterSpec (spec) where

import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           MicroC.Interpreter
import           MicroC.Parser             (parseFile)
import           Test.Hspec

-- | 'MonadEval' instance for testing.
-- The __read__ operation is implemented by the 'ReaderT' transformer.
-- The __write__ operation is implemented by the 'Writer' monad.
newtype TestEval a = TestEval {unTest :: ReaderT String (Writer String) a}
    deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String)

instance MonadEval TestEval where
    evalRead = asks read
    evalWrite = tell . show

spec :: Spec
spec = describe "AST interpreter" $ do
    it "Interprets example program" $ do
        Right naiveEven <- liftIO $ parseFile "sources/even.c"
        forM_ [-10 .. 10 :: Int] $ \i -> do
            let finalVars = evalProgram naiveEven
                finalVars' = execWriter (runReaderT (unTest finalVars) (show i))
            finalVars' `shouldBe` if even i then "1" else "0"
