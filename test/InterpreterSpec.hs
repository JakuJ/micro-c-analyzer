module InterpreterSpec (
    spec
) where

import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Language.MicroC.AST
import           Language.MicroC.Interpreter
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
        forM_ [-10 .. 10 :: Int] $ \i -> do
            let finalVars = evalProgram naiveEven
                finalVars' = execWriter (runReaderT (unTest finalVars) (show i))
            finalVars' `shouldBe` if even i then "1" else "0"

naiveEven :: Program
naiveEven = Program decls stats
  where
    decls :: Declarations
    decls = [ VariableDecl "x"
            , VariableDecl "y"
            , VariableDecl "even"
            ]
    stats :: Statements
    stats = [ Read (Variable "x")
            , Assignment (Variable "even") (Literal 1)
            , IfThen (OpR (Reference (Variable "x")) Lt (Literal 0))
                [Assignment (Variable "x") (OpA (Reference (Variable "x")) Mult (Literal (-1)))]
            , While (OpR (Reference (Variable "x")) Neq (Reference (Variable "y")))
                [ Assignment (Variable "y") (OpA (Reference (Variable "y")) Add (Literal 1))
                , Assignment (Variable "even") (OpA (Literal 1) Sub (Reference (Variable "even")))
                ]
            , Write (Reference (Variable "even"))
            ]
