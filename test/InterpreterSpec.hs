module InterpreterSpec (
    spec
) where

import           Control.Monad.State
import qualified Data.Map.Lazy               as M
import           Language.MicroC.AST
import           Language.MicroC.Interpreter
import           Test.Hspec

spec :: Spec
spec = describe "AST interpreter" $ do
    it "Interprets example program" $ do
        forM_ [-10 .. 10] $ \i -> do
            let initialState = M.singleton "x" i
                finalVars = evalProgram naiveEven initialState
            finalVars M.! "even" `shouldBe` if even i then 1 else 0

naiveEven :: Program
naiveEven = Program decls stats
  where
    decls :: Declarations
    decls = [ VariableDecl "x"
            , VariableDecl "y"
            , VariableDecl "even"
            ]
    stats :: Statements
    stats = [ Assignment (Variable "y") (Literal 0)
            , Assignment (Variable "even") (Literal 1)
            , IfThen (OpR (Reference (Variable "x")) Lt (Literal 0))
                [Assignment (Variable "x") (OpA (Reference (Variable "x")) Mult (Literal (-1)))]
            , While (OpR (Reference (Variable "x")) Neq (Reference (Variable "y")))
                [ Assignment (Variable "y") (OpA (Reference (Variable "y")) Add (Literal 1))
                , Assignment (Variable "even") (OpA (Literal 1) Sub (Reference (Variable "even")))
                ]
            ]
