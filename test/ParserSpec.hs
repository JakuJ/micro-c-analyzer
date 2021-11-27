module ParserSpec (spec) where

import           Common                  (programs)
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (isLeft, isRight)
import           Data.String.Interpolate (i)
import           MicroC.Parser           (parseFile, parseProgram)
import           Test.Hspec

spec :: Spec
spec = do
  it "undefined record" $ parseProgram "read R.fst;" `shouldSatisfy` isLeft
  it "undefined field" $ parseProgram "{int fst; int snd} R; read R.aaa;" `shouldSatisfy` isLeft
  it "too many fields" $ parseProgram "{int fst; int snd} R; R := (1, 2, 3);" `shouldSatisfy` isLeft
  it "too few fields" $ parseProgram "{int fst; int snd; int trd} R; R := (1, 2);" `shouldSatisfy` isLeft
  it "undefined array" $ parseProgram "int i; i[0] := 2;" `shouldSatisfy` isLeft
  it "invalid assignment 1" $ parseProgram "{int fst; int snd} R; while (1 != 2) {i := R;}" `shouldSatisfy` isLeft
  it "invalid assignment 2" $ parseProgram "int [3] R; int i; while (1 != 2) {i := R;}" `shouldSatisfy` isLeft
  it "invalid usage 1" $ parseProgram "int [3] R; if (false) {k :+ 2 + R;}" `shouldSatisfy` isLeft
  it "invalid usage 2" $ parseProgram "{int a; int b} R; if (true) {k :+ 2 + R;}" `shouldSatisfy` isLeft
  describe "Example programs" $
    forM_ programs $ \program -> do
      it (program <> ".c") $ do
        result <- liftIO $ parseFile [i|sources/#{program}.c|]
        result `shouldSatisfy` isRight
