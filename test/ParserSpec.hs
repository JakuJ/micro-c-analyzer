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
  describe "Example programs" $
    forM_ programs $ \program -> do
      it (program <> ".c") $ do
        result <- liftIO $ parseFile [i|sources/#{program}.c|]
        result `shouldSatisfy` isRight

