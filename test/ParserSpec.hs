module ParserSpec (spec) where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (isRight)
import           Data.String.Interpolate (i)
import           MicroC.Parser           (parseFile)
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Parses example programs" $ do
  forM_ programs $ \program -> do
    it (program <> ".c") $ do
      result <- liftIO $ parseFile [i|sources/#{program}.c|]
      result `shouldSatisfy` isRight

programs :: [String]
programs = ["danger", "even", "factorial", "faint", "fibonacci", "ifte", "intervals", "precedence", "records"]
