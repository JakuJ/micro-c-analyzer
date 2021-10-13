module ParserSpec (spec) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Either            (isRight)
import           MicroC.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Parses example programs" $ do
  forM_ programs $ \program -> do
    it (program <> ".c") $ do
      result <- liftIO $ parseProgram $ "sources/" <> program <> ".c"
      result `shouldSatisfy` isRight

programs :: [String]
programs = ["danger", "even", "factorial", "faint", "fibonacci", "ifte", "intervals", "precedence", "records"]
