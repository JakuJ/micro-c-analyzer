module InterpreterSpec (spec) where

import           Common                 (runWithInput)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           MicroC.Parser          (parseFile)
import           Test.Hspec


spec :: Spec
spec = describe "AST interpreter" $ do
    it "Interprets example program" $ do
        Right naiveEven <- liftIO $ parseFile "sources/even.c"
        forM_ [-10 .. 10 :: Int] $ \i -> do
            let output = snd $ runWithInput naiveEven (show i)
            output `shouldBe` if even i then "1" else "0"
