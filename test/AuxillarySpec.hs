module AuxillarySpec (spec) where

import           Data.Set                            (fromList)
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID                           (ID (..))
import           MicroC.Parser                       (parseProgram)
import           MicroC.ProgramGraph                 (toPG)
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    it "getAllNames" $ do
      let source = "int x; {int fst; int snd; int trd} R; {int real; int imag} C; int[32] arr; int[5] A2; y := C.real; R := (A2[3], x + z, 2); if (k <= f && k != f) {}"
          Right prog = parseProgram source
          pg = toPG prog

      getAllNames pg `shouldBe` fromList  [ VariableID "x"
                                          , FieldID "R" "fst"
                                          , FieldID "R" "snd"
                                          , FieldID "R" "trd"
                                          , FieldID "C" "real"
                                          , FieldID "C" "imag"
                                          , VariableID "y"
                                          , VariableID "z"
                                          , VariableID "k"
                                          , VariableID "f"
                                          , ArrayID "A2"
                                          , ArrayID "arr"]
