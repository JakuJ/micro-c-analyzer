module Language.MicroC.Parser (
    parseMicroC
) where

import qualified Data.Text           as T
import           Data.Void           (Void)
import           Language.MicroC.AST (Program)
import           Text.Megaparsec

type Parser = Parsec Void T.Text

parseProgram :: Parser Program
parseProgram = undefined

-- | Parse MicroC source code into a 'Program' structure or fail with a 'ParseErrorBundle'.
parseMicroC :: T.Text -> Either (ParseErrorBundle T.Text Void) Program
parseMicroC = runParser parseProgram ""
