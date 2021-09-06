module Language.MicroC.Parser where

import qualified Data.Text           as T
import           Data.Void
import           Text.Megaparsec

import           Language.MicroC.AST

type Parser = Parsec Void T.Text

parseProgram :: Parser Program
parseProgram = undefined
