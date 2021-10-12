module Language.MicroC.ID
( ID(..)
, lval2ID
, def2IDs
) where

import           Language.MicroC.AST hiding (LValue (Variable))
import qualified Language.MicroC.AST as AST

-- | Result type for multiple analyses.
data ID
  = Variable Identifier
  -- ^ Name of a variable.
  | Array Identifier
  -- ^ Name of an array, we amalgamate those.
  | RecordField Identifier Identifier
  -- ^ Name of a record and its field.
    deriving (Eq, Ord)

instance Show ID where
  show (Variable i)      = i
  show (Array i)         = i
  show (RecordField r f) = r <> "." <> f

lval2ID :: AST.LValue a -> ID
lval2ID (AST.Variable x)  = Variable x
lval2ID (ArrayIndex n _)  = Array n
lval2ID (FieldAccess r f) = RecordField r f

def2IDs :: Declaration -> [ID]
def2IDs = \case
  VariableDecl name   -> [Variable name]
  ArrayDecl _ name    -> [Array name]
  RecordDecl r fields -> map (RecordField r) fields
