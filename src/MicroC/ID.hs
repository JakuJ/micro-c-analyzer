module MicroC.ID
( ID(..)
, lval2ID
, def2IDs
) where

import           MicroC.AST

-- | Result type for multiple analyses.
data ID
  = VariableID Identifier
  -- ^ Name of a variable.
  | ArrayID Identifier
  -- ^ Name of an array, we amalgamate those.
  | FieldID Identifier Identifier
  -- ^ Name of a record and its field.
    deriving (Eq, Ord)

instance Show ID where
  show (VariableID i) = i
  show (ArrayID i)    = i
  show (FieldID r f)  = r <> "." <> f

lval2ID :: LValue a -> ID
lval2ID (Variable x)      = VariableID x
lval2ID (ArrayIndex n _)  = ArrayID n
lval2ID (FieldAccess r f) = FieldID r f

def2IDs :: Declaration -> [ID]
def2IDs = \case
  VariableDecl name   -> [VariableID name]
  ArrayDecl _ name    -> [ArrayID name]
  RecordDecl r fields -> map (FieldID r) fields
