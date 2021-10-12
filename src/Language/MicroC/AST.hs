{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A module containing datatypes defining the structure of the MicroC AST.
module Language.MicroC.AST
( -- * Type system
  CType(..)
, TypeRepr
, Identifier
  -- * Top-level constructs
, Declaration(..)
, Declarations
, Statement(..)
, Statements
, Program(..)
  -- * Expressions
  -- $expressions
, RValue(..)
, LValue(..)
  -- ** Operators
, OpArith(..)
, OpRel(..)
, OpBool(..)
 -- * Miscellaneous
, _RecordDecl
) where

import           Control.Lens (makePrisms)
import           Data.Data
import           GHC.Generics (Generic)

-- | Represents a type in the MicroC language.
data CType
  -- | The __int__ type.
  = CInt
  -- | The type of Boolean expressions.
  | CBool

-- | An injective mapping from MicroC types to the Haskell types used to represent their runtime values.
type family TypeRepr (t :: CType) where
  TypeRepr 'CInt = Int
  TypeRepr 'CBool = Bool

-- | A type alias for identifiers. Leaves room to change 'String' to 'Text' later.
type Identifier = String

-- | Represents a single declaration.
data Declaration
  -- | Declaration of a variable with a given name.
  = VariableDecl Identifier
  -- | Declaration of an array with a given size and name.
  | ArrayDecl Int Identifier
  -- | Declaration of a record with a given name.
  | RecordDecl Identifier [Identifier]
    deriving (Eq, Show, Data, Generic)

makePrisms ''Declaration

-- | A type alias for declarations. Leaves room to change the list to a recursive datatype if need be.
type Declarations = [Declaration]

-- $expressions
-- The following datatypes represent expressions - evaluable constructs which yield a value of a given 'CType'.

-- | An R-value is a value that can only be on the right side of an assignment.
data RValue (t :: CType) where
  -- | A reference to an L-value.
  Reference :: LValue 'CInt -> RValue 'CInt
  -- | A reference to a literal value.
  Literal :: TypeRepr t -> RValue t
  -- | An application of a binary arithmetic operator.
  OpA :: RValue 'CInt -> OpArith -> RValue 'CInt -> RValue 'CInt
  -- | An application of a relational operator.
  OpR :: RValue 'CInt -> OpRel -> RValue 'CInt -> RValue 'CBool
  -- | An application of a binary boolean operator.
  OpB :: RValue 'CBool -> OpBool -> RValue 'CBool -> RValue 'CBool
  -- | Boolean negation.
  Not :: RValue 'CBool -> RValue 'CBool

deriving instance Show (TypeRepr t) => Show (RValue t)
deriving instance Eq (TypeRepr t) => Eq (RValue t)
deriving instance Ord (TypeRepr t) => Ord (RValue t)

-- | An L-value is a value that can only be on the left side of an assignment.
-- To refer to an 'LValue' on the right side of the 'Assignment', use the 'Reference' constructor.
data LValue (t :: CType) where
  -- | Used when assigning to a variable.
  Variable :: Identifier -> LValue 'CInt
  -- | Used when assigning to an array at a given index.
  ArrayIndex :: Identifier -> RValue 'CInt -> LValue 'CInt
  -- | Used when assigning to a field in a record.
  FieldAccess :: Identifier -> Identifier -> LValue 'CInt

deriving instance Show (LValue t)
deriving instance Eq (LValue t)
deriving instance Ord (LValue t)
deriving instance Data (LValue 'CInt)

-- | Arithmetic operators, including bitwise operations.
data OpArith = Add | Sub | Mult | Div | Mod | BitAnd | BitOr
  deriving (Show, Eq, Ord, Data, Generic)

-- | Relational operators.
data OpRel = Lt | Gt | Le | Ge | Eq | Neq
  deriving (Show, Eq, Ord, Data, Generic)

-- | Boolean operators.
data OpBool = And | Or
  deriving (Show, Eq, Ord, Data, Generic)

-- | A statement is a top-level construct that does not evaluate to a value,
-- but otherwise advances the control flow of a program.
data Statement
  -- | An assignment of an R-value to an L-value.
  = Assignment (LValue 'CInt) (RValue 'CInt)
  -- | An assignment of two R-values to consecutive fields in a record.
  | RecordAssignment Identifier [RValue 'CInt]
  -- | An __if-then__ statement without the __else__ clause.
  | IfThen (RValue 'CBool) Statements
  -- | An __if-then-else__ statement.
  | IfThenElse (RValue 'CBool) Statements Statements
  -- | A __while__ statement.
  | While (RValue 'CBool) Statements
  -- | A __read__ statement.
  | Read (LValue 'CInt)
  -- | A __write__ statement.
  | Write (RValue 'CInt)
    deriving (Show, Eq, Ord, Generic)

-- | A type alias for statements. Leaves room to change the list to a recursive datatype if need be.
type Statements = [Statement]

-- | Represents a complete MicroC program.
data Program = Program Declarations Statements
  deriving (Show)

-- MISC. INSTANCES

instance Data (RValue 'CInt) where
  gunfold _ _ _ = undefined
  toConstr = undefined
  dataTypeOf = undefined

instance Data (RValue 'CBool) where
  gunfold _ _ _ = undefined
  toConstr = undefined
  dataTypeOf = undefined
