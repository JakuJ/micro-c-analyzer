{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}

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
) where

-- | Represents a type in the MicroC language.
data CType
  -- | The __int__ type.
  = CInt
  -- | The type of Boolean expressions.
  | CBool
    deriving (Show)

-- | An injective mapping from MicroC types to the Haskell types used to represent their runtime values.
type family TypeRepr (t :: CType) = q | q -> t where
  TypeRepr 'CInt = Int
  TypeRepr 'CBool = Bool

-- | A type alias for identifiers. Leaves room to change 'String' to 'Text' later.
type Identifier = String

-- | Represents a single declaration.
data Declaration where
  -- | Declaration of a variable with a given name.
  VariableDecl :: Identifier -> Declaration
  -- | Declaration of an array with a given size and name.
  ArrayDecl :: Int -> Identifier -> Declaration
  -- | Declaration of a record with a given name.
  RecordDecl :: Identifier -> [Identifier] -> Declaration
    deriving (Show)

-- | A type alias for declarations. Leaves room to change the list to a recursive datatype if need be.
type Declarations = [Declaration]

-- $expressions
-- The following datatypes represent expressions - evaluable constructs which yield a value of a given 'CType'.

-- | An R-value is a value that can only be on the right side of an assignment.
data RValue (t :: CType) where
  -- | A reference to an L-value.
  Reference :: LValue 'CInt -> RValue 'CInt
  -- | A reference to a literal value.
  Literal :: Show (TypeRepr t) => TypeRepr t -> RValue t
  -- | An application of a binary arithmetic operator.
  OpA :: RValue 'CInt -> OpArith -> RValue 'CInt -> RValue 'CInt
  -- | An application of a relational operator.
  OpR :: RValue 'CInt -> OpRel -> RValue 'CInt -> RValue 'CBool
  -- | An application of a binary boolean operator.
  OpB :: RValue 'CBool -> OpBool -> RValue 'CBool -> RValue 'CBool
  -- | Boolean negation.
  Not :: RValue 'CBool -> RValue 'CBool

deriving instance Show (RValue t)
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

deriving instance Show (LValue (t :: CType))
deriving instance Eq (LValue t)
deriving instance Ord (LValue t)

-- | Arithmetic operators, including bitwise operations.
data OpArith = Add | Sub | Mult | Div | Mod | BitAnd | BitOr
  deriving (Show, Eq, Ord)

-- | Relational operators.
data OpRel = Lt | Gt | Le | Ge | Eq | Neq
  deriving (Show, Eq, Ord)

-- | Boolean operators.
data OpBool = And | Or
  deriving (Show, Eq, Ord)

-- | A statement is a top-level construct that does not evaluate to a value,
-- but otherwise advances the control flow of a program.
data Statement where
  -- | An assignment of an R-value to an L-value.
  Assignment :: LValue 'CInt -> RValue 'CInt -> Statement
  -- | An assignment of two R-values to consecutive fields in a record.
  RecordAssignment :: Identifier -> [RValue 'CInt] -> Statement
  -- | An __if-then__ statement without the __else__ clause.
  IfThen :: RValue 'CBool -> Statements -> Statement
  -- | An __if-then-else__ statement.
  IfThenElse :: RValue 'CBool -> Statements -> Statements -> Statement
  -- | A __while__ statement.
  While :: RValue 'CBool -> Statements -> Statement
  -- | A __read__ statement.
  Read :: LValue 'CInt -> Statement
  -- | A __write__ statement.
  Write :: RValue 'CInt -> Statement

deriving instance Show Statement
deriving instance Eq Statement
deriving instance Ord Statement

-- | A type alias for statements. Leaves room to change the list to a recursive datatype if need be.
type Statements = [Statement]

-- | Represents a complete MicroC program.
data Program = Program Declarations Statements
  deriving (Show)
