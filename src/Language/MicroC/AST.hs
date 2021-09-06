{-# LANGUAGE DuplicateRecordFields #-}

module Language.MicroC.AST where

-- Type system

data Type = IntT | BoolT

-- Declarations

type Identifier = String

data RecordField = RecordField {_type :: Type, _name :: Identifier}

data Declaration
  = VariableDecl {_type :: Type, _name :: Identifier}
  | ArrayDecl {_type :: Type, _size :: Int, _name :: Identifier}
  | RecordDecl {_fields :: [RecordField]}

type Declarations = [Declaration]

-- Expressions

data RValue
  = Reference {_ref :: LValue}
  | Literal {_value :: String}
  | OperatorApp {_lhs :: RValue, _rhs :: RValue, _op :: OpA}

data LValue
  = Variable {_name :: Identifier}
  | ArrayIndex {_name :: Identifier, _index :: RValue}
  | FieldAccess {_recordName :: Identifier, _fieldName :: Identifier}

data BValue
  = Constant Bool
  | Not BValue
  | OpR RValue OpR RValue
  | OpB BValue OpB BValue

data OpA = Plus | Minus | Times | Div | Mod | And | Or
data OpR = Lt | Gt | Le | Ge | Eq | Neq
data OpB = BitAnd | BitOr

-- Statements

data Statement
  = Assignment {_lhs :: LValue, _rhs :: RValue}
  | RecordAssignment {_name :: Identifier, _r1 :: RValue, _r2 :: RValue}
  | IfThen {_condition :: BValue, _then :: Statements}
  | IfThenElse {_condition :: BValue, _then :: Statements, _else :: Statements}
  | While {_condition :: BValue, _body :: Statements}
  | Read {_lhs :: LValue}

type Statements = [Statement]

data Program = Program Declarations Statements
