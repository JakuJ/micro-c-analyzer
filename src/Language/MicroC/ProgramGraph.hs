{-# LANGUAGE DataKinds #-}

module Language.MicroC.ProgramGraph where

import qualified Language.MicroC.AST as AST

type State = String

type Identifier = String

data Action 
  = DeclAction AST.Declaration
  | AssignAction Identifier (AST.RValue 'AST.CInt)
  | ReadAction Identifier
  | WriteAction (AST.RValue 'AST.CInt)
  | BoolAction (AST.RValue 'AST.CBool)

type Edge = (State, Action, State)

type PG = [Edge]