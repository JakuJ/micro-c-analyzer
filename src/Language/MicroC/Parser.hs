{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Language.MicroC.Parser (
    parseProgram
) where

import qualified Grammar.Abs         as A
import           Grammar.Par         (myLexer, pProgram)
import qualified Language.MicroC.AST as C

parseProgram :: FilePath -> IO (Either String C.Program)
parseProgram path = do
    contents <- readFile path
    let lexemes = myLexer contents
    return $ tProgram <$> pProgram lexemes

tProgram :: A.Program -> C.Program
tProgram (A.Program decls stats) = C.Program (map tDecl decls) (concatMap tStat stats)

tIdent :: A.Ident -> C.Identifier
tIdent (A.Ident i) = i

tStat :: A.Statement -> [C.Statement]
tStat (A.Assignment l a)           = pure $ C.Assignment (tLval l) (tArith a)
tStat (A.RecordOrVariable i s)     = pure $ case s of
                                              A.Variable a   -> C.Assignment (C.Variable (tIdent i)) (tArith a)
                                              A.Record vals -> C.RecordAssignment (tIdent i) (map tArith vals)
tStat (A.IfThen c b)               = pure $ C.IfThen (tBool c) (concatMap tStat b)
tStat (A.IfThenElse c b1 b2)       = pure $ C.IfThenElse (tBool c) (concatMap tStat b1) (concatMap tStat b2)
tStat (A.While c b)                = pure $ C.While (tBool c) (concatMap tStat b)
tStat (A.ReadL l)                  = pure $ C.Read (tLval l)
tStat (A.ReadI i)                  = pure $ C.Read (C.Variable (tIdent i))
tStat (A.Write a)                  = pure $ C.Write (tArith a)

tField :: A.Field -> C.Identifier
tField (A.Field n) = tIdent n

tDecl :: A.Declaration -> C.Declaration
tDecl (A.PrimDecl i)      = C.VariableDecl (tIdent i)
tDecl (A.ArrayDecl ix i)  = C.ArrayDecl (fromInteger ix) (tIdent i)
tDecl (A.RecordDecl ns i) = C.RecordDecl (tIdent i) (map tField ns)

tLval :: A.LValue -> C.LValue 'C.CInt
tLval (A.Array i ix)      = C.ArrayIndex (tIdent i) (tArith ix)
tLval (A.RecordField i f) = C.FieldAccess (tIdent i) (tIdent f)

tLit :: A.LitArith -> C.TypeRepr 'C.CInt
tLit (A.Lit i)    = fromInteger i
tLit (A.LitNeg i) = fromInteger (- i)

tArith :: A.Arith -> C.RValue 'C.CInt
tArith (A.Literal la)      = C.Literal (tLit la)
tArith (A.VariableR lv)    = C.Reference (tLval lv)
tArith (A.AppArith a op b) = C.OpA (tArith a) (tOpA op) (tArith b)
tArith (A.Parens expr)     = tArith expr

tOpA :: A.OpA -> C.OpArith
tOpA = \case
    A.Plus  -> C.Add
    A.Minus -> C.Sub
    A.Mult  -> C.Mult
    A.Div   -> C.Div

tBool :: A.Boolean -> C.RValue 'C.CBool
tBool A.True                = C.Literal True
tBool A.False               = C.Literal False
tBool (A.AppRel a op b)     = C.OpR (tArith a) (tOpR op) (tArith b)
tBool (A.AppBoolean a op b) = C.OpB (tBool a) (tOpB op) (tBool b)
tBool (A.Not b)             = C.Not (tBool b)

tOpR :: A.OpRel -> C.OpRel
tOpR = \case
    A.LT  -> C.Lt
    A.LTE -> C.Le
    A.GT  -> C.Gt
    A.GTE -> C.Ge
    A.EQ  -> C.Eq
    A.NEQ -> C.Neq

tOpB :: A.OpBoolean -> C.OpBool
tOpB = \case
    A.And -> C.And
    A.Or  -> C.Or
