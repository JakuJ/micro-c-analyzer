module MicroC.Parser
( parseProgram
) where

import qualified Grammar.Abs as A
import           Grammar.Par (myLexer, pProgram)
import qualified MicroC.AST  as C

parseProgram :: FilePath -> IO (Either String C.Program)
parseProgram path = do
  contents <- readFile path
  let lexemes = myLexer contents
  return $ tProgram <$> pProgram lexemes

tProgram :: A.Program -> C.Program
tProgram (A.Program decls stats) = C.Program (map tDecl decls) (map tStat stats)

tIdent :: A.Ident -> C.Identifier
tIdent (A.Ident i) = i

tStat :: A.Statement -> C.Statement
tStat (A.Assignment l a)       = C.Assignment (tLval l) (tArith a)
tStat (A.RecordOrVariable i s) = case s of
                                    A.Variable a   -> C.Assignment (C.Variable (tIdent i)) (tArith a)
                                    A.Record vals -> C.RecordAssignment (tIdent i) (map tArith vals)
tStat (A.IfThen c b)           = C.IfThen (tBool c) (map tStat b)
tStat (A.IfThenElse c b1 b2)   = C.IfThenElse (tBool c) (map tStat b1) (map tStat b2)
tStat (A.While c b)            = C.While (tBool c) (map tStat b)
tStat (A.ReadL l)              = C.Read (tLval l)
tStat (A.ReadI i)              = C.Read (C.Variable (tIdent i))
tStat (A.Write a)              = C.Write (tArith a)

tField :: A.Field -> C.Identifier
tField (A.Field n) = tIdent n

tDecl :: A.Declaration -> C.Declaration
tDecl (A.PrimDecl i)      = C.VariableDecl (tIdent i)
tDecl (A.ArrayDecl ix i)  = C.ArrayDecl (fromInteger ix) (tIdent i)
tDecl (A.RecordDecl ns i) = C.RecordDecl (tIdent i) (map tField ns)

tLval :: A.LValue -> C.LValue 'C.CInt
tLval (A.Array i ix)      = C.ArrayIndex (tIdent i) (tArith ix)
tLval (A.RecordField i f) = C.FieldAccess (tIdent i) (tIdent f)

tArith :: A.Arith -> C.RValue 'C.CInt
tArith (A.NegLiteral la) = C.Literal . negate . fromInteger $ la
tArith (A.Literal la)    = C.Literal (fromInteger la)
tArith (A.VariableR i)   = C.Reference (C.Variable (tIdent i))
tArith (A.ArrRecordR lv) = C.Reference (tLval lv)
tArith (A.Plus a b)      = C.OpA (tArith a) C.Add (tArith b)
tArith (A.Minus a b)     = C.OpA (tArith a) C.Sub (tArith b)
tArith (A.Mult a b)      = C.OpA (tArith a) C.Mult (tArith b)
tArith (A.Div a b)       = C.OpA (tArith a) C.Div (tArith b)
tArith (A.Mod a b)       = C.OpA (tArith a) C.Mod (tArith b)
tArith (A.BitAnd a b)    = C.OpA (tArith a) C.BitAnd (tArith b)
tArith (A.BitOr a b)     = C.OpA (tArith a) C.BitOr (tArith b)

tBool :: A.Boolean -> C.RValue 'C.CBool
tBool A.True              = C.Literal True
tBool A.False             = C.Literal False
tBool (A.Relation a op b) = C.OpR (tArith a) (tOpR op) (tArith b)
tBool (A.And a b)         = C.OpB (tBool a) C.And (tBool b)
tBool (A.Or a b)          = C.OpB (tBool a) C.Or (tBool b)
tBool (A.Not b)           = C.Not (tBool b)

tOpR :: A.OpRel -> C.OpRel
tOpR = \case
  A.LT  -> C.Lt
  A.LTE -> C.Le
  A.GT  -> C.Gt
  A.GTE -> C.Ge
  A.EQ  -> C.Eq
  A.NEQ -> C.Neq
