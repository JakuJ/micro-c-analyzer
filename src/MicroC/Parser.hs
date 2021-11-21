module MicroC.Parser
( Diagnostics
, parseFile
, parseProgram
) where

import           Control.Lens              (universe, (<&>), (^..))
import           Control.Monad.Writer.Lazy
import           Data.Bifunctor            (first)
import           Data.Data.Lens            (biplate)
import qualified Data.Map.Lazy             as M
import           Data.String.Interpolate   (i)
import qualified Grammar.Abs               as A
import           Grammar.Par               (myLexer, pProgram)
import           MicroC.AST
import qualified MicroC.AST                as C
import           MicroC.ProgramGraph       (declaredRecords)

-- | A type for a collection of diagnostic messages.
type Diagnostics = [String]

-- | Append a new error message to the log.
report :: String -> Writer Diagnostics ()
report = tell . pure

-- | Parse a MicroC source file and perform validation.
parseFile :: FilePath -> IO (Either Diagnostics C.Program)
parseFile = fmap parseProgram . readFile

-- | Parse MicroC source code and perform validation.
parseProgram :: String -> Either Diagnostics C.Program
parseProgram source = do
  prog@(Program ds ss) <- first pure . fmap tProgram . pProgram . myLexer $ source
  let recs = declaredRecords ds
  case execWriter $ checkFieldAccesses recs ss >> checkAssignments recs ss of
    []   -> Right prog
    errs -> Left errs

-- VALIDATION

checkFieldAccesses :: M.Map Identifier [Identifier] -> Statements -> Writer Diagnostics ()
checkFieldAccesses recs ss = do
  let fields = ss ^.. biplate @_ @(LValue 'CInt) . _FieldAccess
  forM_ fields $ \(r, f) -> do
    if M.member r recs
      then unless (f `elem` recs M.! r) $ report [i|Record #{r} does not define a field named #{f}|]
      else report [i|Undefined record: #{r}|]

checkAssignments :: M.Map Identifier [Identifier] -> Statements -> Writer Diagnostics ()
checkAssignments recs ss = do
  let rass = concatMap universe ss ^.. traverse . _RecordAssignment <&> fmap length
  forM_ rass $ \(r, l) -> do
    let fs = length $ recs M.! r
    if M.member r recs
      then when (l /= fs) $ report [i|Trying to assign #{l} values to record #{r} which was defined with #{fs} fields.|]
      else report [i|Undefined record: #{r}|]

-- TRANSLATION

tProgram :: A.Program -> C.Program
tProgram (A.Program decls stats) = C.Program (map tDecl decls) (map tStat stats)

tIdent :: A.Ident -> C.Identifier
tIdent (A.Ident x) = x

tStat :: A.Statement -> C.Statement
tStat (A.Assignment l a)       = C.Assignment (tLval l) (tArith a)
tStat (A.RecordOrVariable x s) = case s of
                                    A.Variable a   -> C.Assignment (C.Variable (tIdent x)) (tArith a)
                                    A.Record vals -> C.RecordAssignment (tIdent x) (map tArith vals)
tStat (A.IfThen c b)           = C.IfThen (tBool c) (map tStat b)
tStat (A.IfThenElse c b1 b2)   = C.IfThenElse (tBool c) (map tStat b1) (map tStat b2)
tStat (A.While c b)            = C.While (tBool c) (map tStat b)
tStat (A.ReadL l)              = C.Read (tLval l)
tStat (A.ReadI x)              = C.Read (C.Variable (tIdent x))
tStat (A.Write a)              = C.Write (tArith a)
tStat A.Continue               = C.Continue
tStat A.Break                  = C.Break

tField :: A.Field -> C.Identifier
tField (A.Field n) = tIdent n

tDecl :: A.Declaration -> C.Declaration
tDecl (A.PrimDecl x)      = C.VariableDecl (tIdent x)
tDecl (A.ArrayDecl ix x)  = C.ArrayDecl (fromInteger ix) (tIdent x)
tDecl (A.RecordDecl ns x) = C.RecordDecl (tIdent x) (map tField ns)

tLval :: A.LValue -> C.LValue 'C.CInt
tLval (A.Array x ix)      = C.ArrayIndex (tIdent x) (tArith ix)
tLval (A.RecordField x f) = C.FieldAccess (tIdent x) (tIdent f)

tArith :: A.Arith -> C.RValue 'C.CInt
tArith (A.NegLiteral la) = C.Literal . negate . fromInteger $ la
tArith (A.Literal la)    = C.Literal (fromInteger la)
tArith (A.VariableR x)   = C.Reference (C.Variable (tIdent x))
tArith (A.ArrRecordR lv) = C.Reference (tLval lv)
tArith (A.Plus a b)      = C.OpA (tArith a) C.Add (tArith b)
tArith (A.Minus a b)     = C.OpA (tArith a) C.Sub (tArith b)
tArith (A.Mult a b)      = C.OpA (tArith a) C.Mult (tArith b)
tArith (A.Div a b)       = C.OpA (tArith a) C.Div (tArith b)
tArith (A.Mod a b)       = C.OpA (tArith a) C.Mod (tArith b)

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
