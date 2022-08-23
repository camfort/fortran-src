{-# LANGUAGE ConstraintKinds #-}

module Language.Fortran.Repr.Translate.Value where

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.AST.Literal.Real as F
import qualified Language.Fortran.AST.Literal.Complex as F
import qualified Language.Fortran.AST.Literal.Boz as F
import Language.Fortran.Repr.Translate.Common

import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar
import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Logical.Machine
import Language.Fortran.Repr.Value.Scalar.String
import Language.Fortran.Repr.Type
import qualified Language.Fortran.Repr.Eval.Op as Op

import GHC.Generics ( Generic )
import qualified Data.Text as Text

import Control.Monad.Except

type MonadTranslateValue m = (MonadTranslate m, TranslateTo m ~ FValue, MonadError Error m)

-- TODO best for temp KPs: String, Integer, Text? Word8??
type KindLit = String

-- | Value translation error.
data Error
  = ENoSuchVar F.Name
  | EKindLitBadType F.Name FType
  | ENoSuchKindForType String KindLit
  | EUnsupported String
  | EOp Op.Error
  | ELazy String
  -- ^ Catch-all for non-grouped errors.
    deriving stock (Generic, Show, Eq)

translateVar :: MonadTranslateValue m => F.Name -> m FValue
translateVar name =
    lookupFVar name >>= \case
      Nothing  -> err $ ENoSuchVar name
      Just val -> return val

translateExpr :: MonadTranslateValue m => F.Expression a -> m FValue
translateExpr = \case
  F.ExpValue _ _ astVal ->
    case astVal of
      F.ValVariable name -> translateVar name
      -- TODO: Do same with ValIntrinsic??? idk...
      _ -> MkFScalarValue <$> translateLit astVal
  F.ExpUnary  _ _ uop e   -> do
    v <- translateExpr e
    translateUOp uop v
  F.ExpBinary _ _ bop le re -> do
    -- TODO 2022-08-23 raehik: here is where we would implement
    -- short-circuiting, by inspecting the bop earlier and having special cases
    -- for certain bops
    lv <- translateExpr le
    rv <- translateExpr re
    translateBOp bop lv rv
  F.ExpFunctionCall _ _ ve args -> do
    translatedArgs <- traverse translateArg $ F.alistList args
    translateFunctionCall (forceVarExpr ve) translatedArgs
  _ -> err $ EUnsupported "Expression constructor"

forceVarExpr :: F.Expression a -> F.Name
forceVarExpr = \case
  F.ExpValue _ _ (F.ValVariable v) -> v
  _ -> error "program error, sent me an expr that wasn't a name"

translateLit :: MonadTranslateValue m => F.Value a -> m FScalarValue
translateLit = \case
  F.ValInteger i mkp -> do
    translateKp "4" mkp >>= \case
      "4" -> return $ FSVInt $ SomeFKinded $ FInt4 $ read i
      "8" -> return $ FSVInt $ SomeFKinded $ FInt8 $ read i
      "2" -> return $ FSVInt $ SomeFKinded $ FInt2 $ read i
      "1" -> return $ FSVInt $ SomeFKinded $ FInt1 $ read i
      k   -> err $ ENoSuchKindForType "INTEGER" k
  F.ValReal r mkp -> do
    translateKp "4" mkp >>= \case
      "4" -> return $ FSVReal $ SomeFKinded $ FReal4 $ F.readRealLit r
      "8" -> return $ FSVReal $ SomeFKinded $ FReal8 $ F.readRealLit r
      k   -> err $ ENoSuchKindForType "REAL" k
  F.ValLogical b mkp -> do
    translateKp "4" mkp >>= \case
      "4" -> return $ FSVLogical $ SomeFKinded $ FInt4 $ fLogicalNumericFromBool b
      "8" -> return $ FSVLogical $ SomeFKinded $ FInt8 $ fLogicalNumericFromBool b
      "2" -> return $ FSVLogical $ SomeFKinded $ FInt2 $ fLogicalNumericFromBool b
      "1" -> return $ FSVLogical $ SomeFKinded $ FInt1 $ fLogicalNumericFromBool b
      k   -> err $ ENoSuchKindForType "LOGICAL" k
  F.ValComplex (F.ComplexLit _ _ _cr _ci) ->
    -- TODO annoying & tedious. see Fortran 2008 spec 4.4.2.4
    -- 1. translate each part
    -- 2. determine kind parameter (largest real, or default if both ints)
    --    - fail here if a named part wasn't real or int
    -- 3. upgrade both parts to that kind
    -- 4. package and return
    err $ EUnsupported "COMPLEX literals"
  F.ValString s -> return $ FSVString $ someFString $ Text.pack s
  F.ValBoz boz -> do
    warn "requested to translate BOZ literal with no context: defaulting to INTEGER(4)"
    return $ FSVInt $ SomeFKinded $ FInt4 $ F.bozAsTwosComp boz
  F.ValHollerith s -> return $ FSVString $ someFString $ Text.pack s
  F.ValIntrinsic{} -> error "you tried to translate a lit, but it was an intrinsic name"
  F.ValVariable{} ->  error "you tried to translate a lit, but it was a variable name"
  F.ValOperator{} ->  error "you tried to translate a lit, but it was a custom operator name"
  F.ValAssignment ->  error "you tried to translate a lit, but it was an overloaded assignment name"
  F.ValStar       ->  error "you tried to translate a lit, but it was a star"
  F.ValColon      ->  error "you tried to translate a lit, but it was a colon"
  F.ValType{}     ->  error "not used anywhere, don't know what it is"

err :: MonadError Error m => Error -> m a
err = throwError

translateKp :: MonadTranslateValue m => KindLit -> Maybe (F.KindParam a) -> m KindLit
translateKp kDef = \case
  Nothing -> return kDef
  Just kp -> case kp of
    F.KindParamInt _ _ k -> return $ read k
    F.KindParamVar _ _ var ->
      lookupFVar var >>= \case
        Just val -> case val of
          MkFScalarValue (FSVInt (SomeFKinded i)) ->
            return $ fIntUOp' show show show show i
          _ -> err $ EKindLitBadType var (fValueType val)
        Nothing  -> err $ ENoSuchVar var

translateUOp :: MonadTranslateValue m => F.UnaryOp -> FValue -> m FValue
translateUOp op v = do
    v' <- forceScalar v
    case op of
      F.Plus  -> wrapSOp $ Op.opIcNumericUOpInplace id     v'
      F.Minus -> wrapSOp $ Op.opIcNumericUOpInplace negate v'
      F.Not   -> -- TODO move this to Op (but logicals are a pain)
        case v' of
          FSVLogical (SomeFKinded bi) ->
            return $ MkFScalarValue $ FSVLogical $ SomeFKinded $ fLogicalNot bi
          _ -> err $ EOp $ Op.EBadArgType1 ["LOGICAL"] $ fScalarValueType v'
      _ -> err $ EUnsupported $ "operator: " <> show op

forceScalar :: MonadTranslateValue m => FValue -> m FScalarValue
forceScalar = \case
  MkFArrayValue{} -> err $ EUnsupported "no array values in translate for now thx"
  MkFScalarValue v' -> return v'

wrapOp :: MonadTranslateValue m => Either Op.Error a -> m a
wrapOp = \case
  Right a -> return a
  Left  e -> err $ EOp e

wrapSOp :: MonadTranslateValue m => Either Op.Error FScalarValue -> m FValue
wrapSOp = \case
  Right a -> return $ MkFScalarValue a
  Left  e -> err $ EOp e

translateBOp :: MonadTranslateValue m => F.BinaryOp -> FValue -> FValue -> m FValue
translateBOp bop l r = do
    l' <- forceScalar l
    r' <- forceScalar r
    case bop of
      F.Addition       -> wrapSOp $ Op.opIcNumericBOp (+) l' r'
      F.Subtraction    -> wrapSOp $ Op.opIcNumericBOp (-) l' r'
      F.Multiplication -> wrapSOp $ Op.opIcNumericBOp (*) l' r'

      F.Division -> -- TODO rather complicated
        err $ EUnsupported "division"
      F.Exponentiation -> -- TODO not looked, certainly custom
        err $ EUnsupported "exponentiation"

      F.Concatenation  ->
        case (l', r') of
          (FSVString ls, FSVString rs) ->
            return $ MkFScalarValue $ FSVString $ concatSomeFString ls rs
          _ -> err $ ELazy "concat strings only please"

      F.GT  -> defFLogical <$> wrapOp (Op.opIcNumRelBOp (>)  l' r')
      F.GTE -> defFLogical <$> wrapOp (Op.opIcNumRelBOp (>=) l' r')
      F.LT  -> defFLogical <$> wrapOp (Op.opIcNumRelBOp (<) l' r')
      F.LTE -> defFLogical <$> wrapOp (Op.opIcNumRelBOp (<=) l' r')
      F.NE  -> defFLogical <$> wrapOp (Op.opIcNumRelBOp (/=) l' r')
      F.EQ  -> defFLogical <$> wrapOp (Op.opEq l' r')

      F.And -> defFLogical <$> wrapOp (Op.opIcLogicalBOp (&&) l' r')
      F.Or  -> defFLogical <$> wrapOp (Op.opIcLogicalBOp (||) l' r')
      F.XOr -> defFLogical <$> wrapOp (Op.opIcLogicalBOp boolXor l' r')
      F.Equivalent -> defFLogical <$> wrapOp (Op.opIcLogicalBOp (==) l' r')
      F.NotEquivalent -> defFLogical <$> wrapOp (Op.opIcLogicalBOp (/=) l' r')

      F.BinCustom{} -> -- TODO
        err $ EUnsupported "custom binary operators"

boolXor :: Bool -> Bool -> Bool
boolXor True  False = True
boolXor False True  = True
boolXor _     _     = False

defFLogical :: Bool -> FValue
defFLogical =
    MkFScalarValue . FSVLogical . SomeFKinded . FInt4 . fLogicalNumericFromBool

translateFunctionCall :: MonadTranslateValue m => F.Name -> [FValue] -> m FValue
translateFunctionCall fname _args =
    case fname of
{-
      "ior"  -> ior' es
      "max"  -> max' es
      "char" -> char' es
      "not"  -> not' es
      "int"  -> int' es
      "int2" -> int' es
-}
      _      -> err $ EUnsupported $ "function call: " <> fname

translateArg :: MonadTranslateValue m => F.Argument a -> m FValue
translateArg (F.Argument _ _ _ ae) =
    case ae of
      F.ArgExpr        e -> translateExpr e
      F.ArgExprVar _ _ v -> translateVar v
