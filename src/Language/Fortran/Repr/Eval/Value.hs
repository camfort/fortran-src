{-# LANGUAGE ConstraintKinds #-}

-- | Evaluate AST terms to values in the value representation.

module Language.Fortran.Repr.Eval.Value where

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.AST.Literal.Real as F
import qualified Language.Fortran.AST.Literal.Complex as F
import qualified Language.Fortran.AST.Literal.Boz as F

import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar
import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Logical.Machine
import Language.Fortran.Repr.Value.Scalar.String

import Language.Fortran.Repr.Type ( FType )

import Language.Fortran.Repr.Eval.Common
import qualified Language.Fortran.Repr.Eval.Value.Op as Op


import GHC.Generics ( Generic )
import qualified Data.Text as Text

import Control.Monad.Except

-- simple implementation
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Map ( Map )

-- | A convenience type over 'MonadEval' bringing all requirements into scope.
type MonadEvalValue m = (MonadEval m, EvalTo m ~ FValue, MonadError Error m)

-- | Value evaluation error.
data Error
  = ENoSuchVar F.Name
  | EKindLitBadType F.Name FType
  | ENoSuchKindForType String KindLit
  | EUnsupported String
  | EOp Op.Error
  | EOpTypeError String
  | ELazy String
  -- ^ Catch-all for non-grouped errors.
    deriving stock (Generic, Show, Eq)

-- TODO best for temp KPs: String, Integer, Text? Word8??
type KindLit = String

--------------------------------------------------------------------------------

-- | A simple pure interpreter for Fortran value evaluation programs.
type EvalValueSimple = WriterT [String] (ExceptT Error (Reader (Map F.Name FValue)))

instance MonadEval EvalValueSimple where
    type EvalTo EvalValueSimple = FValue
    warn msg = tell [msg]
    lookupFVar nm = do
        m <- ask
        pure $ Map.lookup nm m

runEvalValueSimple
    :: Map F.Name FValue
    -> EvalValueSimple a -> Either Error (a, [String])
runEvalValueSimple m = flip runReader m . runExceptT . runWriterT

--------------------------------------------------------------------------------

evalVar :: MonadEvalValue m => F.Name -> m FValue
evalVar name =
    lookupFVar name >>= \case
      Nothing  -> err $ ENoSuchVar name
      Just val -> return val

evalExpr :: MonadEvalValue m => F.Expression a -> m FValue
evalExpr = \case
  F.ExpValue _ _ astVal ->
    case astVal of
      F.ValVariable name -> evalVar name
      -- TODO: Do same with ValIntrinsic??? idk...
      _ -> MkFScalarValue <$> evalLit astVal
  F.ExpUnary  _ _ uop e   -> do
    v <- evalExpr e
    evalUOp uop v
  F.ExpBinary _ _ bop le re -> do
    -- TODO 2022-08-23 raehik: here is where we would implement
    -- short-circuiting, by inspecting the bop earlier and having special cases
    -- for certain bops
    lv <- evalExpr le
    rv <- evalExpr re
    evalBOp bop lv rv
  F.ExpFunctionCall _ _ ve args -> do
    evaledArgs <- traverse evalArg $ F.alistList args
    evalFunctionCall (forceVarExpr ve) evaledArgs
  _ -> err $ EUnsupported "Expression constructor"

forceVarExpr :: F.Expression a -> F.Name
forceVarExpr = \case
  F.ExpValue _ _ (F.ValVariable v) -> v
  _ -> error "program error, sent me an expr that wasn't a name"

evalLit :: MonadEvalValue m => F.Value a -> m FScalarValue
evalLit = \case
  F.ValInteger i mkp -> do
    evalKp "4" mkp >>= \case
      "4" -> return $ FSVInt $ SomeFKinded $ FInt4 $ read i
      "8" -> return $ FSVInt $ SomeFKinded $ FInt8 $ read i
      "2" -> return $ FSVInt $ SomeFKinded $ FInt2 $ read i
      "1" -> return $ FSVInt $ SomeFKinded $ FInt1 $ read i
      k   -> err $ ENoSuchKindForType "INTEGER" k
  F.ValReal r mkp -> do
    evalRealKp (F.exponentLetter (F.realLitExponent r)) mkp >>= \case
      "4" -> return $ FSVReal $ SomeFKinded $ FReal4 $ F.readRealLit r
      "8" -> return $ FSVReal $ SomeFKinded $ FReal8 $ F.readRealLit r
      k   -> err $ ENoSuchKindForType "REAL" k
  F.ValLogical b mkp -> do
    evalKp "4" mkp >>= \case
      "4" -> return $ FSVLogical $ SomeFKinded $ FInt4 $ fLogicalNumericFromBool b
      "8" -> return $ FSVLogical $ SomeFKinded $ FInt8 $ fLogicalNumericFromBool b
      "2" -> return $ FSVLogical $ SomeFKinded $ FInt2 $ fLogicalNumericFromBool b
      "1" -> return $ FSVLogical $ SomeFKinded $ FInt1 $ fLogicalNumericFromBool b
      k   -> err $ ENoSuchKindForType "LOGICAL" k
  F.ValComplex (F.ComplexLit _ _ _cr _ci) ->
    -- TODO annoying & tedious. see Fortran 2008 spec 4.4.2.4
    -- 1. evaluate each part
    -- 2. determine kind parameter (largest real, or default if both ints)
    --    - fail here if a named part wasn't real or int
    -- 3. upgrade both parts to that kind
    -- 4. package and return
    err $ EUnsupported "COMPLEX literals"
  F.ValString s -> return $ FSVString $ someFString $ Text.pack s
  F.ValBoz boz -> do
    warn "requested to evaluate BOZ literal with no context: defaulting to INTEGER(4)"
    return $ FSVInt $ SomeFKinded $ FInt4 $ F.bozAsTwosComp boz
  F.ValHollerith s -> return $ FSVString $ someFString $ Text.pack s
  F.ValIntrinsic{} -> error "you tried to evaluate a lit, but it was an intrinsic name"
  F.ValVariable{} ->  error "you tried to evaluate a lit, but it was a variable name"
  F.ValOperator{} ->  error "you tried to evaluate a lit, but it was a custom operator name"
  F.ValAssignment ->  error "you tried to evaluate a lit, but it was an overloaded assignment name"
  F.ValStar       ->  error "you tried to evaluate a lit, but it was a star"
  F.ValColon      ->  error "you tried to evaluate a lit, but it was a colon"
  F.ValType{}     ->  error "not used anywhere, don't know what it is"

err :: MonadError Error m => Error -> m a
err = throwError

evalKp :: MonadEvalValue m => KindLit -> Maybe (F.KindParam a) -> m KindLit
evalKp kDef = \case
  Nothing -> return kDef
  Just kp -> case kp of
    F.KindParamInt _ _ k -> return k
    F.KindParamVar _ _ var ->
      lookupFVar var >>= \case
        Just val -> case val of
          MkFScalarValue (FSVInt (SomeFKinded i)) ->
            return $ fIntUOp' show show show show i
          _ -> err $ EKindLitBadType var (fValueType val)
        Nothing  -> err $ ENoSuchVar var

-- TODO needs cleanup: internal repetition, common parts with evalKp. also needs
-- a docstring
evalRealKp :: MonadEvalValue m => F.ExponentLetter -> Maybe (F.KindParam a) -> m KindLit
evalRealKp l mkp =
    kindViaKindParam >>= \case
      Nothing ->
        case l of
          F.ExpLetterE -> pure "4"
          F.ExpLetterD -> pure "8"
          F.ExpLetterQ -> do
            warn "TODO 1.2Q3 REAL literals not supported; defaulting to REAL(8)"
            evalRealKp F.ExpLetterD mkp
      Just kkp ->
        case l of
          F.ExpLetterE -> -- @1.2E3_8@ syntax is permitted: use @_8@ kind param
            pure kkp
          F.ExpLetterD -> do -- @1.2D3_8@ syntax is nonsensical
            warn $  "TODO exponent letter wasn't E but you gave kind parameter."
                 <> "\nthis isn't allowed, but we'll default to"
                 <> " using kind parameter"
            pure kkp
          F.ExpLetterQ -> do
            warn "TODO 1.2Q3 REAL literals not supported; defaulting to REAL(8)"
            evalRealKp F.ExpLetterD mkp
  where
    kindViaKindParam =
        case mkp of
          Nothing -> pure Nothing
          Just kp -> case kp of
            F.KindParamInt _ _ k -> pure $ Just k
            F.KindParamVar _ _ var ->
              lookupFVar var >>= \case
                Just val -> case val of
                  MkFScalarValue (FSVInt (SomeFKinded i)) ->
                    pure $ Just $ fIntUOp' show show show show i
                  _ -> err $ EKindLitBadType var (fValueType val)
                Nothing  -> err $ ENoSuchVar var

evalUOp :: MonadEvalValue m => F.UnaryOp -> FValue -> m FValue
evalUOp op v = do
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

wrapOp :: MonadEvalValue m => Either Op.Error a -> m a
wrapOp = \case
  Right a -> return a
  Left  e -> err $ EOp e

-- | Wrap the output of an operation that returns a scalar value into the main
--   evaluator.
wrapSOp :: MonadEvalValue m => Either Op.Error FScalarValue -> m FValue
wrapSOp = \case
  Right a -> return $ MkFScalarValue a
  Left  e -> err $ EOp e

-- | Evaluate explicit binary operators (ones denoted as such in the AST).
--
-- Note that this does not cover all binary operators -- there are many
-- intrinsics which use function syntax, but are otherwise binary operators.
evalBOp :: MonadEvalValue m => F.BinaryOp -> FValue -> FValue -> m FValue
evalBOp bop l r = do
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

evalFunctionCall :: MonadEvalValue m => F.Name -> [FValue] -> m FValue
evalFunctionCall fname args =
    case fname of
      "ior"  -> do
        args' <- forceArgs 2 args
        let [l, r] = args'
        l' <- forceScalar l
        r' <- forceScalar r
        evalBOpIor' l' r'
{-
      "max"  -> max' es
      "char" -> char' es
      "not"  -> not' es
      "int"  -> int' es
      "int2" -> int' es
-}
      _      -> err $ EUnsupported $ "function call: " <> fname

evalArg :: MonadEvalValue m => F.Argument a -> m FValue
evalArg (F.Argument _ _ _ ae) =
    case ae of
      F.ArgExpr        e -> evalExpr e
      F.ArgExprVar _ _ v -> evalVar  v

--------------------------------------------------------------------------------

forceScalar :: MonadEvalValue m => FValue -> m FScalarValue
forceScalar = \case
  MkFArrayValue{} -> err $ EUnsupported "no array values in eval for now thx"
  MkFScalarValue v' -> return v'

forceUnconsArg :: MonadEvalValue m => [a] -> m (a, [a])
forceUnconsArg = \case
  []   -> err $ EOpTypeError "not enough arguments"
  a:as -> return (a, as)

-- TODO can I use vector-sized to improve safety here? lol
-- it's just convenience either way
forceArgs :: MonadEvalValue m => Int -> [a] -> m [a]
forceArgs numArgs l =
    if   length l == numArgs
    then return l
    else err $ EOpTypeError $
            "expected "<>show numArgs<>" arguments; got "<>show (length l)

evalBOpIor'
    :: MonadEvalValue m => FScalarValue -> FScalarValue -> m FValue
evalBOpIor' l r = wrapSOp $ FSVInt <$> Op.opIor l r
