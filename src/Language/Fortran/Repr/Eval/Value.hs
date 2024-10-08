{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}

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
import Language.Fortran.Repr.Type.Scalar.Common ( FKindLit )
import Language.Fortran.Repr.Type.Scalar ( fScalarTypeKind )

import Language.Fortran.Repr.Eval.Common
import qualified Language.Fortran.Repr.Eval.Value.Op as Op

import qualified Language.Fortran.Analysis as FA

import GHC.Generics ( Generic )
import qualified Data.Text as Text
import qualified Data.Char
import qualified Data.Bits
import Data.Int

import Control.Monad.Except

import Data.Word ( Word8 )

-- pure implementation
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Map ( Map )

-- | Error encountered while evaluating a Fortran expression to a value.
data Error
  = ENoSuchVar F.Name
  | EKindLitBadType F.Name FType
  | ENoSuchKindForType String FKindLit

  | EUnsupported String
  -- ^ Syntax which probably should be supported, but (currently) isn't.

  | EOp Op.Error
  | EOpTypeError String

  | ESpecial String
  -- ^ Special value-like expression that we can't evaluate usefully.

  | ELazy String
  -- ^ Catch-all for non-grouped errors.

    deriving stock (Generic, Show, Eq)

-- | A convenience constraint tuple defining the base requirements of the
--   'FValue' evaluator.
--
-- The evaluator is formed of combinators returning values in this monad. You
-- may insert your own evaluator which handles monadic actions differently,
-- provided it can fulfill these constraints.
type MonadFEvalValue m = (MonadFEval m, EvalTo m ~ FValue, MonadError Error m)

--------------------------------------------------------------------------------

-- | derivingvia helper
type FEvalValuePureT = WriterT [String] (ExceptT Error (Reader (Map F.Name FValue)))

-- | A simple pure interpreter for Fortran value evaluation programs.
newtype FEvalValuePure a = FEvalValuePure { unFEvalValuePure :: WriterT [String] (ExceptT Error (Reader (Map F.Name FValue))) a }
    deriving (Functor, Applicative, Monad) via FEvalValuePureT
    deriving (MonadReader (Map F.Name FValue)) via FEvalValuePureT
    deriving (MonadWriter [String]) via FEvalValuePureT
    deriving (MonadError Error) via FEvalValuePureT

instance MonadFEval FEvalValuePure where
    type EvalTo FEvalValuePure = FValue
    warn msg = tell [msg]
    lookupFVar nm = do
        m <- ask
        pure $ Map.lookup nm m

runEvalFValuePure
    :: Map F.Name FValue
    -> FEvalValuePure a -> Either Error (a, [String])
runEvalFValuePure m =
    flip runReader m . runExceptT . runWriterT . unFEvalValuePure

--------------------------------------------------------------------------------

evalVar :: MonadFEvalValue m => F.Name -> m FValue
evalVar name =
    lookupFVar name >>= \case
      Nothing  -> err $ ENoSuchVar name
      Just val -> pure val

evalExpr :: MonadFEvalValue m => F.Expression (FA.Analysis a) -> m FValue
evalExpr = \case
  e@(F.ExpValue _ _ astVal) ->
    case astVal of
      F.ValVariable name -> evalVar (FA.varName e)
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
    -- same here, could more arg evaluation into op
    evaledArgs <- traverse evalArg $ F.alistList args
    evalFunctionCall (forceVarExpr ve) evaledArgs
  _ -> err $ EUnsupported "Expression constructor"

forceVarExpr :: F.Expression (FA.Analysis a) -> F.Name
forceVarExpr = \case
  F.ExpValue _ _ (F.ValVariable v) -> v
  F.ExpValue _ _ (F.ValIntrinsic v) -> v
  _ -> error "program error, sent me an expr that wasn't a name"

evalLit :: MonadFEvalValue m => F.Value (FA.Analysis a) -> m FScalarValue
evalLit = \case
  F.ValInteger i mkp -> do
    evalMKp 4 mkp >>= \case
      4 -> pure $ FSVInt $ FInt4 $ read i
      8 -> pure $ FSVInt $ FInt8 $ read i
      2 -> pure $ FSVInt $ FInt2 $ read i
      1 -> pure $ FSVInt $ FInt1 $ read i
      k -> err $ ENoSuchKindForType "INTEGER" k
  F.ValReal r mkp -> do
    evalRealKp (F.exponentLetter (F.realLitExponent r)) mkp >>= \case
      4 -> pure $ FSVReal $ FReal4 $ F.readRealLit r
      8 -> pure $ FSVReal $ FReal8 $ F.readRealLit r
      k -> err $ ENoSuchKindForType "REAL" k
  F.ValLogical b mkp -> do
    evalMKp 4 mkp >>= \case
      4 -> pure $ FSVLogical $ FInt4 $ fLogicalNumericFromBool b
      8 -> pure $ FSVLogical $ FInt8 $ fLogicalNumericFromBool b
      2 -> pure $ FSVLogical $ FInt2 $ fLogicalNumericFromBool b
      1 -> pure $ FSVLogical $ FInt1 $ fLogicalNumericFromBool b
      k -> err $ ENoSuchKindForType "LOGICAL" k
  F.ValComplex (F.ComplexLit _ _ _cr _ci) ->
    -- TODO annoying & tedious. see Fortran 2008 spec 4.4.2.4
    -- 1. evaluate each part
    -- 2. determine kind parameter (largest real, or default if both ints)
    --    - fail here if a named part wasn't real or int
    -- 3. upgrade both parts to that kind
    -- 4. package and return
    err $ EUnsupported "COMPLEX literals"
  F.ValString s -> pure $ FSVString $ Text.pack s
  F.ValBoz boz -> do
    warn "requested to evaluate BOZ literal with no context: defaulting to INTEGER(4)"
    pure $ FSVInt $ FInt4 $ F.bozAsTwosComp boz
  F.ValHollerith s -> pure $ FSVString $ Text.pack s
  F.ValIntrinsic{} -> err $ ESpecial "lit was ValIntrinsic{} (intrinsic name)"
  F.ValVariable{}  -> err $ ESpecial "lit was ValVariable{} (variable name)"
  F.ValOperator{}  -> err $ ESpecial "lit was ValOperator{} (custom operator name)"
  F.ValAssignment  -> err $ ESpecial "lit was ValAssignment (overloaded assignment name)"
  F.ValStar        -> err $ ESpecial "lit was ValStar"
  F.ValColon       -> err $ ESpecial "lit was ValColon"
  F.ValType{}      -> err $ ELazy "lit was ValType: not used anywhere, don't know what it is"

err :: MonadError Error m => Error -> m a
err = throwError

evalKp :: MonadFEvalValue m => F.KindParam (FA.Analysis a) -> m FKindLit
evalKp = \case
  F.KindParamInt _ _ k ->
    -- TODO we may wish to check kind param sensibility here
    -- easy check is length (<=3)
    -- to catch the rest, we may need to read to Int16 and check.
    -- slow and unideal so for now let's assume no bad play such as INTEGER(256)
    pure $ read k
  F.KindParamVar _ _ var ->
    lookupFVar var >>= \case
      Just val -> case val of
        MkFScalarValue (FSVInt i) ->
          pure $ fIntUOp fromIntegral i
        _ -> err $ EKindLitBadType var (fValueType val)
      Nothing  -> err $ ENoSuchVar var

evalMKp :: MonadFEvalValue m => FKindLit -> Maybe (F.KindParam (FA.Analysis a)) -> m FKindLit
evalMKp kDef = \case
  Nothing -> pure kDef
  Just kp -> evalKp kp

-- TODO needs cleanup: internal repetition, common parts with evalKp. also needs
-- a docstring
evalRealKp :: MonadFEvalValue m => F.ExponentLetter -> Maybe (F.KindParam (FA.Analysis a)) -> m FKindLit
evalRealKp l = \case
  Nothing ->
    case l of
      F.ExpLetterE -> pure 4
      F.ExpLetterD -> pure 8
      F.ExpLetterQ -> do
        warn "TODO 1.2Q3 REAL literals not supported; defaulting to REAL(8)"
        pure 8
  Just kp -> do
    k <- evalKp kp
    case l of
      F.ExpLetterE -> -- @1.2E3_8@ syntax is permitted: use @_8@ kind param
        pure k
      F.ExpLetterD -> do -- @1.2D3_8@ syntax is nonsensical
        warn $  "TODO exponent letter wasn't E but you gave kind parameter."
             <> "\nthis isn't allowed, but we'll default to"
             <> " using kind parameter"
        pure k
      F.ExpLetterQ -> do
        warn "TODO 1.2Q3 REAL literals not supported; defaulting to REAL(8)"
        pure 8

evalUOp :: MonadFEvalValue m => F.UnaryOp -> FValue -> m FValue
evalUOp op v = do
    v' <- forceScalar v
    case op of
      F.Plus  -> wrapSOp $ Op.opIcNumericUOpInplace id     v'
      F.Minus -> wrapSOp $ Op.opIcNumericUOpInplace negate v'
      F.Not   -> -- TODO move this to Op (but logicals are a pain)
        case v' of
          FSVLogical bi ->
            pure $ MkFScalarValue $ FSVLogical $ fLogicalNot bi
          _ -> err $ EOp $ Op.EBadArgType1 ["LOGICAL"] $ fScalarValueType v'
      _ -> err $ EUnsupported $ "operator: " <> show op

wrapOp :: MonadFEvalValue m => Either Op.Error a -> m a
wrapOp = \case
  Right a -> pure a
  Left  e -> err $ EOp e

-- | Wrap the output of an operation that returns a scalar value into the main
--   evaluator.
wrapSOp :: MonadFEvalValue m => Either Op.Error FScalarValue -> m FValue
wrapSOp = \case
  Right a -> pure $ MkFScalarValue a
  Left  e -> err $ EOp e

-- | Evaluate explicit binary operators (ones denoted as such in the AST).
--
-- Note that this does not cover all binary operators -- there are many
-- intrinsics which use function syntax, but are otherwise binary operators.
evalBOp :: MonadFEvalValue m => F.BinaryOp -> FValue -> FValue -> m FValue
evalBOp bop l r = do
    -- TODO also see evalExpr: implement short-circuit eval here
    l' <- forceScalar l
    r' <- forceScalar r
    case bop of
      F.Addition       -> wrapSOp $ Op.opIcNumericBOp (+) l' r'
      F.Subtraction    -> wrapSOp $ Op.opIcNumericBOp (-) l' r'
      F.Multiplication -> wrapSOp $ Op.opIcNumericBOp (*) l' r'


      -- TODO confirm correct operation (not checked much)
      F.Division -> wrapSOp $ Op.opIcNumericBOpRealIntSep (div) (/) l' r'

      -- TODO basic - ints only. probably should support floats too.
      F.Exponentiation ->
        case (l', r') of
          (FSVInt li, FSVInt ri) ->
            pure $ MkFScalarValue $ FSVInt $ fIntBOpInplace (^) li ri
          (FSVReal lr, FSVReal ri) ->
            pure $ MkFScalarValue $ FSVReal $ fRealBOpInplace' (**) (**) lr ri
          (FSVReal lr, FSVInt ri) ->
            -- Handle case of a real raised to an integer power.
            pure $ MkFScalarValue $ FSVReal $ fRealBOpInplace' (**) (**) lr (FReal8 $ withFInt ri)

--          _ -> err $ ELazy "exponentiation: unsupported types"

      F.Concatenation  ->
        case (l', r') of
          (FSVString ls, FSVString rs) ->
            pure $ MkFScalarValue $ FSVString $ ls <> rs
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
    MkFScalarValue . FSVLogical . FInt4 . fLogicalNumericFromBool

evalFunctionCall :: MonadFEvalValue m => F.Name -> [FValue] -> m FValue
evalFunctionCall fname args =
    case fname of

      "kind"  -> do
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        let t = fScalarValueType v'
        case fScalarTypeKind t of
          Nothing -> err $ ELazy "called kind with non-kinded scalar"
          Just k  -> pure $ MkFScalarValue $ FSVInt $ FInt4 (fromIntegral k)

      "ior"  -> do
        args' <- forceArgs 2 args
        let [l, r] = args'
        l' <- forceScalar l
        r' <- forceScalar r
        evalIntrinsicIor l' r'

      "max"  -> evalIntrinsicMax args

      "char" -> do
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt i -> do
            -- TODO better error handling
            let c    = Data.Char.chr (fIntUOp fromIntegral i)
            pure $ MkFScalarValue $ FSVString $ Text.singleton c
          _ ->
            err $ EOpTypeError $
                "char: expected INT(x), got "<>show (fScalarValueType v')

      "not"  -> do
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt i -> do
            pure $ MkFScalarValue $ FSVInt $ fIntUOpInplace Data.Bits.complement i
          _ ->
            err $ EOpTypeError $
                "not: expected INT(x), got "<>show (fScalarValueType v')

      "int"  ->
        case args of
          [] -> err $ EOpTypeError $ "int: expected 1 or 2 arguments, got 0"
          [v] -> do
            -- @INT(x)@ == @INT(x, 4)@ (F2018 16.9.100:23, pg.381)
            (MkFScalarValue . FSVInt . FInt4) <$> evalIntrinsicInt4 v
          [v, vk] -> do
            vk' <- forceScalar vk
            case vk' of
              FSVInt vkI -> (MkFScalarValue . FSVInt) <$> evalIntrinsicInt v vkI
              _ ->
                err $ EOpTypeError $
                    "int: kind argument must be INTEGER, got "<>show (fScalarValueType vk')
          _ -> err $ EOpTypeError $ "int: expected 1 or 2 arguments, got >2"

      -- TODO all lies
      "int2" -> do
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt{} ->
            pure $ MkFScalarValue v'
          FSVReal r ->
            pure $ MkFScalarValue $ FSVInt $ FInt2 $ fRealUOp truncate r
          _ ->
            err $ EOpTypeError $
                "int: unsupported or unimplemented type: "<>show (fScalarValueType v')

      _      -> err $ EUnsupported $ "function call: " <> fname

-- TODO 2023-05-03 raehik: gfortran actually performs some range checks for
-- constants! @int(128, 1)@ errors with "this INT(4) is too big for INT(1)".
-- we don't do that currently. just means more plumbing
evalIntrinsicInt :: MonadFEvalValue m => FValue -> FInt -> m FInt
evalIntrinsicInt v = fIntUOp go
  where
    go :: (MonadFEvalValue m, Num a, Eq a) => a -> m FInt
    go = \case
      1 -> FInt1 <$> evalIntrinsicInt1 v
      2 -> FInt2 <$> evalIntrinsicInt2 v
      4 -> FInt4 <$> evalIntrinsicInt4 v
      8 -> FInt8 <$> evalIntrinsicInt8 v
      _ -> err $ ELazy "int: kind argument wasn't 1, 2, 4 or 8"

-- | @INT(a, 1)@
evalIntrinsicInt1 :: MonadFEvalValue m => FValue -> m Int8
evalIntrinsicInt1 = evalIntrinsicIntXCoerce coerceToI1
  where coerceToI1 = fIntUOp' id fromIntegral fromIntegral fromIntegral

-- | @INT(a, 2)@
evalIntrinsicInt2 :: MonadFEvalValue m => FValue -> m Int16
evalIntrinsicInt2 = evalIntrinsicIntXCoerce coerceToI2
  where coerceToI2 = fIntUOp' fromIntegral id fromIntegral fromIntegral

-- | @INT(a, 4)@, @INT(a)@
evalIntrinsicInt4 :: MonadFEvalValue m => FValue -> m Int32
evalIntrinsicInt4 = evalIntrinsicIntXCoerce coerceToI4
  where coerceToI4 = fIntUOp' fromIntegral fromIntegral id fromIntegral

-- | @INT(a, 8)@
evalIntrinsicInt8 :: MonadFEvalValue m => FValue -> m Int64
evalIntrinsicInt8 = evalIntrinsicIntXCoerce coerceToI8
  where coerceToI8 = fIntUOp' fromIntegral fromIntegral fromIntegral id

evalIntrinsicIntXCoerce
    :: forall r m
    .  (MonadFEvalValue m, Integral r) => (FInt -> r) -> FValue -> m r
evalIntrinsicIntXCoerce coerceToIX v = do
    v' <- forceScalar v
    case v' of
      FSVInt  i -> pure $ coerceToIX i
      FSVReal r -> pure $ fRealUOp truncate r
      _ ->
        err $ EOpTypeError $
            "int: unsupported or unimplemented type: "<>show (fScalarValueType v')

evalArg :: MonadFEvalValue m => F.Argument (FA.Analysis a) -> m FValue
evalArg (F.Argument _ _ _ ae) =
    case ae of
      F.ArgExpr        e -> evalExpr e
      F.ArgExprVar _ _ v -> evalVar  v

--------------------------------------------------------------------------------

-- exists because we used to support arrays (now stripped)
forceScalar :: MonadFEvalValue m => FValue -> m FScalarValue
forceScalar = \case
  MkFScalarValue v' -> pure v'

forceUnconsArg :: MonadFEvalValue m => [a] -> m (a, [a])
forceUnconsArg = \case
  []   -> err $ EOpTypeError "not enough arguments"
  a:as -> pure (a, as)

-- TODO can I use vector-sized to improve safety here? lol
-- it's just convenience either way
forceArgs :: MonadFEvalValue m => Int -> [a] -> m [a]
forceArgs numArgs l =
    if   length l == numArgs
    then pure l
    else err $ EOpTypeError $
            "expected "<>show numArgs<>" arguments; got "<>show (length l)

evalIntrinsicIor
    :: MonadFEvalValue m => FScalarValue -> FScalarValue -> m FValue
evalIntrinsicIor l r = case (l, r) of
  (FSVInt li, FSVInt ri) -> wrapSOp $ FSVInt <$> Op.opIor li ri
  _ -> err $ ELazy "ior: bad args"

-- https://gcc.gnu.org/onlinedocs/gfortran/MAX.html
-- TODO should support arrays! at least for >=F2010
evalIntrinsicMax
    :: MonadFEvalValue m => [FValue] -> m FValue
evalIntrinsicMax = \case
  []   -> err $ EOpTypeError "max intrinsic expects at least 1 argument"
  v:vs -> do
    v' <- forceScalar v
    vs' <- traverse forceScalar vs
    go v' vs'
  where
    go vCurMax [] = pure $ MkFScalarValue vCurMax
    go vCurMax (v:vs) =
        case vCurMax of
          FSVInt{} ->
            case v of
              FSVInt{} -> do
                vNewMax <- wrapOp $ Op.opIcNumericBOp max vCurMax v
                go vNewMax vs
              _ ->
                err $ EOpTypeError $
                    "max: expected INT(x), got "<>show (fScalarValueType v)
          FSVReal{} ->
            case v of
              FSVReal{} -> do
                vNewMax <- wrapOp $ Op.opIcNumericBOp max vCurMax v
                go vNewMax vs
              _ ->
                err $ EOpTypeError $
                    "max: expected REAL(x), got "<>show (fScalarValueType v)
          _ ->
            err $ EOpTypeError $
                "max: unsupported type: "<> show (fScalarValueType vCurMax)

-- | Evaluate a constant expression (F2018 10.1.12).
evalConstExpr :: MonadFEvalValue m => F.Expression (FA.Analysis a) -> m FValue
evalConstExpr = evalExpr
