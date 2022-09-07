{-# LANGUAGE ConstraintKinds #-}

{- | Evaluate AST terms to values in the value representation.

TODO
  * .OR. etc: do we rewrap into LOGICAL(4)? do we use internal kinding rules to
    return whatever LOGICALs we compared? probably latter, but if so needs a bit
    of checking to confirm behaviour (tests!)
-}

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
import Language.Fortran.Repr.Type.Scalar.Int

import Language.Fortran.Repr.Eval.Common
import qualified Language.Fortran.Repr.Eval.Value.Op as Op

import GHC.Generics ( Generic )
import qualified Data.Text as Text
import qualified Data.Char
import qualified Data.Bits

import Control.Monad.Reader

class (MonadEval m, EvalTo m ~ FValue, MonadReader env m, HasEvalValueCfg env, Op.MonadEvalOp env m)
  => MonadEvalValue env m where
    err :: Error -> m a

class HasEvalValueCfg env where
    getEvalValueCfg :: env -> Cfg

data Cfg = Cfg
  { cfgOpEvalCfg :: Op.Cfg

  , cfgShortCircuit :: Bool
  -- ^ Whether or not to attempt to short circuit certain operations.
  --
  -- Certain operations like binary OR/AND have cases where the result of
  -- testing one operand can determine the result without testing remaining
  -- operands. When enabled, the evaluator emulates that behaviour by evaluating
  -- operands left-to-right, and potentially returning an evaluating value
  -- "early", without evaluating all operands.
  --
  -- Enabling this option enables evaluating certain syntactically invalid
  -- expressions, e.g. @.TRUE. .OR. "string" == .TRUE.@.

  } deriving stock (Generic, Show, Eq)
instance HasEvalValueCfg Cfg where getEvalValueCfg = id
instance Op.HasEvalOpCfg Cfg where getEvalOpCfg = cfgOpEvalCfg

defCfg :: Cfg
defCfg = Cfg Op.defCfg False

-- | Value evaluation error.
data Error
  = ENoSuchVar F.Name
  | EKindLitBadType F.Name FType
  | ENoSuchKindForType String KindLit
  | EUnsupported String
  | EOp Op.Error
  | EOpTypeError String

  | EOpTypeError' String [FType]
  -- ^ Encountered a type error while evaluating an operation.

  | EOther String
  -- ^ Catch-all for non-grouped errors.
    deriving stock (Generic, Show, Eq)

-- TODO best for temp KPs: String, Integer, Text? Word8??
type KindLit = String

--------------------------------------------------------------------------------

evalVar :: MonadEvalValue env m => F.Name -> m FValue
evalVar name =
    lookupFVar name >>= \case
      Nothing  -> err $ ENoSuchVar name
      Just val -> return val

evalExpr :: MonadEvalValue env m => F.Expression a -> m FValue
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
    evalBOp bop le re
  F.ExpFunctionCall _ _ ve args -> do
    evalFunctionCall (forceVarExpr ve) $ F.alistList args
  _ -> err $ EUnsupported "Expression constructor"

forceVarExpr :: F.Expression a -> F.Name
forceVarExpr = \case
  F.ExpValue _ _ (F.ValVariable v) -> v
  F.ExpValue _ _ (F.ValIntrinsic v) -> v
  _ -> error "program error, sent me an expr that wasn't a name"

evalKpInt :: MonadEvalValue env m => Maybe (F.KindParam a) -> m FTInt
evalKpInt = \case
  Nothing -> pure FTInt4
  Just kp -> case kp of
    F.KindParamInt _ _ k -> go k
    F.KindParamVar _ _ var ->
      lookupFVar var >>= \case
        Nothing  -> err $ ENoSuchVar var
        Just val -> evalKpInt' val
  where
    go = \case
      "4" -> pure FTInt4
      "8" -> pure FTInt8
      "2" -> pure FTInt2
      "1" -> pure FTInt1
      k   -> err $ ENoSuchKindForType "INTEGER" k

evalKpInt' :: MonadEvalValue env m => FValue -> m FTInt
evalKpInt' = \case
  MkFScalarValue (FSVInt (SomeFKinded i)) ->
    case fIntUOp' show show show show i of
      "4" -> pure FTInt4
      "8" -> pure FTInt8
      "2" -> pure FTInt2
      "1" -> pure FTInt1
      k   -> err $ ENoSuchKindForType "INTEGER" k
  v -> err $ EKindLitBadType "TODO LOST" (fValueType v)

evalLit :: MonadEvalValue env m => F.Value a -> m FScalarValue
evalLit = \case
  F.ValInteger i mkp -> do
    evalKpInt mkp >>= \case
      FTInt4 -> return $ FSVInt $ SomeFKinded $ FInt4 $ read i
      FTInt8 -> return $ FSVInt $ SomeFKinded $ FInt8 $ read i
      FTInt2 -> return $ FSVInt $ SomeFKinded $ FInt2 $ read i
      FTInt1 -> return $ FSVInt $ SomeFKinded $ FInt1 $ read i
      FTInt16 -> err $ EOther "INTEGER(16) values not supported"
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

evalKp :: MonadEvalValue env m => KindLit -> Maybe (F.KindParam a) -> m KindLit
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
evalRealKp :: MonadEvalValue env m => F.ExponentLetter -> Maybe (F.KindParam a) -> m KindLit
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

evalUOp :: MonadEvalValue env m => F.UnaryOp -> FValue -> m FValue
evalUOp op v = do
    v' <- forceScalar v
    case op of
      F.Plus  -> MkFScalarValue <$> Op.opIcNumericUOpInplace "+" id     v'
      F.Minus -> MkFScalarValue <$> Op.opIcNumericUOpInplace "-" negate v'
      F.Not   -> -- TODO move this to Op (but logicals are a pain)
        case v' of
          FSVLogical (SomeFKinded bi) ->
            return $ MkFScalarValue $ FSVLogical $ SomeFKinded $ fLogicalNot bi
          _ -> err $ EOp $ Op.ETypeError "not" [fValueType v] "expected LOGICAL"
      _ -> err $ EUnsupported $ "operator: " <> show op

evalBOp'
    :: MonadEvalValue env m
    => F.Expression al -> F.Expression ar
    -> (a -> FScalarValue)
    -> (FScalarValue -> FScalarValue -> m a)
    -> m FValue
evalBOp' le re f g = do
    lv  <- evalExpr le
    rv  <- evalExpr re
    lv' <- forceScalar lv
    rv' <- forceScalar rv
    (MkFScalarValue . f) <$> g lv' rv'

-- | Evaluate explicit binary operators (ones denoted as such in the AST).
--
-- Note that this does not cover all binary operators -- there are many
-- intrinsics which use function syntax, but are otherwise binary operators.
evalBOp
    :: MonadEvalValue env m
    => F.BinaryOp -> F.Expression al -> F.Expression ar -> m FValue
evalBOp bop le re = do
    case bop of
      F.Addition       -> evalBOp' le re id $ Op.opIcNumericBOp (+)
      F.Subtraction    -> evalBOp' le re id $ Op.opIcNumericBOp (-)
      F.Multiplication -> evalBOp' le re id $ Op.opIcNumericBOp (*)

      -- TODO confirm correct operation (not checked much)
      F.Division ->
        evalBOp' le re id $ Op.opIcNumericBOpRealIntSep (div) (/)

      -- TODO not looked, certainly custom
      F.Exponentiation ->
        err $ EUnsupported "exponentiation"

      F.Concatenation  -> do
        lv  <- evalExpr le
        rv  <- evalExpr re
        lv' <- forceScalar lv
        rv' <- forceScalar rv
        case (lv', rv') of
          (FSVString ls, FSVString rs) ->
            pure $ MkFScalarValue $ FSVString $ concatSomeFString ls rs
          _ -> err $ EOpTypeError' "//" [fValueType lv, fValueType rv]

      F.GT  -> evalBOpBool le re $ Op.opIcNumRelBOp (>)
      F.GTE -> evalBOpBool le re $ Op.opIcNumRelBOp (>=)
      F.LT  -> evalBOpBool le re $ Op.opIcNumRelBOp (<)
      F.LTE -> evalBOpBool le re $ Op.opIcNumRelBOp (<=)
      F.NE  -> evalBOpBool le re $ Op.opIcNumRelBOp (/=)
      F.EQ  -> evalBOpBool le re $ Op.opEq

      -- @LOGICAL@-only operators
      F.Or  ->
        evalBOpLogicalShortCircuitable ".OR."  Op.someFLogicalBOpInplaceOr  id  le re
      F.And ->
        evalBOpLogicalShortCircuitable ".AND." Op.someFLogicalBOpInplaceAnd not le re
      F.Equivalent ->
        evalBOpBool le re $ Op.opIcLogicalBOp ".EQV." (==)
      F.NotEquivalent ->
        evalBOpBool le re $ Op.opIcLogicalBOp ".NEQV." (/=)
      F.XOr ->
        evalBOpBool le re $ Op.opIcLogicalBOp ".XOR." boolXor

      F.BinCustom{} -> -- TODO
        err $ EUnsupported "custom binary operators"

-- | Wrap a binary operation returning a 'Bool' into one that returns a default
--   @LOGICAL@.
--
-- TODO provide some spec refs. This is how they do it in Fortran.
evalBOpBool
    :: MonadEvalValue env m
    => F.Expression al -> F.Expression ar
    -> (FScalarValue -> FScalarValue -> m Bool)
    -> m FValue
evalBOpBool le re f = evalBOp' le re (FSVLogical . SomeFKinded . defFLogical) f

defFLogical :: Bool -> FInt 'FTInt4
defFLogical = FInt4 . fLogicalNumericFromBool

boolXor :: Bool -> Bool -> Bool
boolXor True  False = True
boolXor False True  = True
boolXor _     _     = False

evalFunctionCall :: MonadEvalValue env m => F.Name -> [F.Argument a] -> m FValue
evalFunctionCall fname aargs = do
    case fname of

      "ior"  -> do
        args <- traverse evalArg aargs
        args' <- forceArgs 2 args
        let [l, r] = args'
        l' <- forceScalar l
        r' <- forceScalar r
        evalIntrinsicIor l' r'

      "max"  -> do
        args <- traverse evalArg aargs
        evalIntrinsicMax args

      "char" -> do
        args <- traverse evalArg aargs
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt (SomeFKinded i) -> do
            -- TODO better error handling
            let c    = Data.Char.chr (fIntUOp fromIntegral i)
            pure $ MkFScalarValue $ FSVString $ someFString $ Text.singleton c
          _ ->
            err $ EOpTypeError $
                "char: expected INT(x), got "<>show (fScalarValueType v')

      "not"  -> do
        args <- traverse evalArg aargs
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt (SomeFKinded i) -> do
            pure $ MkFScalarValue $ FSVInt $ SomeFKinded $ fIntUOpInplace Data.Bits.complement i
          _ ->
            err $ EOpTypeError $
                "not: expected INT(x), got "<>show (fScalarValueType v')

      "int"  -> do
        -- TODO a real pain. just implementing common bits for now
        -- TODO gfortran actually performs some range checks for constants!
        -- @int(128, 1)@ errors with "this INT(4) is too big for INT(1)".

        (vArg, args') <- popArg aargs

        mKArg <- tryPopArg args'
        mK <- case mKArg of
               Nothing   -> pure Nothing
               Just kArg -> do
                 kVal <- evalArg kArg
                 Just <$> evalKpInt' kVal

        v'' <- evalArg vArg
        v' <- forceScalar v''
        MkFScalarValue . FSVInt <$> Op.opIcInt v' mK

      -- TODO all lies
      "int2" -> do
        args <- traverse evalArg aargs
        args' <- forceArgs 1 args
        let [v] = args'
        v' <- forceScalar v
        case v' of
          FSVInt{} ->
            pure $ MkFScalarValue v'
          FSVReal (SomeFKinded r) ->
            pure $ MkFScalarValue $ FSVInt $ SomeFKinded $ FInt2 $ fRealUOp truncate r
          _ ->
            err $ EOpTypeError $
                "int: unsupported or unimplemented type: "<>show (fScalarValueType v')

      _      -> err $ EUnsupported $ "function call: " <> fname

evalArg :: MonadEvalValue env m => F.Argument a -> m FValue
evalArg (F.Argument _ _ _ ae) =
    case ae of
      F.ArgExpr        e -> evalExpr e
      F.ArgExprVar _ _ v -> evalVar  v

--------------------------------------------------------------------------------

forceScalar :: MonadEvalValue env m => FValue -> m FScalarValue
forceScalar = \case
  MkFArrayValue{} -> err $ EUnsupported "no array values in eval for now thx"
  MkFScalarValue v' -> return v'

forceUnconsArg :: MonadEvalValue env m => [a] -> m (a, [a])
forceUnconsArg = \case
  []   -> err $ EOpTypeError "not enough arguments"
  a:as -> return (a, as)

-- TODO can I use vector-sized to improve safety here? lol
-- it's just convenience either way
forceArgs :: MonadEvalValue env m => Int -> [a] -> m [a]
forceArgs numArgs l =
    if   length l == numArgs
    then return l
    else err $ EOpTypeError $
            "expected "<>show numArgs<>" arguments; got "<>show (length l)

popArg :: MonadEvalValue env m => [a] -> m (a, [a])
popArg = \case
  [] -> err $ EOpTypeError "not enough args"
  arg:args -> pure (arg, args)

-- | Pop an optional value off the end of a list.
tryPopArg :: MonadEvalValue env m => [a] -> m (Maybe a)
tryPopArg = \case
  [] -> pure Nothing
  arg:_ -> pure $ Just arg

evalIntrinsicIor
    :: MonadEvalValue env m => FScalarValue -> FScalarValue -> m FValue
evalIntrinsicIor l r = (MkFScalarValue . FSVInt) <$> Op.opIor l r

-- https://gcc.gnu.org/onlinedocs/gfortran/MAX.html
-- TODO should support arrays! at least for >=F2010
evalIntrinsicMax
    :: MonadEvalValue env m => [FValue] -> m FValue
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
                vNewMax <- Op.opIcNumericBOp max vCurMax v
                go vNewMax vs
              _ ->
                err $ EOpTypeError $
                    "max: expected INT(x), got "<>show (fScalarValueType v)
          FSVReal{} ->
            case v of
              FSVReal{} -> do
                vNewMax <- Op.opIcNumericBOp max vCurMax v
                go vNewMax vs
              _ ->
                err $ EOpTypeError $
                    "max: expected REAL(x), got "<>show (fScalarValueType v)
          _ ->
            err $ EOpTypeError $
                "max: unsupported type: "<> show (fScalarValueType vCurMax)

-- | Helper for defining Fortran operations which may be short circuited. Note
--   that the actual short circuiting only takes place if configured.
evalBOpLogicalShortCircuitable
    :: MonadEvalValue env m
    => String
    -> (SomeFInt -> SomeFInt -> SomeFInt)
    -> (Bool -> Bool)
    -> F.Expression al -> F.Expression ar -> m FValue
evalBOpLogicalShortCircuitable op f g le re = do
    lv <- evalExpr le
    asks (cfgShortCircuit . getEvalValueCfg) >>= \case
      True ->
        case lv of
          MkFScalarValue (FSVLogical lb) ->
            if g (someFLogicalToBool lb) then
                pure lv
            else do
                rv <- evalExpr re
                case rv of
                  MkFScalarValue (FSVLogical rb) ->
                    pure $ MkFScalarValue $ FSVLogical $ f lb rb
                  _ -> err $ EOpTypeError' op [fValueType lv, fValueType rv]
          _ -> err $ EOpTypeError' op [fValueType lv]
      False -> do
        rv <- evalExpr re
        case (lv, rv) of
          (MkFScalarValue (FSVLogical lb), MkFScalarValue (FSVLogical rb)) ->
            pure $ MkFScalarValue $ FSVLogical $ f lb rb
          _ -> err $ EOpTypeError' op [fValueType lv, fValueType rv]

someFLogicalToBool :: SomeFInt -> Bool
someFLogicalToBool (SomeFKinded b) = fLogicalToBool b
