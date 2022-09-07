{-# LANGUAGE ConstraintKinds #-}

{- | Evaluate operations between values in the value representation.

Assorted notes:

  * Functions which return 'Bool's should return the default @LOGICAL@ kind.
    That should likely be @LOGICAL(4)@, which should be equivalent to
    @INTEGER(4)@.
  * F2018 doesn't have great explanation on how to convert between numeric
    types, but I assume I can use the @INT()@ etc. intrinsics.
-}

module Language.Fortran.Repr.Eval.Value.Op where

import Language.Fortran.Repr.Eval.Value.Op.Some

import Language.Fortran.Repr.Value.Machine
import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Value.Scalar.Machine
import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex
import Language.Fortran.Repr.Value.Scalar.Logical.Machine
import Language.Fortran.Repr.Value.Scalar.String
import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Type.Scalar.Real

import GHC.Float ( float2Double, double2Float )
import Data.Bits
import Control.Monad.Reader
import GHC.TypeNats

type OpName = String

class (HasEvalOpCfg env, MonadReader env m) => MonadEvalOp env m where
    err  :: Error -> m a
    warn :: String -> m ()

class HasEvalOpCfg env where
    getEvalOpCfg :: env -> Cfg

data Cfg = Cfg
  { cfgAllowOverflowingNumericConversions :: Bool
  -- ^ When converting between numeric types, allow conversions which would
  --   overflow. Downgrades the error to a warning. For example, you may receive
  --   this error when converting an @INTEGER(4)@ to an @INTEGER(1)@.
  --
  -- Comparable to gfortran's @-fno-range-check@ flag.

  } deriving stock (Show, Eq)

defCfg :: Cfg
defCfg = Cfg False

-- | Operation TODO
data Error
  = ETypeError
        OpName  -- ^ op name
        [FType] -- ^ types of terms sent
        String  -- ^ extra (e.g. allowed types)
  | EOther String
    deriving stock (Show, Eq)

eTypeError :: MonadEvalOp env m => String -> [FScalarValue] -> String -> m a
eTypeError opName termTypes extra =
    err $ ETypeError opName (map (fValueType . MkFScalarValue) termTypes) extra

eOther :: MonadEvalOp env m => String -> m a
eOther = err . EOther

opIcNumericBOp
    :: MonadEvalOp env m
    => (forall a. (Num a, Ord a) => a -> a -> a)
    -> FScalarValue -> FScalarValue -> m FScalarValue
opIcNumericBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = pure $ FSVInt $ someFIntBOpWrap bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        pure $ FSVReal $ someFRealUOpWrap (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = pure $ FSVReal $ someFRealBOpWrap bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        pure $ FSVComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

opIcNumericBOpRealIntSep
    :: MonadEvalOp env m
    => (forall a. Integral  a => a -> a -> a)
    -> (forall a. RealFloat a => a -> a -> a)
    -> FScalarValue -> FScalarValue -> m FScalarValue
opIcNumericBOpRealIntSep bopInt bopReal = go
  where
    go (FSVInt l) (FSVInt r) = pure $ FSVInt $ someFIntBOpWrap bopInt l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        pure $ FSVReal $ someFRealUOpWrap (\x -> withFInt l `bopReal` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = pure $ FSVReal $ someFRealBOpWrap bopReal l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        pure $ FSVComplex $ someFComplexBOpWrap bopReal (someFComplexFromReal l) r

opIcNumRelBOp
    :: MonadEvalOp env m
    => (forall a. Ord a => a -> a -> r)
    -> FScalarValue -> FScalarValue -> m r
opIcNumRelBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = pure $ someFIntBOp bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        pure $ someFRealUOp (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = pure $ someFRealBOp bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    -- TODO real complex
    go (FSVString l) (FSVString r) = pure $ someFStringBOp bop l r

-- plus, minus
opIcNumericUOpInplace
    :: MonadEvalOp env m
    => OpName -> (forall a. Num a => a -> a)
    -> FScalarValue -> m FScalarValue
opIcNumericUOpInplace uopName uop = \case
  FSVInt  (SomeFKinded v) -> pure $ FSVInt  $ SomeFKinded $ fIntUOpInplace  uop v
  FSVReal (SomeFKinded v) -> pure $ FSVReal $ SomeFKinded $ fRealUOpInplace uop v
  v -> eTypeError uopName [v] "expected INT, REAL"

-- | Type, kinding rules for logical intrinsic operators which only operate on
--   LOGICAL values.
--
-- See F2018 table 10.2. Should be used for and, or, eqv, neqv.
opIcLogicalBOp
    :: MonadEvalOp env m
    => OpName -> (Bool -> Bool -> r)
    -> FScalarValue -> FScalarValue -> m r
opIcLogicalBOp bopName bop = go
  where
    go (FSVLogical (SomeFKinded l)) (FSVLogical (SomeFKinded r)) =
        pure $ bop (fLogicalToBool l) (fLogicalToBool r)
    go l r = eTypeError bopName [l, r] "expected both LOGICAL"

-- | @.EQ.@ F2018 10.1.5 (pg.146)
--
-- Unique operator: overloaded, but always returns @LOGICAL@.
opEq :: MonadEvalOp env m => FScalarValue -> FScalarValue -> m Bool
opEq = go
  where
    go (FSVInt  l) (FSVInt  r) = pure $ someFIntBOp  (==) l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        pure $ someFRealUOp (\x -> withFInt l == x) r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)

    go (FSVReal l) (FSVReal r) = pure $ someFRealBOp (==) l r

    go (FSVString l) (FSVString r) = pure $ someFStringBOp (==) l r

    go (FSVComplex l) (FSVComplex r) = pure $ someFComplexBOp (==) (&&) l r
    --go _ _ = eOther "eq: unsupported"

someFLogicalBOpInplaceOr
    :: SomeFInt -> SomeFInt -> SomeFInt
someFLogicalBOpInplaceOr = someFIntBOpWrap' f f f f
  where
    f l r = consumeFLogicalNumeric 1 (consumeFLogicalNumeric 1 0 r) l

someFLogicalBOpInplaceAnd
    :: SomeFInt -> SomeFInt -> SomeFInt
someFLogicalBOpInplaceAnd = someFIntBOpWrap' f f f f
  where
    f l r = consumeFLogicalNumeric (consumeFLogicalNumeric 1 0 r) 0 l

opIor :: MonadEvalOp env m => FScalarValue -> FScalarValue -> m SomeFInt
opIor l'@(FSVInt (SomeFKinded l)) r'@(FSVInt (SomeFKinded r)) =
    case (l, r) of
      (FInt4{}, FInt4{}) -> do
        let out = opIor' l r
        pure $ SomeFKinded out
      (FInt8{}, FInt8{}) -> do
        let out = opIor' l r
        pure $ SomeFKinded out
      (FInt2{}, FInt2{}) -> do
        let out = opIor' l r
        pure $ SomeFKinded out
      (FInt1{}, FInt1{}) -> do
        let out = opIor' l r
        pure $ SomeFKinded out
      _ -> eTypeError "ior" [l', r'] "expected both INT same kind"
opIor l r = eTypeError "ior" [l, r] "expected both INT same kind"

-- | According to gfortran spec and F2010 spec, same kind required. (gfortran
--   complains if not same kind.)
opIor' :: FInt k -> FInt k -> FInt k
opIor' = fIntBOpInplace (.|.)

{-
opBOpNumeric
    :: MonadEvalOp env m
    => OpName
    -> (forall a. Integral  a => a -> a -> a)
    -> (forall a. RealFloat a => a -> a -> a)
    -> FScalarValue -> FScalarValue -> m FScalarValue
opBOpNumeric bopName bopInt bopReal = go
  where
    go (FSVInt l) (FSVInt r) = pure $ someFIntBOpWrap bopInt l r
    go (FSVReal l) (FSVReal r) = pure $ someFRealBOpWrap bopReal l r
    go (FSVComplex l) (FSVComplex r) = pure $ someFComplexBOpWrap bopReal l r

    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        pure $ someFRealUOpWrap' (\x -> withFInt l `bopReal` x) r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)

--    go (FSVInt (SomeFKinded l)) (FSVComplex c) =
--        pure $ someFRealUOpWrap (\x -> withFInt l `bopReal` x) r
    go (FSVComplex l) (FSVInt r) = go (FSVInt r) (FSVComplex l)
-}

opIcInt
    :: MonadEvalOp env m
    => FScalarValue -> Maybe FTInt -> m SomeFInt
opIcInt vIn mTyOut = case mTyOut of
  Nothing -> SomeFKinded <$> opIcIntK SFTInt4 vIn
  Just tyOut -> case tyOut of
    FTInt1 -> SomeFKinded <$> opIcIntK SFTInt1 vIn
    FTInt2 -> SomeFKinded <$> opIcIntK SFTInt2 vIn
    FTInt4 -> SomeFKinded <$> opIcIntK SFTInt4 vIn
    FTInt8 -> SomeFKinded <$> opIcIntK SFTInt8 vIn

-- TODO dense, skip lots of explicit pattern matching via singletons - explain
opIcIntK
    :: (MonadEvalOp env m, KnownNat (FTIntMin kout), KnownNat (FTIntMax kout))
    => SFTInt kout -> FScalarValue -> m (FInt kout)
opIcIntK tyOut vIn = case vIn of
  FSVInt  (SomeFKinded iIn) -> opIcIntKInt tyOut iIn

  -- TODO gfortran actually checks REALs too, in the same way it checks
  -- INTEGERs! I'm not sure how. I suppose we have to select a temporary
  -- representation, check, then convert.
  FSVReal (SomeFKinded rIn) -> pure $ toFInt tyOut $ fRealUOp truncate rIn

  -- F2018 16.9.100: @If A is of type complex, INT (A) = INT (REAL (A, KIND (A)))@
  -- (the kind is defaulted via 'Nothing')
  FSVComplex{} -> do
    r <- opIcReal vIn Nothing
    opIcIntK tyOut (FSVReal r)

  _ -> eTypeError "INT" [vIn] "expected INT, REAL or COMPLEX"

opIcIntKInt
    :: (MonadEvalOp env m, KnownNat (FTIntMin kout), KnownNat (FTIntMax kout))
    => SFTInt kout -> FInt kin -> m (FInt kout)
opIcIntKInt tyOut iIn =
    asks (cfgAllowOverflowingNumericConversions . getEvalOpCfg) >>= \case
      True ->  case mErr of Just msg -> warn (prepMsg msg) >> pure iOut
                            Nothing  ->                       pure iOut
      False -> case mErr of Just msg -> eOther $ prepMsg msg
                            Nothing  ->                       pure iOut
  where
    (iOut, mErr) = fIntConvertChecked tyOut iIn
    prepMsg m = "int: overflow: "<>m

-- | F2018 16.9.160
--
-- Note that since the rules differ between types, we can't really write this in
-- any smaller chunks.
opIcReal
    :: MonadEvalOp env m
    => FScalarValue -> Maybe FTReal -> m SomeFReal
opIcReal vIn mTyOut = case vIn of

  -- TODO not doing pattern matching here, simplifying instead.
  FSVInt (SomeFKinded i) -> case mTyOut of
    Nothing -> pure $ SomeFKinded $ FReal4 $ withFInt i
    Just tyOut -> case tyOut of
      FTReal4 -> pure $ SomeFKinded $ FReal4 $ withFInt i
      FTReal8 -> pure $ SomeFKinded $ FReal8 $ withFInt i

  FSVReal (SomeFKinded r) -> case mTyOut of
    Nothing -> case r of
      FReal4 _r' -> pure $ SomeFKinded r
      FReal8  r' -> pure $ SomeFKinded $ FReal4 $ double2Float r'
    Just tyOut -> case tyOut of
      FTReal4 -> case r of
        FReal4 _r' -> pure $ SomeFKinded r
        FReal8  r' -> pure $ SomeFKinded $ FReal4 $ double2Float r'
      FTReal8 -> case r of
        FReal4  r' -> pure $ SomeFKinded $ FReal8 $ float2Double r'
        FReal8 _r' -> pure $ SomeFKinded r

  FSVComplex (SomeFKinded c) -> case mTyOut of
    Nothing -> case c of
      FComplex8  cr _ci -> pure $ SomeFKinded $ FReal4 cr
      FComplex16 cr _ci -> pure $ SomeFKinded $ FReal8 cr
    Just tyOut -> case tyOut of
      FTReal4 -> case c of
        FComplex8  cr _ci -> pure $ SomeFKinded $ FReal4                cr
        FComplex16 cr _ci -> pure $ SomeFKinded $ FReal4 $ double2Float cr
      FTReal8 -> case c of
        FComplex8  cr _ci -> pure $ SomeFKinded $ FReal8 $ float2Double cr
        FComplex16 cr _ci -> pure $ SomeFKinded $ FReal8                cr

  _ -> eTypeError "REAL" [vIn] "expected INT, REAL or COMPLEX"
