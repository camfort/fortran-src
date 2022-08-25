-- | Evaluate operations between values in the value representation.

module Language.Fortran.Repr.Eval.Value.Op where

import Language.Fortran.Repr.Eval.Value.Op.Some

import Language.Fortran.Repr.Value.Scalar.Machine
import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex
import Language.Fortran.Repr.Value.Scalar.Logical.Machine
import Language.Fortran.Repr.Value.Scalar.String
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Scalar.Real
import GHC.Float ( float2Double )
import Data.Int

import Data.Bits

import Data.Singletons

-- | Operation TODO
data Error
  = EBadArgType1 [String] FScalarType
  | EBadArgType2 [String] FScalarType FScalarType
  | EGeneric String
    deriving stock (Show, Eq)

-- https://gcc.gnu.org/onlinedocs/gfortran/DBLE.html#DBLE
opIcDble :: FScalarValue -> Either Error (FReal 'FTReal8)
opIcDble = \case
  FSVComplex (SomeFKinded c) -> case c of
    FComplex8  r _i -> rfr8 $ float2Double r
    FComplex16 r _i -> rfr8 r
  FSVReal (SomeFKinded r) -> case r of
    FReal4 r'   -> rfr8 $ float2Double r'
    FReal8 _r'  -> Right r
  FSVInt (SomeFKinded i) -> rfr8 $ withFInt i
  v -> eBadArgType1 ["COMPLEX", "REAL", "INT"] v
  where rfr8 = Right . FReal8

eBadArgType1 :: [String] -> FScalarValue -> Either Error a
eBadArgType1 expected = Left . EBadArgType1 expected . fScalarValueType

eBadArgType2 :: [String] -> FScalarValue -> FScalarValue -> Either Error a
eBadArgType2 expected l r =
    Left $ EBadArgType2 expected (fScalarValueType l) (fScalarValueType r)

eGeneric :: String -> Either Error a
eGeneric = Left . EGeneric

opIcNumericBOp
    :: (forall a. Num a => a -> a -> a)
    -> FScalarValue -> FScalarValue -> Either Error FScalarValue
opIcNumericBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ FSVInt $ someFIntBOpWrap bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        Right $ FSVReal $ someFRealUOpWrap (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ FSVReal $ someFRealBOpWrap bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        Right $ FSVComplex $ someFComplexBOpWrap bop (someFComplexFromReal l) r

opIcNumRelBOp
    :: (forall a. Ord a => a -> a -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcNumRelBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ someFIntBOp bop l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        Right $ someFRealUOp (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ someFRealBOp bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    -- TODO real complex
    go (FSVString l) (FSVString r) = Right $ someFStringBOp bop l r

-- plus, minus
opIcNumericUOpInplace
    :: (forall a. Num a => a -> a)
    -> FScalarValue -> Either Error FScalarValue
opIcNumericUOpInplace uop = \case
  FSVInt  (SomeFKinded v) -> Right $ FSVInt  $ SomeFKinded $ fIntUOpInplace  uop v
  FSVReal (SomeFKinded v) -> Right $ FSVReal $ SomeFKinded $ fRealUOpInplace uop v
  v -> eBadArgType1 ["INT", "REAL"] v

-- and, or, eqv, neqv
opIcLogicalBOp
    :: (Bool -> Bool -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcLogicalBOp bop = go
  where
    go (FSVLogical (SomeFKinded l)) (FSVLogical (SomeFKinded r)) =
        Right $ bop (fLogicalToBool l) (fLogicalToBool r)
    go l r = eBadArgType2 ["LOGICAL"] l r

opEq :: FScalarValue -> FScalarValue -> Either Error Bool
opEq = go
  where
    go (FSVInt  l) (FSVInt  r) = Right $ someFIntBOp  (==) l r
    go (FSVReal l) (FSVReal r) = Right $ someFRealBOp (==) l r
    go (FSVInt (SomeFKinded l)) (FSVReal r) =
        Right $ someFRealUOp (\x -> withFInt l == x) r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVString l) (FSVString r) = Right $ someFStringBOp (==) l r

-- | According to gfortran spec and F2010 spec, same kind required.
opIor' :: FInt k -> FInt k -> FInt k
opIor' = fIntBOpInplace (.|.)

opIor :: FScalarValue -> FScalarValue -> Either Error SomeFInt
opIor (FSVInt (SomeFKinded l)) (FSVInt (SomeFKinded r)) =
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
opIor l r = eBadArgType2 ["INT", "INT"] l r
