-- | Evaluate operations between values in the value representation.

module Language.Fortran.Repr.Eval.Value.Op where

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
opIcDble :: FScalarValue -> Either Error FReal
opIcDble = \case
  FSVComplex c -> case c of
    FComplex8  r _i -> rfr8 $ float2Double r
    FComplex16 r _i -> rfr8 r
  FSVReal r -> case r of
    FReal4 r'   -> rfr8 $ float2Double r'
    FReal8 _r'  -> Right r
  FSVInt i -> rfr8 $ withFInt i
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
    :: (forall a. (Num a, Ord a) => a -> a -> a)
    -> FScalarValue -> FScalarValue -> Either Error FScalarValue
opIcNumericBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ FSVInt $ fIntBOpInplace bop l r
    go (FSVInt l) (FSVReal r) =
        Right $ FSVReal $ fRealUOpInplace (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ FSVReal $ fRealBOpInplace bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        Right $ FSVComplex $ fComplexBOpInplace bop (fComplexFromReal l) r

opIcNumericBOpRealIntSep
    :: (forall a. Integral  a => a -> a -> a)
    -> (forall a. RealFloat a => a -> a -> a)
    -> FScalarValue -> FScalarValue -> Either Error FScalarValue
opIcNumericBOpRealIntSep bopInt bopReal = go
  where
    go (FSVInt l) (FSVInt r) = Right $ FSVInt $ fIntBOpInplace bopInt l r
    go (FSVInt l) (FSVReal r) =
        Right $ FSVReal $ fRealUOpInplace (\x -> withFInt l `bopReal` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ FSVReal $ fRealBOpInplace bopReal l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    go (FSVReal l) (FSVComplex r) =
        Right $ FSVComplex $ fComplexBOpInplace bopReal (fComplexFromReal l) r

opIcNumRelBOp
    :: (forall a. Ord a => a -> a -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcNumRelBOp bop = go
  where
    go (FSVInt l) (FSVInt r) = Right $ fIntBOp bop l r
    go (FSVInt l) (FSVReal r) =
        Right $ fRealUOp (\x -> withFInt l `bop` x) r
    -- TODO int complex
    go (FSVReal l) (FSVReal r) = Right $ fRealBOp bop l r
    go (FSVReal l) (FSVInt r) = go (FSVInt r) (FSVReal l)
    -- TODO real complex
    go (FSVString l) (FSVString r) = Right $ l `bop` r

-- plus, minus
opIcNumericUOpInplace
    :: (forall a. Num a => a -> a)
    -> FScalarValue -> Either Error FScalarValue
opIcNumericUOpInplace uop = \case
  FSVInt  v -> Right $ FSVInt  $ fIntUOpInplace  uop v
  FSVReal v -> Right $ FSVReal $ fRealUOpInplace uop v
  v -> eBadArgType1 ["INT", "REAL"] v

-- and, or, eqv, neqv
opIcLogicalBOp
    :: (Bool -> Bool -> r)
    -> FScalarValue -> FScalarValue -> Either Error r
opIcLogicalBOp bop = go
  where
    go (FSVLogical l) (FSVLogical r) =
        Right $ bop (fLogicalToBool l) (fLogicalToBool r)
    go l r = eBadArgType2 ["LOGICAL"] l r

opEq :: FScalarValue -> FScalarValue -> Either Error Bool
opEq = go
  where
    go (FSVInt  l) (FSVInt  r) = Right $ fIntBOp  (==) l r
    go (FSVReal l) (FSVReal r) = Right $ fRealBOp (==) l r
    go (FSVInt i) (FSVReal r) =
        Right $ fRealUOp (\x -> withFInt i == x) r
    go (FSVReal r) (FSVInt i) =
        Right $ fRealUOp (\x -> withFInt i == x) r
    go (FSVString l) (FSVString r) = Right $ l == r

-- | According to gfortran spec and F2010 spec, same kind required.
opIor' :: FInt -> FInt -> FInt
opIor' = fIntBOpInplace (.|.)

opIor :: FInt -> FInt -> Either Error FInt
opIor l r =
    case (l, r) of
      (FInt4{}, FInt4{}) -> Right $ opIor' l r
      (FInt8{}, FInt8{}) -> Right $ opIor' l r
      (FInt2{}, FInt2{}) -> Right $ opIor' l r
      (FInt1{}, FInt1{}) -> Right $ opIor' l r
      _ -> Left $ EGeneric "bad args to ior"
