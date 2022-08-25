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

-- | Operation TODO
data Error
  = EBadArgType1 [String] FScalarType
  | EBadArgType2 [String] FScalarType FScalarType
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

--------------------------------------------------------------------------------

someFIntUOpInplace'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> SomeFInt -> SomeFInt
someFIntUOpInplace' k1f k2f k4f k8f (SomeFKinded i) = SomeFKinded $
    fIntUOpInplace' k1f k2f k4f k8f i

someFIntUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> SomeFInt -> r
someFIntUOp' k1f k2f k4f k8f (SomeFKinded i) =
    fIntUOp' k1f k2f k4f k8f i

someFIntUOp
    :: (forall a. Integral a => a -> r)
    -> SomeFInt -> r
someFIntUOp f = someFIntUOp' f f f f

someFIntUOpWrap'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> SomeFInt -> SomeFInt
someFIntUOpWrap' k1f  k2f  k4f  k8f  (SomeFKinded i) =
    fIntUOp'     k1f' k2f' k4f' k8f' i
  where
    k1f' = SomeFKinded . FInt1 . k1f
    k2f' = SomeFKinded . FInt2 . k2f
    k4f' = SomeFKinded . FInt4 . k4f
    k8f' = SomeFKinded . FInt8 . k8f

someFIntUOpWrap
    :: (forall a. Integral a => a -> a)
    -> SomeFInt -> SomeFInt
someFIntUOpWrap f = someFIntUOpWrap' f f f f

someFIntBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> SomeFInt -> SomeFInt -> r
someFIntBOp' k1f k2f k4f k8f (SomeFKinded il) (SomeFKinded ir) =
    fIntBOp' k1f k2f k4f k8f il            ir

someFIntBOp
    :: (forall a. Integral a => a -> a -> r)
    -> SomeFInt -> SomeFInt -> r
someFIntBOp f = someFIntBOp' f f f f

someFIntBOpWrap'
    :: (Int8  -> Int8  -> Int8)
    -> (Int16 -> Int16 -> Int16)
    -> (Int32 -> Int32 -> Int32)
    -> (Int64 -> Int64 -> Int64)
    -> SomeFInt -> SomeFInt -> SomeFInt
someFIntBOpWrap' k1f  k2f  k4f  k8f =
    someFIntBOp' k1f' k2f' k4f' k8f'
  where
    k1f' l r = SomeFKinded $ FInt1 $ k1f l r
    k2f' l r = SomeFKinded $ FInt2 $ k2f l r
    k4f' l r = SomeFKinded $ FInt4 $ k4f l r
    k8f' l r = SomeFKinded $ FInt8 $ k8f l r

someFIntBOpWrap
    :: (forall a. Integral a => a -> a -> a)
    -> SomeFInt -> SomeFInt -> SomeFInt
someFIntBOpWrap f = someFIntBOpWrap' f f f f

--------------------------------------------------------------------------------

someFRealBOp'
    :: (Float  -> Float  -> r)
    -> (Double -> Double -> r)
    -> SomeFReal -> SomeFReal -> r
someFRealBOp' k4f k8f (SomeFKinded l) (SomeFKinded r) =
    fRealBOp' k4f k8f l             r

someFRealBOp
    :: (forall a. RealFloat a => a -> a -> r)
    -> SomeFReal -> SomeFReal -> r
someFRealBOp f = someFRealBOp' f f

someFRealBOpWrap'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> SomeFReal -> SomeFReal -> SomeFReal
someFRealBOpWrap' k4f  k8f =
    someFRealBOp' k4f' k8f'
  where
    k4f' l r = SomeFKinded $ FReal4 $ k4f l r
    k8f' l r = SomeFKinded $ FReal8 $ k8f l r

someFRealBOpWrap
    :: (forall a. RealFloat a => a -> a -> a)
    -> SomeFReal -> SomeFReal -> SomeFReal
someFRealBOpWrap f = someFRealBOpWrap' f f

someFRealUOp'
    :: (Float  -> r)
    -> (Double -> r)
    -> SomeFReal -> r
someFRealUOp' k4f k8f (SomeFKinded x) =
    fRealUOp' k4f k8f x

someFRealUOp
    :: (forall a. RealFloat a => a -> r)
    -> SomeFReal -> r
someFRealUOp f = someFRealUOp' f f

someFRealUOpWrap'
    :: (Float  -> Float)
    -> (Double -> Double)
    -> SomeFReal -> SomeFReal
someFRealUOpWrap' k4f  k8f =
    someFRealUOp' k4f' k8f'
  where
    k4f' = SomeFKinded . FReal4 . k4f
    k8f' = SomeFKinded . FReal8 . k8f

someFRealUOpWrap
    :: (forall a. RealFloat a => a -> a)
    -> SomeFReal -> SomeFReal
someFRealUOpWrap f = someFRealUOpWrap' f f

--------------------------------------------------------------------------------

someFComplexBOp'
    :: (Float  -> Float  -> a)
    -> (a -> a -> r)
    -> (Double -> Double -> b)
    -> (b -> b -> r)
    -> SomeFComplex -> SomeFComplex -> r
someFComplexBOp' k8f k8g k16f k16g (SomeFKinded l) (SomeFKinded r) =
    fComplexBOp' k8f k8g k16f k16g l                r

someFComplexBOp
    :: (forall a. RealFloat a => a -> a -> b)
    -> (b -> b -> r)
    -> SomeFComplex -> SomeFComplex -> r
someFComplexBOp f g = someFComplexBOp' f g f g

someFComplexBOpWrap'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> SomeFComplex -> SomeFComplex -> SomeFComplex
someFComplexBOpWrap' k8f     k16f =
    someFComplexBOp' k8f k8g k16f k16g
  where
    k8g  l r = SomeFKinded $ FComplex8  l r
    k16g l r = SomeFKinded $ FComplex16 l r

someFComplexBOpWrap
    :: (forall a. RealFloat a => a -> a -> a)
    -> SomeFComplex -> SomeFComplex -> SomeFComplex
someFComplexBOpWrap f = someFComplexBOpWrap' f f

someFComplexFromReal :: SomeFReal -> SomeFComplex
someFComplexFromReal (SomeFKinded r) =
    case r of
      FReal4 x -> SomeFKinded $ FComplex8  x 0.0
      FReal8 x -> SomeFKinded $ FComplex16 x 0.0
