module Language.Fortran.Repr.Eval.Value.Op.Some where

import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex

import Data.Int

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
    :: (forall a. IsFInt a => a -> r)
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
    :: (forall a. IsFInt a => a -> a)
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
    :: (forall a. IsFInt a => a -> a -> r)
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
    :: (forall a. IsFInt a => a -> a -> a)
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
