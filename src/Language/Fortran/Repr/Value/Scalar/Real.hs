module Language.Fortran.Repr.Value.Scalar.Real
  ( FReal(..)
  , SomeFReal

  , fRealUOp
  , fRealUOp'
  , fRealUOpInplace
  , fRealUOpInplace'
  , fRealUOpInternal

  , fRealBOp
  , fRealBOp'
  , fRealBOpInplace
  , fRealBOpInplace'
  , fRealBOpInternal
  ) where

import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Common
import GHC.Float ( float2Double )
import Data.Functor.Const

data FReal (k :: FTReal) where
    FReal4 :: Float  -> FReal 'FTReal4
    FReal8 :: Double -> FReal 'FTReal8
deriving stock instance Show (FReal k)
deriving stock instance Eq   (FReal k)
deriving stock instance Ord  (FReal k)

fRealUOpInternal
    :: (Float  -> ft 'FTReal4)
    -> (Double -> ft 'FTReal8)
    -> FReal k -> ft k
fRealUOpInternal k4f k8f = \case
  FReal4 fl -> k4f fl
  FReal8 db -> k8f db

-- | Run an operation over some 'FReal', with a concrete function for each kind.
fRealUOp'
    :: (Float  -> r)
    -> (Double -> r)
    -> FReal k -> r
fRealUOp' k4f k8f = getConst . fRealUOpInternal (Const . k4f) (Const . k8f)

-- | Run an operation over some 'FReal'.
fRealUOp
    :: (forall a. RealFloat a => a -> r)
    -> FReal k -> r
fRealUOp f = fRealUOp' f f

-- | Run an inplace operation over some 'FReal', with a concrete function for
--   each kind.
fRealUOpInplace'
    :: (Float  -> Float)
    -> (Double -> Double)
    -> FReal k -> FReal k
fRealUOpInplace' k4f k8f = fRealUOpInternal (FReal4 . k4f) (FReal8. k8f)

-- | Run an inplace operation over some 'FReal'.
fRealUOpInplace
    :: (forall a. RealFloat a => a -> a)
    -> FReal k -> FReal k
fRealUOpInplace f = fRealUOpInplace' f f

-- | Combine two Fortran reals with a binary operation, coercing different
--   kinds.
fRealBOpInternal
    :: (Float  -> Float  -> ft 'FTReal4)
    -> (Double -> Double -> ft 'FTReal8)
    -> FReal kl -> FReal kr -> ft (FTRealCombine kl kr)
fRealBOpInternal k4f k8f l r = case (l, r) of
  (FReal4 lr, FReal4 rr) -> k4f lr rr
  (FReal8 lr, FReal8 rr) -> k8f lr rr
  (FReal4 lr, FReal8 rr) -> k8f (float2Double lr) rr
  (FReal8 lr, FReal4 rr) -> k8f lr (float2Double rr)

fRealBOp'
    :: (Float  -> Float  -> r)
    -> (Double -> Double -> r)
    -> FReal kl -> FReal kr -> r
fRealBOp' k4f k8f l r = getConst $ fRealBOpInternal (go k4f) (go k8f) l r
  where go g l r = Const $ g l r

fRealBOp
    :: (forall a. RealFloat a => a -> a -> r)
    -> FReal kl -> FReal kr -> r
fRealBOp f = fRealBOp' f f

fRealBOpInplace'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> FReal kl -> FReal kr -> FReal (FTRealCombine kl kr)
fRealBOpInplace' k4f k8f = fRealBOpInternal (go FReal4 k4f) (go FReal8 k8f)
  where go f g l r = f $ g l r

fRealBOpInplace
    :: (forall a. RealFloat a => a -> a -> a)
    -> FReal kl -> FReal kr -> FReal (FTRealCombine kl kr)
fRealBOpInplace f = fRealBOpInplace' f f

type SomeFReal = SomeFKinded FTReal FReal
deriving stock instance Show SomeFReal
instance Eq  SomeFReal where
    (SomeFKinded l) == (SomeFKinded r) = fRealBOp (==) l r
instance Ord SomeFReal where
    compare (SomeFKinded l) (SomeFKinded r) = fRealBOp compare l r
