{- | Idealized Fortran INTEGER values.

This module stores Fortran INTEGER values in a Haskell 'Integer', together with
a phantom type describing the Fortran kind. This way, we can safely check for
bounds issues, and leave exact behaviour up to the user.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- just for better inference (maybe)
{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Value.Scalar.Int.Idealized where

import Language.Fortran.Repr.Type.Scalar.Int
import Data.Kind
import Data.Int

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

type FIntMRep :: FTInt -> Type
type family FIntMRep k = r | r -> k where
    FIntMRep 'FTInt1 = Int8
    FIntMRep 'FTInt2 = Int16
    FIntMRep 'FTInt4 = Int32
    FIntMRep 'FTInt8 = Int64

newtype FIntI (k :: FTInt) = FIntI Integer
    deriving stock (Show, Generic, Data)
    deriving (Eq, Ord) via Integer
    deriving anyclass (Binary, Out)

fIntICheckBounds
    :: forall k rep. (rep ~ FIntMRep k, Bounded rep, Integral rep)
    => FIntI k -> Maybe String
fIntICheckBounds (FIntI i) =
    if   i > fromIntegral (maxBound @rep)
    then Just "TODO too large"
    else if 　i < fromIntegral (minBound @rep)
         then Just "TODO too small"
         else Nothing

data SomeFIntI = forall fk. SomeFIntI (FIntI fk)
deriving stock instance Show SomeFIntI
instance Eq SomeFIntI where
    (SomeFIntI (FIntI l)) == (SomeFIntI (FIntI r)) = l == r

-- this might look silly, but it's because even if we don't do kinded
-- calculations, we must still kind the output
someFIntIBOpWrap
    :: (Integer -> Integer -> Integer)
    -> SomeFIntI -> SomeFIntI -> SomeFIntI
someFIntIBOpWrap f (SomeFIntI (FIntI li :: FIntI lfk)) (SomeFIntI (FIntI ri :: FIntI rfk)) =
    SomeFIntI $ FIntI @(FTIntCombine lfk rfk) $ f li ri

{-
fIntIBOpWrap
    :: forall kl kr. (Integer -> Integer -> Integer)
    -> FIntI kl -> FIntI kr -> FIntI (FTIntCombine kl kr)
fIntIBOpWrap f l r =
    case (l, r) of
      (FIntI il :: FIntI 'FTInt16, FIntI ir) -> FIntI @'FTInt16 $ f il ir

    {-
      (FIntI l) (FIntI r) =
    case (demote @kl, demote @kr) of
      (FTInt16, _) -> FIntI @'FTInt16 x
      (_, FTInt16) -> FIntI @'FTInt16 x
      (FTInt8, _)  -> FIntI @'FTInt8 x
      (_, FTInt8)  -> FIntI @'FTInt8 x
      (FTInt4, _)  -> FIntI @'FTInt4 x
      (_, FTInt4)  -> FIntI @'FTInt4 x
      (FTInt2, _)  -> FIntI @'FTInt2 x
      (_, FTInt2)  -> FIntI @'FTInt2 x
      (FTInt1, FTInt1) -> FIntI @'FTInt1 x
      -}
-}
