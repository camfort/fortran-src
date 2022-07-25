{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- just for better inference (maybe)

module Language.Fortran.Repr.Value.Scalar.Int.Idealized where

import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Common
import Data.Kind
import Data.Int
import Data.Singletons

type FIntMRep :: FTInt -> Type
type family FIntMRep k = r | r -> k where
    FIntMRep 'FTInt1 = Int8
    FIntMRep 'FTInt2 = Int16
    FIntMRep 'FTInt4 = Int32
    FIntMRep 'FTInt8 = Int64

newtype FIntI (k :: FTInt) = FIntI Integer
    deriving (Show, Eq, Ord) via Integer

fIntICheckBounds
    :: forall k rep. (rep ~ FIntMRep k, Bounded rep, Integral rep)
    => FIntI k -> Maybe String
fIntICheckBounds (FIntI i) =
    if   i > fromIntegral (maxBound @rep)
    then Just "TODO too large"
    else if 　i < fromIntegral (minBound @rep)
         then Just "TODO too small"
         else Nothing

type SomeFIntI = SomeFKinded FTInt FIntI
deriving stock instance Show SomeFIntI
instance Eq SomeFIntI where
    (SomeFKinded (FIntI l)) == (SomeFKinded (FIntI r)) = l == r

-- this might look silly, but it's because even if we don't do kinded
-- calculations, we must still kind the output
someFIntIBOpWrap
    :: (Integer -> Integer -> Integer)
    -> SomeFIntI -> SomeFIntI -> SomeFIntI
someFIntIBOpWrap f l@(SomeFKinded (FIntI il)) r@(SomeFKinded (FIntI ir)) =
    case (someFKindedKind l, someFKindedKind r) of
      (FTInt16, _) -> as @'FTInt16
      (_, FTInt16) -> as @'FTInt16
      (FTInt8, _) -> as @'FTInt8
      (_, FTInt8) -> as @'FTInt8
      (FTInt4, _) -> as @'FTInt4
      (_, FTInt4) -> as @'FTInt4
      (FTInt2, _) -> as @'FTInt2
      (_, FTInt2) -> as @'FTInt2
      (FTInt1, FTInt1) -> as @'FTInt1
  where
    x = f il ir
    as :: forall (k :: FTInt). SingI k => SomeFIntI
    as = SomeFKinded $ FIntI @k x

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
