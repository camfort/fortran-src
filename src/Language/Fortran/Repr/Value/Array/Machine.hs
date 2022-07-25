{- | Fortran array representation primitives.

Fortran arrays are homogeneous: every element has the same type. That means a
@REAL(8)@ array must store only @REAL(8)@s, no @REAL(4)@s. We use some type
algebra to obtain correct-by-construction types.

Also, Fortran arrays are multi-dimensional. Rather than storing a single natural
number for a single dimension, or doing some recursive work for arrays of arrays
(which don't exist in Fortran), we instead store a list of naturals, defining
each dimension's extent.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- unwrapping somes gets you kind and length at the same time - I figure because
-- kind is just a convenience.

module Language.Fortran.Repr.Value.Array.Machine where

import Language.Fortran.Repr.Type.Array
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex
import Language.Fortran.Repr.Value.Scalar.String
import Language.Fortran.Repr.Util ( natVal'' )

import qualified Data.Vector.Sized as V
import Data.Vector.Sized ( Vector )
import GHC.TypeNats
import Data.Kind
import Data.Singletons

type Size :: [Natural] -> Natural
type family Size dims where
    Size (dim ': dims) = dim + Size dims
    Size '[]           = 0

-- can conveniently define kinded array types like so
data FVA (ft :: k -> Type) (fk :: k) (dims :: [Natural])
  = FVA { unFVA :: Vector (Size dims) (ft fk) }
deriving stock instance Show (ft fk) => Show (FVA ft fk dims)

-- makes rank 1 array
mkFVA1 :: forall l ft fk. Vector l (ft fk) -> FVA ft fk '[l]
mkFVA1 = FVA

-- reifies type info
fvaShape :: forall dims ft fk. KnownNats dims => FVA ft fk dims -> Shape
fvaShape _ = Shape $ natVals @dims

-- TODO
mkSomeFVA :: (forall l. KnownNat l => Vector l a -> r) -> [a] -> r
mkSomeFVA f as = V.withSizedList as f

-- | Reify a list of type-level 'Natural's.
class KnownNats (ns :: [Natural]) where natVals :: [Natural]
instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natVals = natVal'' @n : natVals @ns
instance KnownNats '[] where natVals = []

-- | Wrapper for defining an array of a kind-tagged Fortran type.
data SomeFVA k ft =
    forall (fk :: k) (dims :: [Natural]). (KnownNats dims, SingKind k, SingI fk)
        => SomeFVA { unSomeFVA :: FVA ft fk dims }
deriving stock instance Show (SomeFVA FTInt   FInt)
deriving stock instance Show (SomeFVA FTReal  FReal)
deriving stock instance Show (SomeFVA FTReal  FComplex)
deriving stock instance Show (SomeFVA Natural FString)

someFVAKind :: SomeFVA k ft -> Demote k
someFVAKind (SomeFVA (_ :: FVA ft fk dims)) = demote @fk

someFVAShape :: SomeFVA k ft -> Shape
someFVAShape (SomeFVA a) = fvaShape a

-- makes rank 1 array
mkSomeFVA1
    :: forall k ft (fk :: k). (SingKind k, SingI fk)
    => [ft fk] -> SomeFVA k ft
mkSomeFVA1 = mkSomeFVA $ SomeFVA . mkFVA1

data FArrayValue
  = FAVInt     (SomeFVA FTInt   FInt)
  | FAVReal    (SomeFVA FTReal  FReal)
  | FAVComplex (SomeFVA FTReal  FComplex)
  | FAVLogical (SomeFVA FTInt   FInt)
  | FAVString  (SomeFVA Natural FString)
deriving stock instance Show FArrayValue

fArrayValueType :: FArrayValue -> FArrayType
fArrayValueType = \case
  FAVInt     a -> go FSTInt     a
  FAVReal    a -> go FSTReal    a
  FAVComplex a -> go FSTComplex a
  FAVLogical a -> go FSTLogical a
  FAVString  a -> go FSTString  a
  where
    go :: (Demote k -> FScalarType) -> SomeFVA k ft -> FArrayType
    go f a = FArrayType (f (someFVAKind a)) (someFVAShape a)
