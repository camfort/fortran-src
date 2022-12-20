{-# LANGUAGE UndecidableInstances #-}
-- | Common definitions for Fortran scalar representations.
module Language.Fortran.Repr.Value.Scalar.Common where

import Data.Singletons

{- | Convenience wrapper which multiple Fortran tag-kinded intrinsic types fit.

A type @ft@ takes some type @fk@ of kind @k@, and we are permitted to move the
type between the term and type levels using the included singleton instances.

For example, integers are kinded with type level @FTInt@s. So we can define an
integer with an existential ("unknown") kind with the type @'SomeFKinded' FTInt
FInt@. By pattern matching on it, we recover the hidden kind tag (as well as
obtaining the value).
-}
data SomeFKinded k ft = forall (fk :: k). (SingKind k, SingI fk, Eq (ft fk), Ord (ft fk)) => SomeFKinded (ft fk)

instance {-# OVERLAPPABLE#-} Eq (Demote k) => Eq (SomeFKinded k ft) where
  x == y = someFKindedKind x == someFKindedKind y

instance {-# OVERLAPPABLE#-} Ord (Demote k) => Ord (SomeFKinded k ft) where
  x <= y = someFKindedKind x <= someFKindedKind y


-- | Recover some @TYPE(x)@'s kind (the @x@).
someFKindedKind :: SomeFKinded k ft -> Demote k
someFKindedKind (SomeFKinded (_ :: ft fk)) = demote @fk
