{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common definitions for Fortran scalar representations.
module Language.Fortran.Repr.Value.Scalar.Common where

import Language.Fortran.Repr.Type.Scalar.Common

import Data.Singletons

import Text.PrettyPrint.GenericPretty ( Out )
import Text.PrettyPrint.GenericPretty.ViaShow ( OutShowly(..) )
import Data.Binary
import Data.Data ( Data, Typeable )

import Data.Kind

{- | Convenience wrapper which multiple Fortran tag-kinded intrinsic types fit.

A type @ft@ takes some type @fk@ of kind @k@, and we are permitted to move the
type between the term and type levels using the included singleton instances.

For example, integers are kinded with type level @FTInt@s. So we can define an
integer with an existential ("unknown") kind with the type @'SomeFKinded' FTInt
FInt@. By pattern matching on it, we recover the hidden kind tag (as well as
obtaining the value).

Note that many type classes usually derived generically (e.g.
'Data.Binary.Binary') instances should be manually derived on this wrapper type.
TODO give a better explanation why?
-}
data SomeFKinded k ft where
    SomeFKinded
        :: forall {k} ft (fk :: k)
        .  (SingKind k, SingI fk, Data (ft fk))
        => ft fk
        -> SomeFKinded k ft

deriving stock instance
  ( SingKind k
  , forall (fk :: k). SingI fk
  , forall (fk :: k). Data (ft fk)
  , Typeable ft
  , Typeable k
  ) => Data (SomeFKinded k ft)
--instance (Typeable k, Typeable ft) => Data (SomeFKinded k ft) where

-- | GHC can derive stock 'Show' instances given some @QuantifiedConstraints@
--   guarantees (wow!).
deriving stock instance (forall fk. Show (ft fk)) => Show (SomeFKinded k ft)

-- | Derive 'Out' instances via 'Show'.
deriving via OutShowly (SomeFKinded k ft) instance (forall fk. Show (ft fk)) => Out (SomeFKinded k ft)

-- | For any Fortran type @ft@ kinded with @k@, we may derive a 'Binary'
--   instance by leveraging the kind tag's instance @'Binary' ('Demote' k)@ and
--   the kinded value's instance @'Binary' (ft k)@. (We also have to ferry some
--   singletons instances through.)
--
-- WARNING: This instance is only sound for types where each kind tag value is
-- used once at most (meaning if you know the fkind, you know the constructor).
--
-- Note that the 'Data.Binary.Get' instance works by parsing a kind tag,
-- promoting it to a singleton, then gleaning type information and using that to
-- parse the inner kinded value. Dependent types!
-- TODO if we pack a Data context into SomeFKinded, get can't recover it!!
instance
  ( Binary (Demote k)
  , SingKind k
  , forall (fk :: k). SingI fk => Binary (ft fk)
  , forall (fk :: k). Data (ft fk)
  ) => Binary (SomeFKinded k ft) where
    put someV@(SomeFKinded v) = do
        put $ someFKindedKind someV
        put v
    get = get @(Demote k) >>= \case -- parse fkind tag
      kindTag ->
        withSomeSing kindTag f
      where
        f :: forall (fk :: k). Sing fk -> Get (SomeFKinded k ft)
        f kind = do
            withSingI @fk kind $ do
                v <- get @(ft fk)
                pure $ undefined -- SomeFKinded @k @ft v

-- | Recover some @TYPE(x)@'s kind (the @x@).
someFKindedKind :: SomeFKinded k ft -> Demote k
someFKindedKind (SomeFKinded (_ :: ft fk)) = demote @fk

---

-- | A kinded Fortran value.
class FKinded' a where
    -- | The Haskell type used to record this Fortran type's kind.
    type FKind a

    -- | For every Fortran kind of this Fortran type @a@, the underlying
    --   representation @b@ has the given constraints.
    type FTypeC a b :: Constraint

    -- | Obtain the kind of a Fortran value.
    fKind :: a -> FKind a
