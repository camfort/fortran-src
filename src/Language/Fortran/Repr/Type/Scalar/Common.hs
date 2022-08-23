module Language.Fortran.Repr.Type.Scalar.Common where

import Language.Fortran.Repr.Util
import Language.Fortran.Repr.Compat.Natural

import Data.Kind
import GHC.TypeNats

import Data.Type.Equality
import Data.Ord.Singletons
import Unsafe.Coerce

-- | Fortran kinds are represented by natural numbers. We use them on both type
--   and term levels.
type FKindTerm = Natural
type FKindType = NaturalK

-- | Reify a kind tag to its 'Natural' equivalent.
reifyKinded
    :: forall k (a :: k) n. (n ~ FKindOf a, KnownNat n)
    => Sing a -> FKindTerm
reifyKinded _ = natVal'' @n

-- | Fortran types which use simple integer kinds.
class FKinded (a :: Type) where
    type FKindOf (x :: a) :: FKindType
    type FKindDefault :: a

    -- | This we get via the type family, but require singletons.
    printFKind :: a -> FKindTerm

    -- | This we *should* get via the type family, but again require singletons.
    parseFKind :: FKindTerm -> Maybe a

{-
-- | Fortran strings
instance FKinded Natural where
    type FKindOf n = n
    type FKindDefault = 1 -- TODO ??
    printFKind = id
    parseFKind = Just
-}

--------------------------------------------------------------------------------

data SingCmp (l :: k) (r :: k)
  = SingEq (l :~: r)
  | SingLt
  | SingGt

-- | Upgrade an 'SOrdering' to include a proof of type equality for the equal
--   case.
--
-- We have no choice but to fake the 'Refl' with 'unsafeCoerce'. But assuming
-- 'SEQ' is used correctly, it should be safe.
singCompare
    :: forall k (a :: k) (b :: k). SOrd k
    => Sing a -> Sing b -> SingCmp a b
singCompare a b =
    case a `sCompare` b of
      SEQ -> SingEq (unsafeCoerce Refl)
      SLT -> SingLt
      SGT -> SingGt
