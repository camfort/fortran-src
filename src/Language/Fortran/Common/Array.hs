{-# LANGUAGE UndecidableInstances #-}

module Language.Fortran.Common.Array where

import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

-- | The data type using this has two values. Are they start and end/lower and
--   upper bounds, or start and extent?
data DimensionType = DTStartAndEnd | DTStartAndSize

-- | Evaluated dimensions of a Fortran array.
--
-- Syntactic datatype; includes lower bound. (Normalized arrays only require the
-- extent.) Also, since Fortran array dimensions can be defined in two ways
-- (@lower:upper@, or @lower@ and dimension extent) but share representation, a
-- type-level switch is included for distinguishing between them.
--
-- Extremely general. You select the list type @t@ (which should be 'Foldable')
-- and the numeric index type @a@ (e.g. 'Int'). The intent is that this type may
-- be quietly wrapped into others with type synonyms and perhaps pattern
-- synonyms.
--
-- 'DimensionType' only matters for explicit and assumed-size (the tuples).
data Dimensions (dt :: DimensionType) t a
  = DExplicitShape
      (t (a, a)) -- ^ list of all dimensions

  | DAssumedSize
      (t (a, a)) -- ^ list of all dimensions except last
      a          -- ^ lower bound of last dimension

  -- | Assumed-shape array dimensions. Here, we only have the lower bound for
  --   each dimension, and the rank (via length).
  | DAssumedShape
      (t a) -- ^ list of lower bounds

    deriving stock Generic
    deriving stock (Functor, Foldable, Traversable)

-- We have to standalone derive most instances due to @t (a, a)@.
deriving stock instance (Show a, Show (t a), Show (t (a, a)))
  => Show (Dimensions dt t a)
deriving anyclass instance (NFData a, NFData (t a), NFData (t (a, a)))
  => NFData (Dimensions dt t a)
deriving stock instance (Data a, Data (t a), Data (t (a, a)), Typeable t, Typeable dt)
  => Data (Dimensions dt t a)
deriving stock instance (Eq a, Eq (t a), Eq (t (a, a)))
  => Eq (Dimensions dt t a)
deriving anyclass instance (Binary a, Binary (t a), Binary (t (a, a)))
  => Binary (Dimensions dt t a)
deriving anyclass instance (Out a, Out (t a), Out (t (a, a)))
  => Out (Dimensions dt t a)

-- | Meaningless instance, only use transparently.
deriving stock instance (Ord a, Ord (t a), Ord (t (a, a)))
  => Ord (Dimensions dt t a)
