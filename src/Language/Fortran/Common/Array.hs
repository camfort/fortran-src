{-# LANGUAGE UndecidableInstances #-}

module Language.Fortran.Common.Array where

import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )

import qualified Text.PrettyPrint as Pretty

import qualified Language.Fortran.PrettyPrint as F

-- | The data type using this has two values. Are they lower and upper bounds,
--   or lower bound and extent?
data DimType = DimTypeUpper | DimTypeExtent

data Dim (dt :: DimType) a = Dim
  { dimLeftLower :: a
  -- ^ Dimension lower bound.

  , dimRight :: a
  -- ^ If 'DimType' is 'DimTypeUpper', this is the upper bound. If it is
  -- 'DimTypeExtent', this is the extent.
  } deriving stock (Show, Generic, Data, Eq)
    deriving stock (Functor, Foldable, Traversable)
    deriving anyclass (NFData, Binary)

    -- | Meaningless instance, only use transparently.
    deriving stock Ord

-- | Fortran syntax uses @lower:upper@, so only provide an 'Out' instance for
--   that style.
instance Out a => Out (Dim 'DimTypeUpper a) where
    doc (Dim lb ub) = doc lb <> Pretty.char ':' <> doc ub

instance Out (Dim dt a) => F.Pretty (Dim dt a) where
    pprint' _ = doc

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
-- For soundness, consider using a non-empty list type.
--
-- 'DimensionType' only matters for explicit and assumed-size (the tuples).
data Dims (dt :: DimType) t a
  = DimsExplicitShape
      (t (Dim dt a)) -- ^ list of all dimensions

  | DimsAssumedSize
      (Maybe (t (Dim dt a))) -- ^ list of all dimensions except last
      a          -- ^ lower bound of last dimension

  -- | Assumed-shape array dimensions. Here, we only have the lower bound for
  --   each dimension, and the rank (via length).
  | DimsAssumedShape
      (t a) -- ^ list of lower bounds

    deriving stock (Generic)
    deriving stock (Functor, Foldable, Traversable)

-- We have to standalone derive most instances due to @t (a, a)@.
deriving stock instance (Show a, Show (t a), Show (t (Dim dt a)))
  => Show (Dims dt t a)
deriving anyclass instance (NFData a, NFData (t a), NFData (t (Dim dt a)))
  => NFData (Dims dt t a)
deriving stock instance (Data a, Data (t a), Data (t (Dim dt a)), Typeable t, Typeable dt)
  => Data (Dims dt t a)
deriving stock instance (Eq a, Eq (t a), Eq (t (Dim dt a)))
  => Eq (Dims dt t a)
deriving anyclass instance (Binary a, Binary (t a), Binary (t (Dim dt a)))
  => Binary (Dims dt t a)

-- | Meaningless instance, only use transparently.
deriving stock instance (Ord a, Ord (t a), Ord (t (Dim dt a)))
  => Ord (Dims dt t a)

instance (Foldable t, Functor t, Out (Dim dt a))
  => Out (Dims dt t a) where
    docPrec _ = doc
    doc = Pretty.parens . \case
      DimsExplicitShape ds ->
        prettyIntersperse (Pretty.text ", ") $ fmap doc ds
      _ -> mempty

instance Out (Dims dt t a) => F.Pretty (Dims dt t a) where
    pprint' _ = doc

-- Faster is possible for non-@List@s, but this is OK for the general case.
prettyIntersperse :: Foldable t => Pretty.Doc -> t Pretty.Doc -> Pretty.Doc
prettyIntersperse dBetween ds =
    case foldMap (\d -> [dBetween, d]) ds of
      []    -> mempty
      _:ds' -> mconcat ds'
