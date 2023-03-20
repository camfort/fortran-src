{-# LANGUAGE UndecidableInstances #-} -- required due to instance design

module Language.Fortran.Common.Array where

import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )

import qualified Text.PrettyPrint as Pretty

import qualified Language.Fortran.PrettyPrint as F

data Dim a = Dim
  { dimLower :: a -- ^ Dimension lower bound.
  , dimUpper :: a -- ^ Dimension upper bound.
  } deriving stock (Show, Generic, Data, Eq)
    deriving stock (Functor, Foldable, Traversable)
    deriving anyclass (NFData, Binary)

    -- | This instance is purely for convenience. No definition of ordering is
    --   provided, and the implementation may change at any time.
    deriving stock Ord

-- | Fortran syntax uses @lower:upper@, so only provide an 'Out' instance for
--   that style.
instance Out a => Out (Dim a) where
    doc (Dim lb ub) = doc lb <> Pretty.char ':' <> doc ub

instance Out (Dim a) => F.Pretty (Dim a) where
    pprint' _ = doc

-- | Evaluated dimensions of a Fortran array.
--
-- A known-length dimension is defined by a lower bound and an upper bound. This
-- data type takes a syntactic view, rather than normalizing lower bound to 0
-- and passing just dimension extents.
--
-- You select the list type @t@ (which should be 'Functor', 'Foldable' and
-- 'Traversable') and the numeric index type @a@ (e.g. 'Int').
--
-- Note that using a non-empty list type such as 'Data.List.NonEmpty.NonEmpty'
-- will disallow representing zero-dimension arrays, which may be useful for
-- soundness.
--
-- Note the following excerpt from the F2018 standard (8.5.8.2 Explicit-shape
-- array):
--
-- > If the upper bound is less than the lower bound, the range is empty, the
-- > extent in that dimension is zero, and the array is of zero size.
data Dims t a
  = DimsExplicitShape
      (t (Dim a)) -- ^ list of all dimensions

  | DimsAssumedSize
      (Maybe (t (Dim a))) -- ^ list of all dimensions except last
      a          -- ^ lower bound of last dimension

  -- | Assumed-shape array dimensions. Here, we only have the lower bound for
  --   each dimension, and the rank (via length).
  | DimsAssumedShape
      (t a) -- ^ list of lower bounds

    deriving stock (Generic)
    deriving stock (Functor, Foldable, Traversable)

-- We have to standalone derive most instances due to the @t@ list-like.
deriving stock instance (Show a, Show (t a), Show (t (Dim a)))
  => Show (Dims t a)
deriving anyclass instance (NFData a, NFData (t a), NFData (t (Dim a)))
  => NFData (Dims t a)
deriving stock instance (Data a, Data (t a), Data (t (Dim a)), Typeable t)
  => Data (Dims t a)
deriving stock instance (Eq a, Eq (t a), Eq (t (Dim a)))
  => Eq (Dims t a)
deriving anyclass instance (Binary a, Binary (t a), Binary (t (Dim a)))
  => Binary (Dims t a)

-- | This instance is purely for convenience. No definition of ordering is
--   provided, and the implementation may change at any time.
deriving stock instance (Ord a, Ord (t a), Ord (t (Dim a)))
  => Ord (Dims t a)

instance (Foldable t, Functor t, Out (Dim a), Out a)
  => Out (Dims t a) where
    docPrec _ = doc
    doc = Pretty.parens . \case
      DimsExplicitShape ds ->
        prettyIntersperse dimSep $ fmap doc ds
      DimsAssumedShape ss ->
        prettyIntersperse dimSep $ fmap go ss
        where
          go s = doc s <> Pretty.char ':'
      DimsAssumedSize mds d ->
        -- A bit fragile, but hopefully won't explode on empty 'Just's.
        case mds of
          Nothing -> prettyLast
          Just ds -> prettyAfter dimSep (fmap doc ds) <> prettyLast
        where
          prettyLast = doc d <> Pretty.text ":*"
      where
        dimSep = Pretty.text ", "

instance Out (Dims t a) => F.Pretty (Dims t a) where
    pprint' _ = doc

-- Faster is possible for non-@List@s, but this is OK for the general case.
prettyIntersperse :: Foldable t => Pretty.Doc -> t Pretty.Doc -> Pretty.Doc
prettyIntersperse dBetween ds =
    case foldMap (\d -> [dBetween, d]) ds of
      []    -> mempty
      _:ds' -> mconcat ds'

prettyAfter :: Foldable t => Pretty.Doc -> t Pretty.Doc -> Pretty.Doc
prettyAfter dAfter = foldMap (\d -> d <> dAfter)
