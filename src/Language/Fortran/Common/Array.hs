{-# LANGUAGE UndecidableInstances #-} -- required due to instance design

module Language.Fortran.Common.Array where

import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )

import qualified Text.PrettyPrint as Pretty

import qualified Language.Fortran.PrettyPrint as F

-- | A single array dimension with bounds of type @a@.
--
--   * @'Num' a => 'Dim' a@ is a static, known-size dimension.
--   * @'Dim' ('Language.Fortran.AST.Expression' '()')@ is a dimension with
--     unevaluated bounds expressions. Note that these bounds may be constant
--     expressions, or refer to dummy variables, or be invalid.
--   * @'Num' a => 'Dim' ('Maybe' a)@ is a dimension where some bounds are
--     known, and others are not. This may be useful to record some information
--     about dynamic explicit-shape arrays.
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

-- | Fortran array dimensions, defined by a list of 'Dim's storing lower and
--   upper bounds.
--
-- You select the list type @t@ (which should be 'Functor', 'Foldable' and
-- 'Traversable') and the bound type @a@ (e.g. 'Int').
--
-- Using a non-empty list type such as 'Data.List.NonEmpty.NonEmpty' will
-- disallow representing zero-dimension arrays, providing extra soundness.
--
-- Note the following excerpt from the F2018 standard (8.5.8.2 Explicit-shape
-- array):
--
-- > If the upper bound is less than the lower bound, the range is empty, the
-- > extent in that dimension is zero, and the array is of zero size.
data Dims t a
  -- | Explicit-shape array. All dimensions are known.
  = DimsExplicitShape
      (t (Dim a)) -- ^ list of all dimensions

  -- | Assumed-size array. The final dimension has no upper bound (it is
  --   obtained from its effective argument). Earlier dimensions may be defined
  --   like explicit-shape arrays.
  | DimsAssumedSize
      (Maybe (t (Dim a))) -- ^ list of all dimensions except last
      a                   -- ^ lower bound of last dimension

  -- | Assumed-shape array. Shape is taken from effective argument. We store the
  --   lower bound for each dimension, and thus also the rank (via list length).
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

-- Faster is possible for non @[]@ list-likes, but this is OK for the general
-- case.
prettyIntersperse :: Foldable t => Pretty.Doc -> t Pretty.Doc -> Pretty.Doc
prettyIntersperse dBetween ds =
    case foldMap (\d -> [dBetween, d]) ds of
      []    -> mempty
      _:ds' -> mconcat ds'

prettyAfter :: Foldable t => Pretty.Doc -> t Pretty.Doc -> Pretty.Doc
prettyAfter dAfter = foldMap (\d -> d <> dAfter)

-- | Traverse over the functor in a 'Dims' value with a functor bound type.
--
-- For example, to turn a @'Dims' t ('Maybe' a)@ into a @'Maybe' ('Dims' t a)@.
dimsTraverse :: (Traversable t, Applicative f) => Dims t (f a) -> f (Dims t a)
dimsTraverse = traverse id
-- TODO provide a SPECIALIZE clause for the above Maybe case. performance! :)
