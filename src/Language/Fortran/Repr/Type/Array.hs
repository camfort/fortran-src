{-# OPTIONS_GHC -Wno-orphans #-} -- 'Out' instances: TODO move elsewhere
{-# LANGUAGE RankNTypes #-}

module Language.Fortran.Repr.Type.Array where

import Language.Fortran.Repr.Type.Scalar ( FTypeScalar )

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty )

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )
import Text.PrettyPrint ( text )
import Numeric.Natural

data FTypeArray = FTypeArray
  { fTypeArrayScalar :: FTypeScalar
  , fTypeArrayShape  :: ArrayShape
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

{-| Fortran array type information.

Fortran's arrays are a statically-sized collection of scalar values all of the
same type (and kind), indexed by between 1 and X dimensions (varies between
standards), where each dimension has a static size, called an extent. The number
of dimensions is called the rank. An array defines its _shape_, which is each
dimension's extent.

Accessing arrays involves some indirection, since Fortran allows defining
arbitrary lower and upper bounds. In reality, this means storing a starting
index per dimension(instead of an implicit 0), and recovering the upper bound by
adding it to the extent.

In the latest Fortran standards, there are many different types of arrays, which
are shaped, indexed, and/or reshaped in various ways at runtime. See the
constructor documentation for details. All shapes store at least array rank,
though some allow automatic reshaping.

Implied-shape arrays are the only way to get constant arrays in Fortran. They're
also the only array type with constant shape, though we can attempt to resolve
other shape info explicit-shape array shape (and fail if they use things like
function arguments). We don't have a strong embedding of Fortran expressions to
allow recording non-constant dimension info, so it's "all or nothing".

-}
data ArrayShape
  = ExplicitShape (Maybe (NonEmpty Dimension))
  -- ^ explicit-shape: all dimension bounds known before executable statements.
  --   Not guaranteed constant.

  | AssumedShape (Maybe (NonEmpty DimIdx))
  -- ^ assumed-shape: must be DUMMY, and not ALLOCATABLE or POINTER. Stores only
  --   lower bound for each dimension. The extent (length) is retrieved from the
  --   respective dimension of its effective argument. (The upper bound is then
  --   calculated from them.)

  | ImpliedShape (NonEmpty DimIdx)
  -- ^ implied-shape: named constant, takes shape from constant expr in
  --   declaration. Similar to assumed-shape, but non-DUMMY.

  | DeferredShape Rank
  -- ^ deferred-shape: must be ALLOCATABLE or POINTER. Only stores rank=number
  --   of dimensions (>=1, <=7); no other information.

  | AssumedSize (Maybe [Dimension]) (Maybe DimIdx)
  -- ^ assumed-size: DUMMY, size assumed from effective argument. (Rank &
  --   extents may differ.) Implicit @*@ final dimension, with an optional lower
  --   bound. (@Nothing@ means it was either not present, or not constant.)

    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

data ArrayAttr
  = ArrayAttrDummy
  | ArrayAttrAllocatable
  | ArrayAttrPointer
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | @(lower, upper)@ for indexing into the dimension. Negatives allowed.
--
-- Could also use @(lower, extent)@ which is apparently the internal norm.
--
-- TODO exclusive? confirm
type Dimension = (DimIdx, DimIdx)

-- TODO confirm maximum array size (and likely use Int64 explicitly)
type DimIdx = Int

type Rank = Natural

-- | Try to obtain the rank defined by an 'ArrayShape'. If the shape isn't
--   "filled out", return 'Nothing'.
rank :: ArrayShape -> Maybe Rank
rank = \case
  ExplicitShape ds   -> fil <$> ds
  AssumedShape  ds   -> fil <$> ds
  ImpliedShape  ds   -> Just $ fil ds
  DeferredShape r    -> Just r
  AssumedSize   ds _ -> fil <$> ds
  where
    fil :: forall t a. Foldable t => t a -> Natural
    fil = fromIntegral . length

--------------------------------------------------------------------------------

instance Out a => Out (NonEmpty a) where
    doc = docList . NE.toList
    docPrec _ = doc

instance Out Natural where
    doc = text . show
    docPrec _ = doc
