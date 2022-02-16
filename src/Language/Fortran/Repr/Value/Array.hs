-- TODO no Binary instance because @Generic a => Generic (Vector a)@ doesn't
-- exist (should it, I don't know, but I want it!)

module Language.Fortran.Repr.Value.Array where

import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

import qualified Data.Vector as V
import           Data.Vector ( Vector )

import           Language.Fortran.Repr.Value.Scalar

type FValArray = RankArray FValScalar

data RankArray a = RankArray
  { rankArrayDims :: Vector Dim
  , rankArrayData :: Vector a
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out)

data Dim = Dim
  { dimStart :: Int
  , dimEnd   :: Int
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

instance Out a => Out (Vector a) where
    doc = doc . V.toList
    docPrec = const doc

atIndex :: Vector Dim -> RankArray a -> Maybe a
atIndex = error "not yet"
