-- TODO no Binary instance because @Generic a => Generic (Vector a)@ doesn't
-- exist (should it, I don't know, but I want it!)

{-
Some values store some info also used at the type level. This is done where that
info can be used on the value level to influence how the value is used e.g. to
check bounds.
-}

module Language.Fortran.Repr.Value where

import           Language.Fortran.Repr.Value.Scalar
import           Language.Fortran.Repr.Value.Array
import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.AST.Literal.Boz ( Boz )

import           Data.Int                       ( Int8, Int16, Int32, Int64 )
import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                      as Text
import           Data.Void
import qualified Data.Vector as V
import           Data.Vector ( Vector )

data FVal = FValScalar FValScalar | FValArray' FValArray
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out)
