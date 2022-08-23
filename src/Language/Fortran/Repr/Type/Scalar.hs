module Language.Fortran.Repr.Type.Scalar where

import Language.Fortran.Repr.Type.Scalar.Common
import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Type.Scalar.Complex
import Language.Fortran.Repr.Type.Scalar.String

import Language.Fortran.Repr.Compat.Natural

import GHC.Generics ( Generic )
import Data.Data ( Data )

-- | A Fortran scalar type.
data FScalarType
  = FSTInt FTInt
  | FSTReal FTReal
  | FSTComplex FTReal
  | FSTLogical FTInt
  | FSTString Natural
  | FSTCustom String     -- ^ F77 structure, F90 DDT (non-intrinsic scalar)
    deriving stock (Generic, Data, Show, Eq, Ord)

prettyScalarType :: FScalarType -> String
prettyScalarType = \case
  FSTInt     k -> prettyKinded k "INTEGER"
  FSTReal    k -> prettyKinded k "REAL"
  FSTComplex k -> prettyKinded (FTComplexWrapper k) "COMPLEX"
  FSTLogical k -> prettyKinded k "LOGICAL"
  FSTString  l -> "CHARACTER("<>prettyCharLen l<>")"
  FSTCustom  t -> "TYPE("<>t<>")"

prettyKinded :: FKinded a => a -> String -> String
prettyKinded k name = name<>"("<>show (printFKind k)<>")"
