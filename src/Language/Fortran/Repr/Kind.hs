-- | Resolving integers to the various type kind tags.

module Language.Fortran.Repr.Kind where

import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Value.Scalar

intAsIntKind :: FValInt -> Maybe FTypeInt
intAsIntKind = parseKindInt' . fvalInt
