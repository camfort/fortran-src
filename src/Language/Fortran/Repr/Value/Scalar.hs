{- | Fortran scalar value representation.

For kinded Fortran types where different kinds use different representations,
e.g. INTEGER, the general pattern is to export a rank-2 function each for unary
and binary operations. They are restricted with a type class appropriate to the
underlying values stored e.g. 'Integral', 'RealFloat'. The function is then
specialized depending on the value's representation - and thus kind, since the
kind informs the representation.

For more details, see the 'Language.Fortran.Repr.Value.Scalar.Int.Machine'
module.
-}

module Language.Fortran.Repr.Value.Scalar
  ( module Language.Fortran.Repr.Value.Scalar.Machine
  ) where

import Language.Fortran.Repr.Value.Scalar.Machine
