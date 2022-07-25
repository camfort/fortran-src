{- | Fortran logical value.

Fortran compilers usually store logicals as integers (TODO actually I think this
is in the standard), with the same kinding. To more accurately simulate their
behaviour, we represent them directly as integers, and simply provide a handful
of definitions for using them as booleans.
-}
module Language.Fortran.Repr.Value.Scalar.Logical.Machine where

import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Int.Machine
import Language.Fortran.Repr.Value.Scalar.Common

-- | Retrieve the boolean value stored by a @LOGICAL(x)@.
fLogicalToBool :: FInt k -> Bool
fLogicalToBool = fIntUOp $ consumeFLogicalNumeric True False

-- | Convert a bool to its Fortran machine representation in any numeric type.
fLogicalNumericFromBool :: Num a => Bool -> a
fLogicalNumericFromBool = \case True -> 1; False -> 0

-- | Consume some Fortran logical stored using an integer.
consumeFLogicalNumeric :: (Num a, Eq a) => r -> r -> a -> r
consumeFLogicalNumeric whenTrue whenFalse bi =
    if bi == 1 then whenTrue else whenFalse

fLogicalNot :: FInt k -> FInt k
fLogicalNot = fIntUOpInplace (consumeFLogicalNumeric 0 1)
