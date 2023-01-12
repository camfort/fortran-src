{- | Machine Fortran LOGICAL values.

Fortran compilers usually store LOGICALs as INTEGERs (they former is tied to the
latter in the specifications). To more accurately simulate their behaviour, we
represent them directly as integers, and simply provide a handful of definitions
for using them as booleans.
-}

module Language.Fortran.Repr.Value.Scalar.Logical.Machine where

import Language.Fortran.Repr.Value.Scalar.Int.Machine

-- | Retrieve the boolean value stored by a @LOGICAL(x)@.
fLogicalToBool :: FInt -> Bool
fLogicalToBool = fIntUOp $ consumeFLogicalNumeric True False

-- | Convert a bool to its Fortran machine representation in any numeric type.
fLogicalNumericFromBool :: Num a => Bool -> a
fLogicalNumericFromBool = \case True -> 1; False -> 0

-- | Consume some Fortran logical stored using an integer.
consumeFLogicalNumeric :: (Num a, Eq a) => r -> r -> a -> r
consumeFLogicalNumeric whenTrue whenFalse bi =
    if bi == 1 then whenTrue else whenFalse

fLogicalNot :: FInt -> FInt
fLogicalNot = fIntUOpInplace (consumeFLogicalNumeric 0 1)
