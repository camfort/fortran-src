{- | Idealized Fortran LOGICAL values.

In cases where you don't need the machine representation of a @LOGICAL(x)@,
which is likely to be an @INTEGER(x)@, you can store all kinds with a Haskell
'Bool'.
-}

module Language.Fortran.Repr.Value.Scalar.Logical.Idealized where

import Language.Fortran.Repr.Type.Scalar.Int

newtype FLogical (k :: FTInt) = FLogical Bool
    deriving stock (Show, Eq, Ord)
