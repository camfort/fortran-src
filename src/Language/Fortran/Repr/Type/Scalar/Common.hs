module Language.Fortran.Repr.Type.Scalar.Common where

import Data.Word ( Word8 )

-- | The internal type used to pass type kinds around.
type FKindLit = Word8

-- | Fortran types which use simple integer kinds.
class FKind a where
    -- | Serialize the kind tag to the shared kind representation.
    printFKind :: a -> FKindLit

    -- | Parse a kind tag from the shared kind representation.
    parseFKind :: FKindLit -> Maybe a
