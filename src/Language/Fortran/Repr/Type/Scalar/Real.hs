module Language.Fortran.Repr.Type.Scalar.Real where

import Language.Fortran.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

data FTReal
  = FTReal4
  | FTReal8
    deriving stock (Show, Generic, Data, Enum, Eq, Ord)
    deriving anyclass (Binary, Out)

instance FKind FTReal where
    parseFKind = \case 4 -> Just FTReal4
                       8 -> Just FTReal8
                       _ -> Nothing
    -- spurious warning on GHC 9.0
    printFKind = \case FTReal4 -> 4
                       FTReal8 -> 8
