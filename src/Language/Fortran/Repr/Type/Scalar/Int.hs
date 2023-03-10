{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Fortran.Repr.Type.Scalar.Int where

import Language.Fortran.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

-- | The Fortran integer type.
data FTInt
  = FTInt1  -- ^ @INTEGER(1)@
  | FTInt2  -- ^ @INTEGER(2)@
  | FTInt4  -- ^ @INTEGER(4)@
  | FTInt8  -- ^ @INTEGER(8)@
  | FTInt16 -- ^ @INTEGER(16)@
    deriving stock (Show, Generic, Data, Enum, Eq, Ord)
    deriving anyclass (Binary, Out)

instance FKind FTInt where
    parseFKind = \case 1  -> Just FTInt1
                       2  -> Just FTInt2
                       4  -> Just FTInt4
                       8  -> Just FTInt8
                       16 -> Just FTInt16
                       _  -> Nothing
    printFKind = \case FTInt1  -> 1
                       FTInt2  -> 2
                       FTInt4  -> 4
                       FTInt8  -> 8
                       FTInt16 -> 16

type FTIntCombine :: FTInt -> FTInt -> FTInt
type family FTIntCombine k1 k2 where
    FTIntCombine k k = k
    FTIntCombine 'FTInt16 _        = 'FTInt16
    FTIntCombine _        'FTInt16 = 'FTInt16
    FTIntCombine 'FTInt8  _        = 'FTInt8
    FTIntCombine _        'FTInt8  = 'FTInt8
    FTIntCombine 'FTInt4  _        = 'FTInt4
    FTIntCombine _        'FTInt4  = 'FTInt4
    FTIntCombine 'FTInt2  _        = 'FTInt2
    FTIntCombine _        'FTInt2  = 'FTInt2
