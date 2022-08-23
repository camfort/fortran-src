{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, UndecidableInstances #-}

module Language.Fortran.Repr.Type.Scalar.Int where

import Language.Fortran.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons
import Data.Ord.Singletons

$(singletons [d|
    -- | The Fortran integer type.
    data FTInt
      = FTInt1  -- ^ @INTEGER(1)@
      | FTInt2  -- ^ @INTEGER(2)@
      | FTInt4  -- ^ @INTEGER(4)@
      | FTInt8  -- ^ @INTEGER(8)@
      | FTInt16 -- ^ @INTEGER(16)@
        deriving stock (Eq, Ord, Show)
    |])
deriving stock instance Generic FTInt
deriving stock instance Data    FTInt
deriving stock instance Enum    FTInt

-- | Get the output type from combining two integer values of arbitrary kinds
--   (for example, adding an @INTEGER(1)@ and an @INTEGER(4)@).
type FTIntCombine :: FTInt -> FTInt -> FTInt
type family FTIntCombine k1 k2 where
    FTIntCombine 'FTInt16 _        = 'FTInt16
    FTIntCombine _        'FTInt16 = 'FTInt16
    FTIntCombine 'FTInt8  _        = 'FTInt8
    FTIntCombine _        'FTInt8  = 'FTInt8
    FTIntCombine 'FTInt4  _        = 'FTInt4
    FTIntCombine _        'FTInt4  = 'FTInt4
    FTIntCombine 'FTInt2  _        = 'FTInt2
    FTIntCombine _        'FTInt2  = 'FTInt2
    FTIntCombine 'FTInt1  'FTInt1  = 'FTInt1

instance FKinded FTInt where
    type FKindOf 'FTInt1  = 1
    type FKindOf 'FTInt2  = 2
    type FKindOf 'FTInt4  = 4
    type FKindOf 'FTInt8  = 8
    type FKindOf 'FTInt16 = 16
    type FKindDefault = 'FTInt4
    parseFKind = \case 1  -> Just FTInt1
                       2  -> Just FTInt2
                       4  -> Just FTInt4
                       8  -> Just FTInt8
                       16 -> Just FTInt16
                       _ -> Nothing
    printFKind (FromSing x) = case x of
      SFTInt1  -> reifyKinded x
      SFTInt2  -> reifyKinded x
      SFTInt4  -> reifyKinded x
      SFTInt8  -> reifyKinded x
      SFTInt16 -> reifyKinded x
