{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Language.Fortran.Repr.Type.Scalar.Int where

import Language.Fortran.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons hiding ( type (-), type (*) )
import Data.Ord.Singletons

import GHC.TypeNats

import Data.Binary ( Binary )

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
deriving stock    instance Generic FTInt
deriving stock    instance Data    FTInt
deriving stock    instance Enum    FTInt
deriving anyclass instance Binary  FTInt

-- | Get the output type from combining two integer values of arbitrary kinds
--   (for example, adding an @INTEGER(1)@ and an @INTEGER(4)@).
--
-- TODO is this OK?? the @k k = k@ equation at top???
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
    -- spurious warning on GHC 9.0
    printFKind (FromSing x) = case x of
      SFTInt1  -> reifyKinded x
      SFTInt2  -> reifyKinded x
      SFTInt4  -> reifyKinded x
      SFTInt8  -> reifyKinded x
      SFTInt16 -> reifyKinded x

-- | @max k = 2^(8k-1) - 1@
type FTIntMax :: FTInt -> Nat
type family FTIntMax k where
    FTIntMax 'FTInt1  = 2^(8*1 -1) - 1
    FTIntMax 'FTInt2  = 2^(8*2 -1) - 1
    FTIntMax 'FTInt4  = 2^(8*4 -1) - 1
    FTIntMax 'FTInt8  = 2^(8*8 -1) - 1
    FTIntMax 'FTInt16 = 2^(8*16-1) - 1

-- | @min k = - (2^(8k-1))@ (make sure you negate when reifying etc!)
type FTIntMin :: FTInt -> Nat
type family FTIntMin k where
    FTIntMin 'FTInt1  = 2^(8*1 -1)
    FTIntMin 'FTInt2  = 2^(8*2 -1)
    FTIntMin 'FTInt4  = 2^(8*4 -1)
    FTIntMin 'FTInt8  = 2^(8*8 -1)
    FTIntMin 'FTInt16 = 2^(8*16-1)
