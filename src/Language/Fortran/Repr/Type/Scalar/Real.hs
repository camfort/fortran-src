{-# LANGUAGE TemplateHaskell, StandaloneKindSignatures, UndecidableInstances #-}

module Language.Fortran.Repr.Type.Scalar.Real where

import Language.Fortran.Repr.Type.Scalar.Common

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Singletons.TH
-- required for deriving instances (seems like bug)
import Prelude.Singletons
import Data.Ord.Singletons

import Data.Binary ( Binary )

$(singletons [d|
    data FTReal
      = FTReal4
      | FTReal8
        deriving stock (Eq, Ord, Show)
    |])
deriving stock    instance Generic FTReal
deriving stock    instance Data    FTReal
deriving stock    instance Enum    FTReal
deriving anyclass instance Binary  FTReal

-- | Get the output type from combining two real values of arbitrary kinds (for
--   example, adding a @REAL(4)@ and a @REAL(8)@).
type FTRealCombine :: FTReal -> FTReal -> FTReal
type family FTRealCombine k1 k2 where
    FTRealCombine 'FTReal8 _        = 'FTReal8
    FTRealCombine _        'FTReal8 = 'FTReal8
    FTRealCombine 'FTReal4 'FTReal4 = 'FTReal4

instance FKinded FTReal where
    type FKindOf 'FTReal4 = 4
    type FKindOf 'FTReal8 = 8
    type FKindDefault = 'FTReal4
    parseFKind = \case 4 -> Just FTReal4
                       8 -> Just FTReal8
                       _ -> Nothing
    -- spurious warning on GHC 9.0
    printFKind (FromSing x) = case x of
      SFTReal4 -> reifyKinded x
      SFTReal8 -> reifyKinded x
