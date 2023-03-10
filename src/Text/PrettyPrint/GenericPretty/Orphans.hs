{-# LANGUAGE DerivingVia #-}
-- TODO orphans pragma

module Text.PrettyPrint.GenericPretty.Orphans where

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.GenericPretty.ViaShow ( OutShowly(..) )

import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Int
import Numeric.Natural

-- | Not particularly efficient (but neither is GenericPretty).
deriving via OutShowly Text instance Out Text

deriving via OutShowly Int8  instance Out Int8
deriving via OutShowly Int16 instance Out Int16
deriving via OutShowly Int32 instance Out Int32
deriving via OutShowly Int64 instance Out Int64
deriving via OutShowly Natural instance Out Natural
