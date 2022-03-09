module Language.Fortran.AST.Literal where

import Language.Fortran.AST.Common ( Name )
import Language.Fortran.Util.Position ( SrcSpan )

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data, Typeable )
import           Control.DeepSeq                ( NFData )
import           Text.PrettyPrint.GenericPretty ( Out )

data KindParam a
  = KindParamInt a SrcSpan String -- ^ @[0-9]+@
  | KindParamVar a SrcSpan Name   -- ^ @[a-z][a-z0-9]+@ (case insensitive)
    deriving stock    (Eq, Show, Data, Typeable, Generic, Functor)
    deriving anyclass (NFData, Out)
