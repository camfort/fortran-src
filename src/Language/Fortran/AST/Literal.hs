module Language.Fortran.AST.Literal where

import Language.Fortran.AST.Common ( Name )
import Language.Fortran.Util.Position ( SrcSpan, Spanned )
import Language.Fortran.Util.FirstParameter  ( FirstParameter )
import Language.Fortran.Util.SecondParameter ( SecondParameter )
import Language.Fortran.AST.Annotated ( Annotated )

import GHC.Generics                   ( Generic )
import Data.Data                      ( Data, Typeable )
import Control.DeepSeq                ( NFData )
import Text.PrettyPrint.GenericPretty ( Out )

data KindParam a
  = KindParamInt a SrcSpan String -- ^ @[0-9]+@
  | KindParamVar a SrcSpan Name   -- ^ @[a-z][a-z0-9]+@ (case insensitive)
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic, Functor)
    deriving anyclass (NFData, Out)

instance FirstParameter  (KindParam a) a
instance Annotated       KindParam
instance SecondParameter (KindParam a) SrcSpan
instance Spanned         (KindParam a)
