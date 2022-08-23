module Language.Fortran.Repr.Translate.Type where

import qualified Language.Fortran.AST as F
import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Translate.Common

fromExpression
    :: forall m a. (MonadTranslate m, TranslateTo m ~ FType)
    => F.Expression a -> m (Either String FType)
fromExpression = \case
  F.ExpValue _ _ (F.ValVariable name) ->
    lookupFVar name >>= \case
      Nothing  -> return $ Left "no such variable found TODO"
      Just val -> return $ Right val
