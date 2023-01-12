-- | Evaluate AST terms to types in the type representation.

module Language.Fortran.Repr.Eval.Type where

import qualified Language.Fortran.AST as F
import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Eval.Common

fromExpression
    :: forall m a. (MonadFEval m, EvalTo m ~ FType)
    => F.Expression a -> m (Either String FType)
fromExpression = \case
  F.ExpValue _ _ (F.ValVariable name) ->
    lookupFVar name >>= \case
      Nothing  -> return $ Left "no such variable found TODO"
      Just val -> return $ Right val

-- TODO support for IMPLICIT rules
