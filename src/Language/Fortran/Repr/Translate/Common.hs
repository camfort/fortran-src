module Language.Fortran.Repr.Translate.Common where

import qualified Language.Fortran.AST as F

class Monad m => MonadTranslate m where
    type TranslateTo m
    lookupFVar :: F.Name -> m (Maybe (TranslateTo m))

    -- | Arbitrarily record some user-facing information concerning translation.
    --
    -- For example, potentially useful when making defaulting decisions.
    warn :: String -> m ()

{-

-- examples v
instance MonadTranslate (Reader FV.SymbolTable) where
    type TranslateTo FV.SymbolTable = ExpVal
    warn _ = pure ()

instance MonadTranslate WrappedSymbolTable where
    type TranslateTo WrappedSymbolTable = ExpVal

-}
