module Language.Fortran.Repr.Eval.Common where

import qualified Language.Fortran.AST as F

class Monad m => MonadEval m where
    type EvalTo m
    lookupFVar :: F.Name -> m (Maybe (EvalTo m))

    -- | Arbitrarily record some user-facing information concerning evaluation.
    --
    -- For example, potentially useful when making defaulting decisions.
    warn :: String -> m ()

{-

-- examples v
instance MonadEval (Reader FV.SymbolTable) where
    type EvalTo FV.SymbolTable = ExpVal
    warn _ = pure ()

instance MonadEval WrappedSymbolTable where
    type EvalTo WrappedSymbolTable = ExpVal

-}
