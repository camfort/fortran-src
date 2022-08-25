module Language.Fortran.Repr.Eval.Common where

import qualified Language.Fortran.AST as F

{- | Monads which provide functionality to evaluate some Fortran type or value.

We abstract over the evaluation target type in order to reuse this for both
value evaluation, and "type evaluation", since there is (a small amount of)
overlap.

Instances of this class will have a way to access variables in the current
context (e.g. a @Reader@ over a @Map@), and log warnings (e.g. a @Writer
String@).
-}
class Monad m => MonadEval m where
    -- | Target type that we evaluate to.
    type EvalTo m
    lookupFVar :: F.Name -> m (Maybe (EvalTo m))

    -- | Arbitrarily record some user-facing information concerning evaluation.
    --
    -- For example, potentially useful when making defaulting decisions.
    warn :: String -> m ()
