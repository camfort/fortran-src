-- | Common Fortran evaluation definitions.

module Language.Fortran.Repr.Eval.Common where

import qualified Language.Fortran.AST as F

{- | Monads which provide functionality to evaluate Fortran expressions in some
     static context.

Actions in this monad may

  * request the value of a variable (may return 'Nothing' if not in scope)
  * record some user-facing information concerning evaluation

As usage examples, a simple pure evaluator may use a plain map of 'F.Name' to
@'EvalTo' m@. A more complex type evaluator may allow "defaulting" for variables
not in scope via IMPLICIT rules.

The associated type family 'EvalTo' enables using this for both type and value
evaluators.
-}
class Monad m => MonadFEval m where
    -- | Target type that we evaluate to.
    type EvalTo m

    -- | Request the value of a variable.
    --
    -- Returns 'Nothing' if the variable is not in scope.
    lookupFVar :: F.Name -> m (Maybe (EvalTo m))

    -- | Record some user-facing information concerning evaluation.
    --
    -- For example, you may want to inform the user when you've made a
    -- defaulting decision.
    warn :: String -> m ()
