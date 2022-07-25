module Language.Fortran.Analysis.Types where

import Language.Fortran.AST
import Language.Fortran.Repr.Eval.Common
import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Value.Scalar
import Language.Fortran.Analysis

-- | TODO binder scoping? probably use "uniques" everywhere, but unclear how
--   consistent they are...
--
-- Implementations should be able to throw Fortran type errors, like a variable
-- being used at two types which don't automatically coerce.
class (MonadEval m, EvalTo m ~ FScalarValue) => MonadInferFType m where
    lookupFVarType :: Name -> m (Maybe FType)

    -- | Should fail if you try to insert over an existing binder.
    insertFVarType :: Name -> m ()

analyseTypes :: MonadInferFType m => [Block (Analysis a)] -> m [Block (Analysis a)]
analyseTypes = undefined
