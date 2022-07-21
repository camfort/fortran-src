module Language.Fortran.Analysis.Types where

data TypeEnv = TypeEnv
  { typeEnv :: Map Name 
  }

class MonadState m => Infer m where
    kwi

analyseTypes :: Infer m => [Block (Analysis a)] -> m [Block (Analysis a)]
