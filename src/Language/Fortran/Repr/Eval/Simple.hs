{-# LANGUAGE DerivingVia #-}

module Language.Fortran.Repr.Eval.Simple where

import Language.Fortran.Repr.Eval.Common
import Language.Fortran.Repr.Eval.Value
import qualified Language.Fortran.Repr.Eval.Value.Op as Op
import Language.Fortran.Repr.Value.Machine ( FValue )
import qualified Language.Fortran.AST as F

import Data.Map ( Map )
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

-- | A simple pure interpreter for Fortran value evaluation programs.
newtype Eval v a = Eval
    { unEval :: WriterT [String] (ExceptT Error (Reader (Env v))) a
    } deriving (Functor, Applicative, Monad, MonadReader (Env v), MonadWriter [String], MonadError Error) via WriterT [String] (ExceptT Error (Reader (Env v)))

runEval :: Env v -> Eval v a -> Either Error (a, [String])
runEval env = flip runReader env . runExceptT . runWriterT . unEval

data Env v = Env
  { envVars :: Map F.Name v
  , envCfg  :: Cfg
  } deriving stock (Show, Eq)

defEnv :: Env v
defEnv = Env Map.empty defCfg

instance MonadEval (Eval v) where
    type EvalTo (Eval v) = v
    warn msg = tell [msg]
    lookupFVar nm = do
        m <- asks envVars
        pure $ Map.lookup nm m

instance MonadEvalValue (Env FValue) (Eval FValue) where
    err = throwError

instance HasEvalValueCfg (Env v) where getEvalValueCfg = envCfg
instance Op.MonadEvalOp (Env v) (Eval v) where
    err = throwError . EOp
    warn msg = tell [msg]
instance Op.HasEvalOpCfg (Env v) where getEvalOpCfg = Op.getEvalOpCfg . envCfg
