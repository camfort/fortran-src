{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Repr.Eval.Scalar where

import           Language.Fortran.AST
import           Language.Fortran.Repr.Value

import qualified Data.Data as Data
import           Data.Data ( Data )
import qualified Data.Map  as Map
import           Data.Map  ( Map )

data Env = Env
  { envVars       :: Map Name FValScalar
  , envIntrinsics :: Map Name ()
  }

data Error
  = ErrorVarUndefined Name
  | ErrorUnsupportedExpression String
  | ErrorUnsupportedValue String
    deriving (Eq, Show)

eval :: Data a => Env -> Expression a -> Either Error FValScalar
eval env = \case
  ExpValue _ _ valExpr -> case valExpr of
    ValVariable v -> do
      case Map.lookup v (envVars env) of
        Nothing  -> Left $ ErrorVarUndefined v
        Just val -> return val
    _ -> evalValue valExpr
  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

-- | Must not be a 'ValVariable'.
evalValue :: Data a => Value a -> Either Error FValScalar
evalValue = \case
  ValInteger i mkp -> FValScalarInt <$> evalInt i mkp
  v                -> Left $ ErrorUnsupportedValue $ show $ Data.toConstr v

evalInt :: String -> Maybe (Expression a) -> Either Error FValInt
evalInt = undefined
