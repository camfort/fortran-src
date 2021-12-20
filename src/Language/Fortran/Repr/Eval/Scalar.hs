{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Repr.Eval.Scalar where

import           Language.Fortran.AST
import           Language.Fortran.Repr.Type
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
  | ErrorUnsupportedCommonType String String
    deriving (Eq, Show)

eval :: Data a => Env -> Expression a -> Either Error FValScalar
eval env v = case v of
  ExpValue _ _ valExpr -> case valExpr of
    ValVariable v -> do
      case Map.lookup v (envVars env) of
        Nothing  -> Left $ ErrorVarUndefined v
        Just val -> return val
    _ -> evalValue valExpr

  ExpBinary a s op valExpr1 valExpr2 -> do
    res1 <- eval env valExpr1
    res2 <- eval env valExpr2
    case evalBinaryOp op res1 res2 of
      Left  _   -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr v
      Right res -> Right res

  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

-- | Must not be a 'ValVariable'.
evalValue :: Data a => Value a -> Either Error FValScalar
evalValue = \case
  ValInteger i mkp -> FValScalarInt <$> evalInt i mkp
  v                -> Left $ ErrorUnsupportedValue $ show $ Data.toConstr v

evalInt :: String -> Maybe (Expression a) -> Either Error FValInt
evalInt = undefined

evalBinaryOp :: BinaryOp -> FValScalar -> FValScalar -> Either Error FValScalar
evalBinaryOp op (FValScalarInt v1@(FValInt ty1 val1)) (FValScalarInt v2@(FValInt ty2 val2)) =
  case joinType ty1 ty2 of
    Nothing -> Left  $ ErrorUnsupportedCommonType (show ty1) (show ty2)
    Just jt ->
      (FValScalarInt . FValInt jt) <$>
        case op of
          Addition       -> Right $ val1 + val2
          Multiplication -> Right $ val1 * val2
          Subtraction    -> Right $ val1 - val2
          -- Division is not a homomorphism wrt. representation, so must convert first
          Division       -> Right $ fvalInt (toRuntimeRepr v1) `div`  fvalInt (toRuntimeRepr v2)
          _              -> Left $ ErrorUnsupportedExpression ""


evalBinaryOp _ _ _ = Left $ ErrorUnsupportedValue ""