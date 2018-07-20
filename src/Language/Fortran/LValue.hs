{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Fortran.LValue where

import           Prelude                               hiding (exp)
import           Data.Data
import           GHC.Generics                          (Generic)

import           Language.Fortran.AST
import           Language.Fortran.Util.FirstParameter
import           Language.Fortran.Util.Position
import           Language.Fortran.Util.SecondParameter

-- | A subset of 'Expression' which can only contain values that can be assigned
-- to.
data LValue a
  = LvSimpleVar a SrcSpan Name
  | LvSubscript a SrcSpan (LValue a) (AList Index a)
  | LvDataRef a SrcSpan (LValue a) (LValue a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)


-- | If the expression can be seen as an lvalue, convert it to an 'LValue'.
toLValue :: Expression a -> Maybe (LValue a)
toLValue (ExpValue ann sp (ValVariable nm)) = Just (LvSimpleVar ann sp nm)
toLValue (ExpSubscript ann sp exp ixs) = LvSubscript ann sp <$> toLValue exp <*> pure ixs
toLValue (ExpDataRef ann sp lhs rhs) = LvDataRef ann sp <$> toLValue lhs <*> toLValue rhs
toLValue _ = Nothing

instance FirstParameter (LValue a) a
instance SecondParameter (LValue a) SrcSpan

instance Annotated LValue
instance Spanned (LValue a)
