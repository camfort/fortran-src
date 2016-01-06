{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Forpar.Util.Position where

import Data.Data
import Data.Typeable

import Forpar.Util.FirstParameter
import Forpar.Util.SecondParameter

class Loc a where
  getPos :: a -> Position

data Position = Position 
  { posAbsoluteOffset   :: Integer
  , posColumn           :: Integer
  , posLine             :: Integer
  } deriving (Eq, Data, Typeable)

instance Show Position where
  show (Position _ c l) = (show l) ++ ":" ++ (show c)

initPosition :: Position
initPosition = Position 
  { posAbsoluteOffset = 0
  , posColumn = 1
  , posLine = 1 
  }

data SrcSpan = SrcSpan Position Position deriving (Eq, Typeable, Data)

instance Show SrcSpan where
  show (SrcSpan s1 s2)= "(" ++ (show s1) ++ "," ++ (show s2) ++ ")"

initSrcSpan :: SrcSpan
initSrcSpan = SrcSpan initPosition initPosition

class Spanned a where
  getSpan :: a -> SrcSpan
  setSpan :: SrcSpan -> a -> a

  default getSpan :: (SecondParameter a SrcSpan) => a -> SrcSpan
  getSpan a = getSecondParameter a

  default setSpan :: (SecondParameter a SrcSpan) => SrcSpan -> a -> a
  setSpan e a = setSecondParameter e a
