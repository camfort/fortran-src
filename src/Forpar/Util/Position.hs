{-# LANGUAGE DeriveDataTypeable #-}

module Forpar.Util.Position where

import Data.Data
import Data.Typeable

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

data SrcLoc = SrcLoc 
  { locPosition   :: Position
  , locFilename   :: String
  } deriving (Eq, Typeable, Data)

instance Show SrcLoc where
  show (SrcLoc p f) = (show f) ++ ":" ++ (show p)

initSrcLoc :: SrcLoc
initSrcLoc = SrcLoc 
  { locPosition = initPosition
  , locFilename = "<unknown>" 
  }

data SrcSpan = SrcSpan SrcLoc SrcLoc deriving (Eq, Typeable, Data)

instance Show SrcSpan where
  show (SrcSpan s1 s2)= "(" ++ (show s1) ++ "," ++ (show s2) ++ ")"

initSrcSpan :: SrcSpan
initSrcSpan = SrcSpan initSrcLoc initSrcLoc
