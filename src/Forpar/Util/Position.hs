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
  } deriving (Show, Eq, Data, Typeable)

initPosition :: Position
initPosition = Position 
  { posAbsoluteOffset = 0
  , posColumn = 1
  , posLine = 1 
  }

data SrcLoc = SrcLoc 
  { locPosition   :: Position
  , locFilename   :: String
  } deriving (Show, Eq, Typeable, Data)

data SrcSpan = SrcSpan SrcLoc SrcLoc deriving (Eq, Show, Typeable, Data)
