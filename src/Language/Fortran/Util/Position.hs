{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran.Util.Position where

import Data.Data
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.Binary

import Language.Fortran.Util.SecondParameter

class Loc a where
  getPos :: a -> Position

data Position = Position
  { posAbsoluteOffset   :: Int
  , posColumn           :: Int
  , posLine             :: Int
  , filePath            :: String
  } deriving (Eq, Ord, Data, Typeable, Generic)

instance Binary Position

instance Show Position where
  show (Position _ c l _) = show l ++ ':' : show c

initPosition :: Position
initPosition = Position
  { posAbsoluteOffset = 0
  , posColumn = 1
  , posLine = 1
  , filePath = ""
  }

lineCol :: Position -> (Int, Int)
lineCol p  = (fromIntegral $ posLine p, fromIntegral $ posColumn p)

data SrcSpan = SrcSpan Position Position deriving (Eq, Ord, Typeable, Data, Generic)

instance Binary SrcSpan

instance Show SrcSpan where
  show (SrcSpan s1 s2)= '(' : show s1 ++ ")-(" ++ show s2 ++ ")"

instance Out SrcSpan where
  doc s = text $ show s
  docPrec _ = doc

-- Difference between the column of the upper and lower positions in a span
columnDistance :: SrcSpan -> Int
columnDistance (SrcSpan (Position _ c1 _ _) (Position _ c2 _ _)) = c2 - c1

-- Difference between the lines of the upper and lower positions in a span
lineDistance :: SrcSpan -> Int
lineDistance (SrcSpan (Position _ _ l1 _) (Position _ _ l2 _)) = l2 - l1

initSrcSpan :: SrcSpan
initSrcSpan = SrcSpan initPosition initPosition

instance Spanned SrcSpan where
  getSpan s = s
  setSpan _ _ = undefined

class Spanned a where
  getSpan :: a -> SrcSpan
  setSpan :: SrcSpan -> a -> a

  default getSpan :: (SecondParameter a SrcSpan) => a -> SrcSpan
  getSpan = getSecondParameter

  default setSpan :: (SecondParameter a SrcSpan) => SrcSpan -> a -> a
  setSpan = setSecondParameter
