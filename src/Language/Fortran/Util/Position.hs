{-# LANGUAGE DefaultSignatures #-}

module Language.Fortran.Util.Position where

import Data.Data
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.Binary
import Control.DeepSeq

import Language.Fortran.Util.SecondParameter

class Loc a where
  getPos :: a -> Position

data Position = Position
  { posAbsoluteOffset   :: Int
  , posColumn           :: Int
  , posLine             :: Int
  , filePath            :: String
  , posPragmaOffset     :: Maybe (Int, String)  -- ^ line-offset and filename as given by a pragma.
  } deriving (Eq, Ord, Data, Typeable, Generic)

instance Binary Position
instance NFData Position

instance Show Position where
  show (Position _ c l _ _) = show l ++ ':' : show c

initPosition :: Position
initPosition = Position
  { posAbsoluteOffset = 0
  , posColumn = 1
  , posLine = 1
  , filePath = ""
  , posPragmaOffset = Nothing
  }

lineCol :: Position -> (Int, Int)
lineCol p = (fromIntegral $ posLine p, fromIntegral $ posColumn p)

-- | (line, column) number taking into account any specified line pragmas.
apparentLineCol :: Position -> (Int, Int)
apparentLineCol (Position _ c l _ (Just (o, _))) = (l + o, c)
apparentLineCol (Position _ c l _ _)             = (l, c)

-- | Path of file taking into account any specified line pragmas.
apparentFilePath :: Position -> String
apparentFilePath p | Just (_, f) <- posPragmaOffset p = f
                   | otherwise                        = filePath p

data SrcSpan = SrcSpan Position Position deriving (Eq, Ord, Typeable, Data, Generic)

instance Binary SrcSpan
instance NFData SrcSpan
instance Show SrcSpan where
  show (SrcSpan s1 s2)= '(' : show s1 ++ ")-(" ++ show s2 ++ ")"

instance Out SrcSpan where
  doc s = text $ show s
  docPrec _ = doc

-- Difference between the column of the upper and lower positions in a span
columnDistance :: SrcSpan -> Int
columnDistance (SrcSpan (Position _ c1 _ _ _) (Position _ c2 _ _ _)) = c2 - c1

-- Difference between the lines of the upper and lower positions in a span
lineDistance :: SrcSpan -> Int
lineDistance (SrcSpan (Position _ _ l1 _ _) (Position _ _ l2 _ _)) = l2 - l1

-- List of lines that are spanned
spannedLines :: SrcSpan -> [Int]
spannedLines (SrcSpan (Position _ _ l1 _ _) (Position _ _ l2 _ _)) = [l1..l2]

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

class (Spanned a, Spanned b) => SpannedPair a b where
  getTransSpan :: a -> b -> SrcSpan

--------------------------------------------------------------------------------

instance (Spanned a) => Spanned [a] where
  getSpan [] = error "Trying to find how long an empty list spans for."
  getSpan [x]   = getSpan x
  getSpan (x:xs) = getTransSpan x (last xs)
  setSpan _ _ = error "Cannot set span to an array"

instance (Spanned a, Spanned b) => Spanned (a, Maybe b) where
  getSpan (x, Just y) = getTransSpan x y
  getSpan (x,_) = getSpan x
  setSpan _ = undefined

instance (Spanned a, Spanned b) => Spanned (Maybe a, b) where
  getSpan (Just x,y) = getTransSpan x y
  getSpan (_,y) = getSpan y
  setSpan _ = undefined

instance (Spanned a, Spanned b) => Spanned (Either a b) where
  getSpan (Left x) = getSpan x
  getSpan (Right x) = getSpan x
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b) => Spanned (a, b) where
  getSpan (x,y) = getTransSpan x y
  setSpan _ = undefined

instance {-# OVERLAPPING #-}(Spanned a, Spanned b, Spanned c) => Spanned (Maybe a, Maybe b, Maybe c) where
  getSpan (Just x,_,Just z) = getTransSpan x z
  getSpan (Just x,Just y,Nothing) = getTransSpan x y
  getSpan (Nothing,Just y,Just z) = getTransSpan y z
  getSpan (Just x,Nothing,Nothing) = getSpan x
  getSpan (Nothing,Just y,Nothing) = getSpan y
  getSpan (Nothing,Nothing,Just z) = getSpan z
  getSpan (Nothing,Nothing,Nothing) = undefined
  setSpan _ = undefined

instance {-# OVERLAPPING #-}(Spanned a, Spanned b, Spanned c) => Spanned (a, Maybe b, Maybe c) where
  getSpan (x,_,Just z) = getTransSpan x z
  getSpan (x,Just y,Nothing) = getTransSpan x y
  getSpan (x,Nothing,Nothing) = getSpan x
  setSpan _ = undefined

instance {-# OVERLAPPING #-} (Spanned a, Spanned b, Spanned c) => Spanned (Maybe a, b, c) where
  getSpan (Just x,_,z) = getTransSpan x z
  getSpan (_,y,z) = getSpan (y,z)
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b, Spanned c) => Spanned (a, b, c) where
  getSpan (x,_,z) = getTransSpan x z
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b) => SpannedPair a b where
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y

instance {-# OVERLAPS #-} (Spanned a, Spanned b) => SpannedPair a [b] where
  getTransSpan x [] = getSpan x
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y

instance {-# OVERLAPS #-} (Spanned a, Spanned b) => SpannedPair a [[b]] where
  getTransSpan x [] = getSpan x
  getTransSpan x y | all null y = getSpan x
  getTransSpan x y | any null y = getTransSpan x (filter (not . null) y)
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y
