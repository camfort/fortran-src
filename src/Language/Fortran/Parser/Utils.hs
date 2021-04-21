{-# LANGUAGE LambdaCase #-}

{-| Simple module to provide functions that read Fortran literals -}
module Language.Fortran.Parser.Utils
  ( readReal
  , readInteger
  , parseRealLiteral
  , RealLit(..)
  , Exponent(..)
  , NumSign(..)
  , ExponentLetter(..)
  ) where

import Language.Fortran.AST (Kind)

import Data.Char
import Numeric
import Text.Read (readMaybe)

breakAtDot :: String -> (String, String)
replaceDwithE :: Char -> Char
readsToMaybe :: [(a, b)] -> Maybe a
fixAtDot :: (String, String) -> (String, String)
fixAtDot' :: (String, String) -> (String, String)
combineAtDot :: (String, String) -> String

-- | Convert a Fortran literal Real into a Haskell Double.
readReal :: String -> Maybe Double
readReal = readsToMaybe . reads . filter (/= '+') . combineAtDot . fixAtDot . breakAtDot . map replaceDwithE . takeWhile (/= '_')

-- | Convert a Fortran literal Integer into a Haskell Integer.
readInteger :: String -> Maybe Integer
readInteger s = readsToMaybe $ case s' of
  'b':_ -> readInt 2 (`elem` "01") digitToInt (drop 2 s')
  'o':_ -> readInt 8 (`elem` ['0'..'7']) digitToInt (drop 2 s')
  'z':_ -> readInt 16 (`elem` (['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f'])) digitToInt (drop 2 s')
  _     -> readSigned readDec s'
  where
    s' = filter (/= '+') . takeWhile (/= '_') $ s

fixAtDot' ("", r)                      = ("0", r)
fixAtDot' ("-", r)                     = ("-0", r)
fixAtDot' (l, "")                      = (l, "0")
fixAtDot' (l, r0:r) | not (isDigit r0) = (l, '0':r0:r)
fixAtDot' x                            = x

combineAtDot (a, b) = a ++ "." ++ b
fixAtDot x
  | x == x'         = x
  | otherwise       = fixAtDot x' where x' = fixAtDot' x
breakAtDot          = fmap (drop 1) . break (=='.')
replaceDwithE 'd'   = 'e'
replaceDwithE c     = c

readsToMaybe r = case r of
  (x, _):_ -> Just x
  _ -> Nothing

--------------------------------------------------------------------------------

-- TODO incorrect, kind params also allow 'Name's
type KindParam = Kind

-- | A REAL literal may have an optional exponent and kind.
--
-- The value can be retrieved as a 'Double' by using these parts.
data RealLit = RealLit
  { realLitValue     :: String
  , realLitExponent  :: Maybe Exponent
  , realLitKindParam :: Maybe KindParam
  } deriving Show

-- | An exponent is an exponent letter (E, D) and a value (with an optional
-- sign).
data Exponent = Exponent
  { expLetter :: ExponentLetter
  , expSign   :: Maybe NumSign
  , expNum    :: String
  } deriving Show

-- Note: Some Fortran language references include extensions here. HP's F90
-- reference provides a Q exponent letter which sets kind to 16.
data ExponentLetter
  = ExpLetterD
  | ExpLetterE
    deriving Show

data NumSign
  = SignPos
  | SignNeg
    deriving Show

-- | Parse a Fortran literal real to its constituent parts.
parseRealLiteral :: String -> RealLit
parseRealLiteral r =
    RealLit { realLitValue     = takeWhile isValuePart r
            , realLitExponent  = parseRealLitExponent (dropWhile isValuePart r)
            , realLitKindParam = parseRealLitKindInt (dropWhile (/= '_') r)
            }
  where
    isValuePart :: Char -> Bool
    isValuePart ch
      | isDigit ch = True
      | ch == '.'  = True
      | otherwise  = False
    parseRealLitKindInt :: String -> Maybe Kind
    parseRealLitKindInt = \case
      '_':chs -> readMaybe chs
      _       -> Nothing
    parseRealLitExponent :: String -> Maybe Exponent
    parseRealLitExponent "" = Nothing
    parseRealLitExponent (c:cs) = do
        letter <-
                case toLower c of
                  'e' -> Just ExpLetterE
                  'd' -> Just ExpLetterD
                  _   -> Nothing
        let (sign, cs'') =
                case cs of
                  ""       -> (Nothing, cs)
                  c':cs'  -> -- TODO: want to locally scope cs'' but unsure how to??
                    case c' of
                      '-' -> (Just SignNeg, cs')
                      '+' -> (Just SignPos, cs')
                      _   -> (Nothing     , cs)
            digitStr = read (takeWhile isDigit cs'')
        return $ Exponent letter sign digitStr
