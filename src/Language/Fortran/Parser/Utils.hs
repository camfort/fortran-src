{-| Simple module to provide functions that read Fortran literals -}
module Language.Fortran.Parser.Utils (readReal, readInteger) where
import Data.Char
import Numeric

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
