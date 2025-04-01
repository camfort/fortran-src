{-| Utils for both lexers. -}
module Language.Fortran.Parser.LexerUtils ( readIntOrBoz, unescapeSpecialChars) where

import Language.Fortran.AST.Literal.Boz
import Numeric

-- | Read a string as either a signed integer, or a BOZ constant (positive).
--
-- Useful in manual lexing.
readIntOrBoz :: String -> Integer
readIntOrBoz s = do
    case readSToMaybe $ readSigned readDec s of
      Just int -> int
      Nothing  -> bozAsNatural $ parseBoz s

readSToMaybe :: [(a, b)] -> Maybe a
readSToMaybe = \case (x, _):_ -> Just x
                     _        -> Nothing


-- | Pretty prints exception message that contains things like carriage return, indents, etc.
unescapeSpecialChars :: String -> String
unescapeSpecialChars [] = []
unescapeSpecialChars ('\\' : c : rest) =
  case c of
    'n'  -> '\n' : unescapeSpecialChars rest
    't'  -> '\t' : unescapeSpecialChars rest
    'r'  -> '\r' : unescapeSpecialChars rest
    '\\' -> '\\' : unescapeSpecialChars rest
    _    -> '\\' : c : unescapeSpecialChars rest
unescapeSpecialChars (c : rest) =
  c : unescapeSpecialChars rest
