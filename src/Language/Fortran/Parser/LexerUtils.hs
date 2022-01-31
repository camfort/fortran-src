{-| Utils for both lexers. -}
module Language.Fortran.Parser.LexerUtils ( readIntOrBoz ) where

import Language.Fortran.AST.Boz
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
