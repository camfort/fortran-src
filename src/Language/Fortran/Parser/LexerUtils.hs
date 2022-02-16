{-| Utils for both lexers. -}
module Language.Fortran.Parser.LexerUtils ( readIntOrBoz ) where

import Language.Fortran.AST.Literal.Boz
import Numeric

-- | Read a string as either a signed integer, or a BOZ constant.
--
-- Useful in manual lexing.
readIntOrBoz :: String -> Int
readIntOrBoz s = do
    case readSToMaybe $ readSigned readDec s of
      Just int -> int
      Nothing  -> bozAsInteger $ unsafeParseBoz s

readSToMaybe :: [(a, b)] -> Maybe a
readSToMaybe = \case (x, _):_ -> Just x
                     _        -> Nothing
