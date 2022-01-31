{-# LANGUAGE CPP #-}
module Language.Fortran.Parser.Free.Utils where

import Language.Fortran.Parser.Free.Lexer
import Language.Fortran.Parser.Monad
import Control.Monad.State

unitNameCheck :: Token -> String -> Parse AlexInput Token ()
unitNameCheck (TId _ name1) name2
  | name1 == name2 = return ()
  | otherwise = fail "Unit name does not match the corresponding END statement."
unitNameCheck _ _ = return ()

parseError :: Token -> LexAction a
parseError _ = do
    parseState <- get
#ifdef DEBUG
    tokens <- reverse <$> aiPreviousTokensInLine <$> getAlex
#endif
    fail $ psFilename parseState ++ ": parsing failed. "
#ifdef DEBUG
      ++ '\n' : show tokens
#endif
