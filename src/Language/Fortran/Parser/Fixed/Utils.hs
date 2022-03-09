{-# LANGUAGE CPP #-}
module Language.Fortran.Parser.Fixed.Utils where

import Language.Fortran.Parser.Fixed.Lexer
import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real
import Language.Fortran.Util.Position
import Language.Fortran.Parser.Monad
import Control.Monad.State

-- | UNSAFE. Must be called with expected token types (see usage sites). Will
--   cause a runtime exception if it doesn't form a valid REAL literal.
makeRealLit
    :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe (SrcSpan, String)
    -> Expression A0
makeRealLit i1 dot i2 expr =
  let span1   = getSpan (i1, dot, i2)
      span2   = case expr of
                  Just e -> getTransSpan span1 (fst e)
                  Nothing -> span1
      i1Str   = case i1 of { Just (TInt _ s) -> s ; _ -> "" }
      dotStr  = case dot of { Just (TDot _) -> "." ; _ -> "" }
      i2Str   = case i2 of { Just (TInt _ s) -> s ; _ -> "" }
      exprStr  = case expr of { Just (_, s) -> s ; _ -> "" }
      litStr  = i1Str ++ dotStr ++ i2Str ++ exprStr
   in ExpValue () span2 $ ValReal (parseRealLit litStr) Nothing

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

convCmts :: [Block a] -> [ProgramUnit a]
convCmts = map convCmt
  where convCmt (BlComment a s c) = PUComment a s c
        convCmt _ = error "convCmt applied to something that is not a comment"
