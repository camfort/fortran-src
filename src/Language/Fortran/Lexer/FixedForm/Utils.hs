module Language.Fortran.Lexer.FixedForm.Utils where

import           Language.Fortran.Lexer.FixedForm
import           Language.Fortran.AST
import           Language.Fortran.AST.RealLit
import           Language.Fortran.Util.Position

makeReal :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe (SrcSpan, String) -> Expression A0
makeReal i1 dot i2 expr =
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
