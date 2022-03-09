{-# LANGUAGE CPP #-}

{-| Utils for various parsers (beyond token level).

We can sometimes work around there being free-form and fixed-form versions of
the @LexAction@ monad by requesting the underlying instances instances. We place
such utilities that match that form here.

-}
module Language.Fortran.Parser.ParserUtils where

import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Complex
import Language.Fortran.Util.Position

#if !MIN_VERSION_base(4,13,0)
-- Control.Monad.Fail import is redundant since GHC 8.8.1
import Control.Monad.Fail ( MonadFail )
#endif

{- $complex-lit-parsing

Parsing complex literal parts unambiguously is a pain, so instead, we parse any
expression, then case on it to determine if it's valid for a complex literal
part -- and if so, push it into a 'ComplexPart' constructor. This may cause
unexpected behaviour if more bracketing/tuple rules are added!
-}

-- | Try to validate an expression as a COMPLEX literal part.
--
-- $complex-lit-parsing
exprToComplexLitPart :: MonadFail m => Expression a -> m (ComplexPart a)
exprToComplexLitPart e =
    case e' of
      ExpValue a ss val ->
        case val of
          ValReal    r mkp ->
            let r' = r { realLitSignificand = sign <> realLitSignificand r }
             in return $ ComplexPartReal a ss r' mkp
          ValInteger i mkp -> return $ ComplexPartInt a ss (sign<>i) mkp
          ValVariable var  -> return $ ComplexPartNamed a ss var
          _                -> fail $ "Invalid COMPLEX literal @ " <> show ss
      _ -> fail $ "Invalid COMPLEX literal @ " <> show (getSpan e')
  where
    (sign, e') = case e of ExpUnary _ _ Minus e'' -> ("-", e'')
                           ExpUnary _ _ Plus  e'' -> ("", e'')
                           _                      -> ("", e)

-- | Helper for forming COMPLEX literals.
complexLit
    :: MonadFail m => SrcSpan -> Expression A0 -> Expression A0
    -> m (Expression A0)
complexLit ss e1 e2 = do
    compReal <- exprToComplexLitPart e1
    compImag <- exprToComplexLitPart e2
    return $ ExpValue () ss $ ValComplex $ ComplexLit () ss compReal compImag
