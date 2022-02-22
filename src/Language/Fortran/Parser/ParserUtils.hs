{-# LANGUAGE CPP #-}

{-| Utils for various parsers (beyond token level).

We can sometimes work around there being free-form and fixed-form versions of
the @LexAction@ monad by requesting the underlying instances instances. We place
such utilities that match that form here.

-}
module Language.Fortran.Parser.ParserUtils where

import Language.Fortran.AST
import Language.Fortran.AST.RealLit
import Language.Fortran.Util.Position

#if !MIN_VERSION_base(4,13,0)
-- Control.Monad.Fail import is redundant since GHC 8.8.1
import Control.Monad.Fail ( MonadFail )
#endif

{- $complex-lit-parsing

Complex literals use a potentially misleading tuple syntax. The things allowed
in each part are highly restricted, where you might otherwise be used to writing
expressions. The actual allowed forms (taken from the F90 standard and
gfortran's behaviour) are specified by the structure, but parsing them
unambiguously is a pain.

We parse any @(expr, expr)@, then case on each expression to determine where it
was in fact valid for a complex literal -- and if so, push it into a
'ComplexPart' constructor. This may cause unexpected behaviour if more
bracketing/tuple rules are added!
-}

-- | Try to validate an expression as a COMPLEX literal part.
--
-- $complex-lit-parsing
exprToComplexLitPart :: MonadFail m => Expression a -> m (ComplexPart a)
exprToComplexLitPart e =
    case e' of
      ExpValue a ss v ->
        case v of
          ValReal    r mkp ->
            let r' = r { realLitSignificand = sign <> realLitSignificand r }
             in return $ ComplexPartReal a ss r' mkp
          ValInteger i mkp -> return $ ComplexPartInt a ss (sign<>i) mkp
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
    return $ ExpValue () ss $ ValComplex ss compReal compImag
