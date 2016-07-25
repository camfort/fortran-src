{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Fortran.Pretty where

import Prelude hiding (EQ,LT,GT)
import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Text.PrettyPrint

class Pretty t where
   pprint :: FortranVersion -> t -> Doc

-- A subset of Value permit the 'FirstParameter' operation
instance FirstParameter (Value a) String
instance (FirstParameter (Value a) String, Pretty (Expression a))
       => Pretty (Value a) where
    pprint v ValStar       = char '*'
    pprint v ValAssignment = text "assignment (=)"
    pprint v (ValComplex e1 e2) =
        parens $ commaSep [pprint v e1, pprint v e2]
    pprint v valLit =
        text . getFirstParameter $ valLit

instance Pretty (Expression a) => Pretty (Index a) where
    pprint v (IxSingle _ s Nothing e) = pprint v e
    -- This is an intermediate expression form which shouldn't make it
    -- to the pretty printer
    pprint v (IxSingle _ s (Just _) e) = pprint v e
    pprint v (IxRange _ s low up stride) = undefined

instance (Pretty (Argument a), Pretty (Value a)) => Pretty (Expression a) where
    pprint v (ExpValue _ s val)  =
         pprint v val
    pprint v (ExpBinary _ s op e1 e2) =
        floatDoc s $ pprint v e1 <+> pprint v op <+> pprint v e2
    pprint v (ExpUnary _ s op e) =
        floatDoc s $ pprint v op <+> pprint v e
    pprint v (ExpSubscript _ s e ixs) =
        floatDoc s $ pprint v e
                 <> parens (commaSep (map (pprint v) (aStrip ixs)))
    pprint v (ExpDataRef _ s e1 e2) =
        floatDoc s $ pprint v e1 <+> char '%' <+> pprint v e2
    pprint v (ExpFunctionCall _ s e mes) =
        floatDoc s $ pprint v e
                 <> parens (commaSep (map (pprint v) (maybe [] aStrip mes)))
    --pprint v (ExpImpliedDo _ s es dospec) = undefined
    pprint v _ = error "Unsupported"

instance Pretty (Argument a) where
    pprint v _ = error "Unsupported"

instance Pretty (Expression a) => Pretty (Statement a) where
    pprint v (StExpressionAssign _ span e1 e2) = empty

instance Pretty BinaryOp where
    pprint v Addition       = char '+'
    pprint v Subtraction    = char '-'
    pprint v Multiplication = char '*'
    pprint v Division       = char '/'
    pprint v Exponentiation = text "**"
    pprint v Concatenation  = text "//"
    pprint v GT  = text $ if v77orLess v then ".gt." else ">"
    pprint v LT  = text $ if v77orLess v then ".lt." else "<"
    pprint v LTE = text $ if v77orLess v then ".le." else "<="
    pprint v GTE = text $ if v77orLess v then ".ge." else ">="
    pprint v EQ  = text $ if v77orLess v then ".eq." else "=="
    pprint v NE  = text $ if v77orLess v then ".ne." else "!="
    pprint v Or  = text $ ".or."
    pprint v And = text $ ".and."
    pprint v Equivalent = text $ ".eqv."
    pprint v NotEquivalent = text $ ".neqv."
    pprint v (BinCustom custom) = text $ "." ++ custom ++ "."

instance Pretty UnaryOp where
    pprint v Plus  = char '+'
    pprint v Minus = char '-'
    pprint v Not   = text ".not."
    pprint v (UnCustom custom) = text $ "." ++ custom ++ "."

commaSep :: [Doc] -> Doc
commaSep = vcat . punctuate (comma <> space)

floatDoc :: SrcSpan -> Doc -> Doc
floatDoc span d | lineDistance span == 0 =
   -- If the rendered pretty print is less than the width of
   -- the span, then pad to the end with spaces
   if (length (render d)) < (columnDistance span)
   then vcat (d : (replicate ((columnDistance span) - (length (render d))) space))
   else d

-- Difficult to know what to dif line distance is non-zero
floatDoc span d | otherwise = d