{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Pretty where

import Data.Char
import Prelude hiding (EQ,LT,GT)
import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Control.Monad (void)

import Text.PrettyPrint

class Pretty t where
   pprint :: FortranVersion -> t -> Doc

instance Pretty a => Pretty (Maybe a) where
    pprint v Nothing  = empty
    pprint v (Just e) = pprint v e

instance (Pretty (e a)) => Pretty (AList e a) where
    pprint v es = commaSep (map (pprint v) (aStrip es))

instance Pretty BaseType where
    pprint v TypeInteger = "integer"
    pprint v TypeReal    = "real"
    pprint v TypeDoublePrecision = "double precision"
    pprint v TypeComplex = "complex"
    pprint Fortran77Extended TypeDoubleComplex = "DOUBLECOMPLEX"
    pprint v TypeLogical = "logical"
    pprint v TypeCharacter = "character"
    pprint v (TypeCustom str) = "type(" <> text str <> ")"

instance Pretty (TypeSpec a) where
    pprint v (TypeSpec _ a basetype mselector) =
      pprint v basetype <>
      case mselector of
        Nothing -> empty
        Just (Selector _ s Nothing Nothing) -> empty
        Just (Selector _ s mlength mkind)   ->
          case basetype of
            -- Character type spec
            TypeCharacter ->
              parens
                (maybe empty (\e -> "len=" <> pprint v e) mlength
              <> maybe empty (\k -> comma <+> "kind=" <> pprint v k) mkind)
            -- Non character type spec
            _ -> parens (maybe empty (\k -> "kind=" <> pprint v k) mkind)

instance Pretty (Selector a) where
  pprint Fortran66 s =
    error "Not possible to pretty print kind/length selectors for Fortran 66."

  pprint Fortran77 s = pprint Fortran77Extended s

  pprint Fortran77Extended (Selector _ _ mLenSel mKindSel)
    | (Just lenSel, Nothing) <- (mLenSel, mKindSel) =
        char '*' <+> parens (pprint Fortran77Extended lenSel)
    | (Nothing, Just kindSel) <- (mLenSel, mKindSel) =
        char '*' <+> parens (pprint Fortran77Extended kindSel)
    | otherwise =
        error "Kind and length selectors can be active one at a time in\
              \Fortran 77."

  pprint Fortran90 (Selector _ _ mLenSel mKindSel)
    | (Just lenSel, Just kindSel) <- (mLenSel, mKindSel) =
      parens $ len lenSel <> char ',' <+> kind kindSel
    | (Nothing, Just kindSel) <- (mLenSel, mKindSel) = parens $ kind kindSel
    | (Just lenDev, Nothing) <- (mLenSel, mKindSel) = parens $ len lenDev
    | otherwise =
        error "No way for both kind and length selectors to be empty in \
              \Fortran 90."
    where
      len e  = "len=" <> pprint Fortran90 e
      kind e = "kind=" <> pprint Fortran90 e

instance Pretty (Expression a) => Pretty (Statement a) where
    pprint v (StDeclaration _ s typespec attributes declarators) = empty
    pprint v (StExpressionAssign _ span e1 e2) = empty
    pprint v _ = empty

instance Pretty (Expression a) => Pretty (Use a) where
    pprint Fortran90 (UseRename _ _ uSrc uDst) =
      pprint Fortran90 uSrc <+> "=>" <+> pprint Fortran90 uDst
    pprint Fortran90 (UseID _ _ u) = pprint Fortran90 u

    pprint _ _ = error "Module system is introduced in Fortran 90."

instance Pretty (Argument a) where
    pprint v (Argument _ s key e) = floatDoc s $
       case key of
         Just keyName -> text keyName <+> char '=' <+> pprint v e
         Nothing      -> pprint v e

instance Pretty (DimensionDeclarator a) => Pretty (Attribute a) where
    pprint Fortran90 (AttrParameter _ _)   = "parameter"
    pprint Fortran90 (AttrPublic    _ _)   = "public"
    pprint Fortran90 (AttrPrivate   _ _)   = "private"
    pprint Fortran90 (AttrAllocatable _ _) = "allocatable"
    pprint Fortran90 (AttrDimension _ _ dims) =
      "dimesion" <> parens (pprint Fortran90 dims)
    pprint Fortran90 (AttrExternal _ _)    = "external"
    pprint Fortran90 (AttrIntent _ _ i)    =
      "intent" <> parens (pprint Fortran90 i)
    pprint Fortran90 (AttrIntrinsic _ _)   = "intrinsic"
    pprint Fortran90 (AttrOptional _ _)    = "optional"
    pprint Fortran90 (AttrPointer _ _)     = "pointer"
    pprint Fortran90 (AttrSave _ _)        = "save"
    pprint Fortran90 (AttrTarget _ _)      = "target"
    pprint _ _ = error "Attributes are introduced in Fortran 90."

instance Pretty Intent where
    pprint Fortran90 In = "in"
    pprint Fortran90 Out = "out"
    pprint Fortran90 InOut = "in out"
    pprint _ _ = error "Attributes are introduced in Fortran90."

instance (Pretty (Expression a)) => Pretty (DoSpecification a) where
    pprint v (DoSpecification _ s e0assign en stride) =
      case e0assign of
        s@StExpressionAssign{} ->
          pprint v s <> comma <+> pprint v en
                     <> maybe empty (\e -> comma <> pprint v e) stride

        _ -> error $ "Malformed syntax tree: do-specification has non-assignment\
                      \statement at location " ++ show s

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
        floatDoc s $ pprint v e <> parens (maybe empty (pprint v) mes)

    pprint v (ExpImpliedDo _ s es dospec) =
        floatDoc s $ pprint v es <> comma <+> pprint v dospec

    pprint v (ExpInitialisation _ s es) =
        floatDoc s $ "(/" <> pprint v es <> "/)"

    pprint v (ExpReturnSpec _ s e) =
        floatDoc s $ char '*' <> pprint v e

instance Pretty (Expression a) => Pretty (Index a) where
    pprint v (IxSingle _ s Nothing e) = pprint v e
    -- This is an intermediate expression form which shouldn't make it
    -- to the pretty printer
    pprint v (IxSingle _ s (Just _) e) = pprint v e
    pprint v (IxRange _ s low up stride) =
       low' <> char ':' <> up' <> stride'
      where
        low' = maybe empty (pprint v) low
        up'  = maybe empty (pprint v) up
        stride' =  maybe empty (\e -> char ':' <> pprint v e) stride

-- A subset of Value permit the 'FirstParameter' operation
instance FirstParameter (Value a) String
instance (FirstParameter (Value a) String, Pretty (Expression a))
       => Pretty (Value a) where
    pprint v ValStar       = char '*'
    pprint v ValAssignment = "assignment (=)"
    pprint v (ValComplex e1 e2) =
        parens $ commaSep [pprint v e1, pprint v e2]
    pprint v (ValString str) =
        char '"' <> text str <> char '"'
    pprint v valLit =
        text . getFirstParameter $ valLit

instance Pretty (Expression a) => Pretty (Declarator a) where
    pprint v (DeclVariable _ s e Nothing (Just e')) =
        pprint v e <+> char '=' <+> pprint v e'
    pprint v (DeclVariable _ s e Nothing Nothing) =
        pprint v e
    pprint v (DeclVariable _ s e (Just (ExpValue _ _ ValStar)) Nothing) =
        pprint v e <+> char '*' <+> parens (char '*')
    pprint v (DeclVariable _ s e (Just len) Nothing) =
        pprint v e <+> char '*' <+> pprint v len
    pprint v (DeclVariable _ s e (Just len) (Just e')) =
        error $ "Malformed syntax tree: decl-variable has both length and\
              \and initial value at " ++ show s
    pprint v (DeclArray _ s ae dims Nothing Nothing) =
        pprint v ae <> parens (pprint v dims)

    pprint v (DeclArray _ s ae dims (Just (ExpValue _ _ ValStar)) Nothing) =
        pprint v ae <> parens (pprint v dims) <> char '*' <> parens (char '*')

    pprint v (DeclArray _ s ae dims (Just len) Nothing) =
        pprint v ae <> parens (pprint v dims) <> char '*' <> pprint v len

    pprint v (DeclArray _ s ae dims len init) =
        error $ "Malformed syntax tree: decl-array has init value at " ++ show s

instance Pretty (DimensionDeclarator a) where
    pprint v (DimensionDeclarator _ _ me1 me2) =
      pprint v me1 <> maybe empty (const $ char ':') me1 <> pprint v me2

instance Pretty UnaryOp where
    pprint v Plus  = char '+'
    pprint v Minus = char '-'
    pprint v Not   = ".not."
    pprint v (UnCustom custom) = text $ "." ++ custom ++ "."

instance Pretty BinaryOp where
    pprint v Addition       = char '+'
    pprint v Subtraction    = char '-'
    pprint v Multiplication = char '*'
    pprint v Division       = char '/'
    pprint v Exponentiation = "**"
    pprint v Concatenation  = "//"
    pprint v GT  = if v77orLess v then ".gt." else ">"
    pprint v LT  = if v77orLess v then ".lt." else "<"
    pprint v LTE = if v77orLess v then ".le." else "<="
    pprint v GTE = if v77orLess v then ".ge." else ">="
    pprint v EQ  = if v77orLess v then ".eq." else "=="
    pprint v NE  = if v77orLess v then ".ne." else "!="
    pprint v Or  = ".or."
    pprint v And = ".and."
    pprint v Equivalent = ".eqv."
    pprint v NotEquivalent = ".neqv."
    pprint v (BinCustom custom) = text $ "." ++ custom ++ "."

commaSep :: [Doc] -> Doc
commaSep = vcat . punctuate (comma <> space)

floatDoc :: SrcSpan -> Doc -> Doc
floatDoc span d | lineDistance span == 0 =
   -- If the rendered pretty print is less than the width of
   -- the span, then pad to the end with spaces
   if length (render d) < columnDistance span
   then vcat (d : replicate (columnDistance span - length (render d)) space)
   else d

-- Difficult to know what to dif line distance is non-zero
floatDoc span d = d
