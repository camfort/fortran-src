{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Pretty where

import Data.Char
import Data.Maybe (isJust)
import Prelude hiding (EQ,LT,GT)
import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Control.Monad (void)

import Text.PrettyPrint

unsupported :: FortranVersion -> String -> a
unsupported fv msg = error $
    msg ++ " You called pretty print with " ++ show fv ++ "."

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
    pprint v TypeDoubleComplex
      | v == Fortran77Extended = "double complex"
      | otherwise =
        unsupported v "Double Complex is an unofficial Fortran 77 extension."
    pprint v TypeLogical = "logical"
    pprint v TypeCharacter
      | v >= Fortran77 = "character"
      | otherwise =
        unsupported v "Character data type is introduced in Fortran 77."
    pprint v (TypeCustom str)
      | v >= Fortran90 = "type" <+> parens (text str)
      | otherwise =
        unsupported v "User defined types are introduced in Fortran 90."

instance Pretty (TypeSpec a) where
    pprint v (TypeSpec _ _ baseType mSelector) =
      pprint v baseType <>
      if isJust mSelector then space <> pprint v mSelector else empty

instance Pretty (Selector a) where
  pprint v (Selector _ _ mLenSel mKindSel)
    | v < Fortran77 =
      unsupported Fortran66 "Length and kind selectors are introduced in \
                            \Fortran 77 and 90 respectively."

    | v < Fortran90 =
      case (mLenSel, mKindSel) of
        (Just lenSel, Nothing) ->
          char '*' <+> parens (pprint Fortran77Extended lenSel)
        (Nothing, Just kindSel) ->
          char '*' <+> parens (pprint Fortran77Extended kindSel)
        _ -> error "Kind and length selectors can be active one at a time in\
                   \Fortran 77."

    | v >= Fortran90 =
      case (mLenSel, mKindSel) of
        (Just lenSel, Just kindSel) ->
          parens $ len lenSel <> char ',' <+> kind kindSel
        (Nothing, Just kindSel) -> parens $ kind kindSel
        (Just lenDev, Nothing) -> parens $ len lenDev
        _ -> error "No way for both kind and length selectors to be empty in \
                   \Fortran 90 onwards."
    where
      len e  = "len=" <> pprint Fortran90 e
      kind e = "kind=" <> pprint Fortran90 e

instance (Pretty (Expression a), Pretty Intent) => Pretty (Statement a) where
    pprint v st@(StDeclaration _ s typeSpec mAttrList declList)
      | v < Fortran90 = pprint v typeSpec <+> pprint v declList
      | v >= Fortran90 =
          pprint v typeSpec <>
          (if isJust mAttrList then comma else empty) <+>
          pprint v mAttrList <+>
          text "::" <+>
          pprint v declList

    pprint v (StIntent _ _ intent exps)
      | v >= Fortran90 =
          "intent" <+> parens (pprint v intent) <+> "::" <+> pprint v exps
      | otherwise =
        unsupported v "Intent statement is introduced in Fortran 90."

    pprint v (StOptional _ _ vars)
      | v >= Fortran90 = "optional ::" <+> pprint v vars
      | otherwise =
        unsupported v "Optional statement is introduced in Fortran 90."

    pprint v (StPublic _ _ mVars)
      | v >= Fortran90 =
        "public" <>
        if isJust mVars then " :: " <> pprint v mVars else empty
      | otherwise =
        unsupported v "Public statement is introduced in Fortran 90."

    pprint v (StPrivate _ _ mVars)
      | v >= Fortran90 =
        "private" <>
        if isJust mVars then " :: " <> pprint v mVars else empty
      | otherwise =
        unsupported v "Private statement is introduced in Fortran 90."

    pprint v (StSave _ _ mVars)
      | v >= Fortran90 =
        "save" <>
        if isJust mVars then " :: " <> pprint v mVars else empty
      | otherwise = hang "save" 1 (pprint v mVars)

    pprint v (StDimension _ _ decls)
      | v >= Fortran90 = "dimension ::" <+> pprint v decls
      | otherwise = "dimension" <+> pprint v decls

    pprint v (StAllocatable _ _ decls)
      | v >= Fortran90 = "allocatable ::" <+> pprint v decls
      | otherwise =
        unsupported v "Allocatable statement is introduced in Fortran 90."

    pprint v (StPointer _ _ decls)
      | v >= Fortran90 = "pointer ::" <+> pprint v decls
      | otherwise =
        unsupported v "Pointer statement is introduced in Fortran 90."

    pprint v (StTarget _ _ decls)
      | v >= Fortran90 = "target ::" <+> pprint v decls
      | otherwise =
        unsupported v "Target statement is introduced in Fortran 90."

    pprint v (StData _ _ aDataGroups@(AList _ _ dataGroups))
      | v >= Fortran90 = "data" <+> pprint v aDataGroups
      | otherwise = "data" <+> hsep (map (pprint v) dataGroups)

    pprint v (StNamelist _ _ namelist)
      | v >= Fortran90 = "namelist" <+> pprint v namelist
      | otherwise =
        unsupported v "Namelist statement is introduced in Fortran 90."

    pprint v (StParameter _ _ aDecls) = "parameter" <+> parens (pprint v aDecls)

    pprint v (StExternal _ _ vars) = "external" <+> pprint v vars
    pprint v (StIntrinsic _ _ vars) = "intrinsic" <+> pprint v vars

    pprint v (StCommon _ _ aCommonGroups) = "common" <+> pprint v aCommonGroups

    pprint v (StEquivalence _ _ (AList _ _ equivGroups)) =
      "equivalence" <+> commaSep (map (parens . pprint v) equivGroups)

    pprint v (StFormat _ _ (AList _ _ formatItems)) =
      "format" <+> hcat (map (pprint v) formatItems)

    pprint _ _ = empty
{-
    pprint v (StImplicit _ s s3) = _
    pprint v (StEntry _ s s3 s4 s5) = _
    pprint v (StInclude _ s s3) = _
    pprint v (StDo _ s s3 s4 s5) = _
    pprint v (StDoWhile _ s s3 s4 s5) = _
    pprint v (StEnddo _ s s3) = _
    pprint v (StCycle _ s s3) = _
    pprint v (StExit _ s s3) = _
    pprint v (StIfLogical _ s s3 s4) = _
    pprint v (StIfArithmetic _ s s3 s4 s5 s6) = _
    pprint v (StIfThen _ s s3 s4) = _
    pprint v (StElse _ s s3) = _
    pprint v (StElsif _ s s3 s4) = _
    pprint v (StEndif _ s s3) = _
    pprint v (StSelectCase _ s s3 s4) = _
    pprint v (StCase _ s s3 s4) = _
    pprint v (StEndcase _ s s3) = _
    pprint v (StFunction _ s s3 s4 s5) = _
    pprint v (StExpressionAssign _ s s3 s4) = _
    pprint v (StPointerAssign _ s s3 s4) = _
    pprint v (StLabelAssign _ s s3 s4) = _
    pprint v (StGotoUnconditional _ s s3) = _
    pprint v (StGotoAssigned _ s s3 s4) = _
    pprint v (StGotoComputed _ s s3 s4) = _
    pprint v (StCall _ s s3 s4) = _
    pprint v (StReturn _ s s3) = _
    pprint v (StContinue _ s) = _
    pprint v (StStop _ s s3) = _
    pprint v (StPause _ s s3) = _
    pprint v (StRead _ s s3 s4) = _
    pprint v (StRead2 _ s s3 s4) = _
    pprint v (StWrite _ s s3 s4) = _
    pprint v (StPrint _ s s3 s4) = _
    pprint v (StOpen _ s s3) = _
    pprint v (StClose _ s s3) = _
    pprint v (StInquire _ s s3) = _
    pprint v (StRewind _ s s3) = _
    pprint v (StRewind2 _ s s3) = _
    pprint v (StBackspace _ s s3) = _
    pprint v (StBackspace2 _ s s3) = _
    pprint v (StEndfile _ s s3) = _
    pprint v (StEndfile2 _ s s3) = _
    pprint v (StAllocate _ s s3 s4) = _
    pprint v (StNullify _ s s3) = _
    pprint v (StDeallocate _ s s3 s4) = _
    pprint v (StWhere _ s s3 s4) = _
    pprint v (StWhereConstruct _ s s3) = _
    pprint v (StElsewhere _ s) = _
    pprint v (StEndWhere _ s) = _
    pprint v (StUse _ s s3 s4) = _
    pprint v (StModuleProcedure _ s s3) = _
    pprint v (StType _ s s3 s4) = _
    pprint v (StEndType _ s s3) = _
    pprint v (StSequence _ s) = _
    pprint v (StFormatBogus _ s s3) = _
-}

instance Pretty (Expression a) => Pretty (Use a) where
    pprint v use
      | v >= Fortran90 =
        case use of
          UseRename _ _ uSrc uDst -> pprint v uSrc <+> "=>" <+> pprint v uDst
          UseID _ _ u -> pprint v u
      | v < Fortran90 =
        unsupported v "Module system is introduced in Fortran 90."

instance Pretty (Argument a) where
    pprint v (Argument _ s key e) = floatDoc s $
       case key of
         Just keyName -> text keyName <+> char '=' <+> pprint v e
         Nothing      -> pprint v e

instance Pretty (DimensionDeclarator a) => Pretty (Attribute a) where
    pprint v attr
      | v >= Fortran90 =
        case attr of
          AttrParameter _ _ -> "parameter"
          AttrPublic _ _ -> "public"
          AttrPrivate _ _ -> "private"
          AttrAllocatable _ _ -> "allocatable"
          AttrDimension _ _ dims ->
            "dimesion" <> parens (pprint v dims)
          AttrExternal _ _ -> "external"
          AttrIntent _ _ intent ->
            "intent" <> parens (pprint v intent)
          AttrIntrinsic _ _ -> "intrinsic"
          AttrOptional _ _ -> "optional"
          AttrPointer _ _ -> "pointer"
          AttrSave _ _ -> "save"
          AttrTarget _ _ -> "target"
      | otherwise = unsupported v "Attributes are introduced in Fortran 90."

instance Pretty Intent where
    pprint v intent
      | v >= Fortran90 =
        case intent of
          In -> "in"
          Out -> "out"
          InOut -> "inout"
      | otherwise = unsupported v "Attributes are introduced in Fortran90."

-- TODO come back to this once edit descriptors are properly handled in the
-- parser.
instance Pretty (Expression a) => Pretty (FormatItem a) where
    pprint _ (FIHollerith _ _ (ValHollerith s)) =
      text (show $ length s) <> char 'h' <> text s
    pprint _ _ = error "Not yet supported."

instance (Pretty (Expression a)) => Pretty (DoSpecification a) where
    pprint v (DoSpecification _ _ s@StExpressionAssign{} limit mStride) =
      pprint v s <> comma
      <+> pprint v limit
      <> maybe empty ((comma<+>) . pprint v) mStride

    -- Given DoSpec. has a single constructor, the only way for pattern
    -- match above to fail is to have the wrong type of statement embedded
    -- in it.
    pprint _ _ = error "Incorrect initialisation in DO specification."

instance Pretty (Expression a) => Pretty (ControlPair a) where
    pprint v (ControlPair _ _ mStr exp)
      | v >= Fortran77
      , Just str <- mStr =
        text str <+> char '=' <+> pprint v exp
      | v < Fortran77
      , Just str <- mStr =
        unsupported v "Named control pairs are introduced in Fortran 77."
      | otherwise = pprint v exp

instance Pretty (ImpElement a) => Pretty (ImpList a) where
    pprint v (ImpList _ _ bt els) = pprint v bt <+> parens (pprint v els)

instance Pretty (Expression a) => Pretty (CommonGroup a) where
    pprint v (CommonGroup _ _ mName elems) =
      char '/' <> pprint v mName <> char '/' <> pprint v elems

instance Pretty (Expression a) => Pretty (Namelist a) where
    pprint Fortran90 (Namelist _ _ name elems) =
      char '/' <> pprint Fortran90 name <> char '/' <> pprint Fortran90 elems
    pprint v _ =
      unsupported v "Namelists statements are introduced in Fortran 90."

instance Pretty (Expression a) => Pretty (DataGroup a) where
    pprint v (DataGroup _ _ vars exps) =
      pprint v vars <> char '/' <> pprint v exps <> char '/'

instance Pretty (ImpElement a) where
    pprint v (ImpCharacter _ _ c) = text c
    pprint v (ImpRange _ _ beg end) = text beg <> "-" <> text end

instance (Pretty (Argument a), Pretty (Value a)) => Pretty (Expression a) where
    pprint v (ExpValue _ s val)  =
         pprint v val

    pprint v (ExpBinary _ s op e1 e2) =
        floatDoc s $ pprint v e1 <+> pprint v op <+> pprint v e2

    pprint v (ExpUnary _ s op e) =
        floatDoc s $ pprint v op <+> pprint v e

    pprint v (ExpSubscript _ s e ixs) =
        floatDoc s $ pprint v e <> parens (pprint v ixs)

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
    pprint v ValAssignment
      | v >= Fortran90 = "assignment (=)"
      -- TODO better error message is needed. Assignment is too vague.
      | otherwise = unsupported v "Asiggnment is introduced in Fortran 90."
    pprint v (ValOperator op)
      | v >= Fortran90 = "operator" <+> parens (text op)
      -- TODO better error message is needed. Operator is too vague.
      | otherwise = unsupported v "Operator is introduced in Fortran 90."
    pprint v (ValComplex e1 e2) = parens $ commaSep [pprint v e1, pprint v e2]
    pprint v (ValString str) = quotes $ text str
    pprint v valLit = text . getFirstParameter $ valLit

instance Pretty (Expression a) => Pretty (Declarator a) where
    pprint v (DeclVariable _ _ e mLen mInit)
      | v >= Fortran90 =
        pprint v e <>
        maybe empty (("*"<>) . pprint v) mLen <>
        maybe empty ((" ="<+>) . pprint v) mInit

    pprint v (DeclVariable _ _ e mLen mInit)
      | v >= Fortran77 =
        case mInit of
          Nothing -> pprint v e <>
                     maybe empty (("*"<>) . pprint v) mLen
          _ -> unsupported v "Variable initialisations in declarations is \
                             \introduced in Fortran 90."
    pprint v (DeclVariable _ _ e mLen mInit)
      | Nothing <- mLen
      , Nothing <- mInit = pprint v e
      | Just _ <- mInit =
        unsupported v "Variable initialisations in declarations is introduced \
                      \in Fortran 90."
      | Just _ <- mLen =
        unsupported v "Variable width in declarations is introduced in Fortran \
                      \77."

    pprint v (DeclArray _ _ e dims mLen mInit)
      | v >= Fortran90 =
        pprint v e <> parens (pprint v dims) <>
        maybe empty (("*"<>) . pprint v) mLen <>
        maybe empty ((" ="<+>) . pprint v) mInit

    pprint v (DeclArray _ _ e dims mLen mInit)
      | v >= Fortran77 =
        case mInit of
          Nothing -> pprint v e <> parens (pprint v dims) <>
                     maybe empty (("*"<>) . pprint v) mLen
          _ -> unsupported v "Variable initialisations in declarations is \
                             \introduced in Fortran 90."
    pprint v (DeclArray _ _ e dims mLen mInit)
      | Nothing <- mLen
      , Nothing <- mInit = pprint v e <> parens (pprint v dims)
      | Just _ <- mInit =
        unsupported v "Variable initialisations in declarations is introduced \
                      \in Fortran 90."
      | Just _ <- mLen =
        unsupported v "Variable width in declarations is introduced in Fortran \
                      \77."

instance Pretty (DimensionDeclarator a) where
    pprint v (DimensionDeclarator _ _ me1 me2) =
      pprint v me1 <> maybe empty (const $ char ':') me1 <> pprint v me2

instance Pretty UnaryOp where
    pprint _ Plus  = char '+'
    pprint _ Minus = char '-'
    pprint _ Not   = ".not."
    pprint v (UnCustom custom)
      | v >= Fortran90 = text $ "." ++ custom ++ "."
      | otherwise =
        unsupported v "Custom unary operators are introduct in Fortran 90."

instance Pretty BinaryOp where
    pprint _ Addition       = char '+'
    pprint _ Subtraction    = char '-'
    pprint _ Multiplication = char '*'
    pprint _ Division       = char '/'
    pprint _ Exponentiation = "**"
    pprint v Concatenation
      | v >= Fortran77 = "//"
      | otherwise = unsupported v "Character type is introduced in Fortran 77."
    pprint v GT  = if v <= Fortran77Extended then ".gt." else ">"
    pprint v LT  = if v <= Fortran77Extended then ".lt." else "<"
    pprint v LTE = if v <= Fortran77Extended then ".le." else "<="
    pprint v GTE = if v <= Fortran77Extended then ".ge." else ">="
    pprint v EQ  = if v <= Fortran77Extended then ".eq." else "=="
    pprint v NE  = if v <= Fortran77Extended then ".ne." else "!="
    pprint v Or  = ".or."
    pprint v And = ".and."
    pprint v Equivalent
      | v >= Fortran77 = ".eqv."
      | otherwise = unsupported v ".EQV. operator is introduced in Fortran 77."
    pprint v NotEquivalent
      | v >= Fortran77 = ".neqv."
      | otherwise = unsupported v ".NEQV. operator is introduced in Fortran 77."
    pprint v (BinCustom custom)
      | v >= Fortran90 = "." <> text custom <> "."
      | otherwise =
        unsupported v "Custom binary operators are introduced in Fortran90."

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate (comma <> space)

floatDoc :: SrcSpan -> Doc -> Doc
floatDoc span d | lineDistance span == 0 =
   -- If the rendered pretty print is less than the width of
   -- the span, then pad to the end with spaces
   if length (render d) < columnDistance span
   then vcat (d : replicate (columnDistance span - length (render d)) space)
   else d

-- Difficult to know what to dif line distance is non-zero
floatDoc span d = d
