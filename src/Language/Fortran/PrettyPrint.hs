{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Fortran.PrettyPrint where

import Data.Maybe (isJust, isNothing)
import Data.List (foldl')

import Prelude hiding (EQ,LT,GT,pred,exp)

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.FirstParameter

import Text.PrettyPrint

tooOld :: FortranVersion -> String -> FortranVersion -> a
tooOld currentVersion featureName featureVersion = error $
    featureName ++ " was introduced in " ++ show featureVersion ++
    ". You called pretty print with " ++ show currentVersion ++ "."

(<?>) :: Doc -> Doc -> Doc
doc1 <?> doc2 = if doc1 == empty || doc2 == empty then empty else doc1 <> doc2
infixl 7 <?>

(<?+>) :: Doc -> Doc -> Doc
doc1 <?+> doc2 = if doc1 == empty || doc2 == empty then empty else doc1 <+> doc2
infixl 7 <?+>

newline :: Doc
newline = char '\n'

type Indentation = Maybe Int

incIndentation :: Indentation -> Indentation
incIndentation indentation = (+2) <$> indentation

indent :: Indentation -> Doc -> Doc
indent Nothing d = d
indent (Just i) d = text (replicate i ' ') <> d

overlay :: Doc -> Doc -> Doc
overlay top bottom = text $ top' ++ drop (length top') (render bottom)
  where top' = render top

fixedForm :: Maybe Int
fixedForm = Just 6

pprintAndRender :: IndentablePretty t => FortranVersion -> t -> Indentation -> String
pprintAndRender v t i = render $ pprint v t i

class IndentablePretty t where
    pprint :: FortranVersion -> t -> Indentation -> Doc

instance {-# OVERLAPPABLE #-} Pretty a => IndentablePretty a where
    pprint v t _ = pprint' v t

instance IndentablePretty a => IndentablePretty (Maybe a) where
    pprint v (Just t) i = pprint v t i
    pprint _ Nothing _ = empty

instance IndentablePretty (ProgramFile a) where
    pprint v (ProgramFile _ programUnits) i =
      foldl' (\b a -> b <> pprintUnit a) empty programUnits
      where
        pprintUnit pu = pprint v pu i

instance IndentablePretty [ProgramUnit a] where
    pprint v pus i = foldl' (\b a -> b <?> newline <> pprint v a i) empty pus

instance IndentablePretty (ProgramUnit a) where
    pprint v (PUMain _ _ mName body mSubs) i
      | v < Fortran77 =
        if isJust mName
          then tooOld v "Named main program unit" Fortran77
          else
            if isJust mSubs
              then tooOld v "Subprogram unit" Fortran90
              else pprint v body fixedForm <>
                   indent fixedForm ("end" <> newline)
      | v < Fortran90 =
        indent fixedForm ("program" <?+> pprint' v mName <?> newline) <>
        if isJust mSubs
          then tooOld v "Subprogram unit" Fortran90
          else pprint v body fixedForm <>
               indent fixedForm ("end" <> newline)
      | otherwise =
        indent i ("program" <?+> pprint' v mName <?> newline) <>
        pprint v body nextI <>
        newline <?>
        indent nextI ("contains" <> newline) <?>
        newline <?>
        pprint v mSubs nextI <>
        indent i ("end" <> " program" <?+> pprint' v mName <> newline)
      where
        nextI = incIndentation i

    pprint v (PUModule _ _ name body mSubs) i
      | v >= Fortran90 =
        indent i ("module" <+> text name <> newline) <>
        pprint v body nextI <>
        newline <?>
        indent nextI ("contains" <?> newline) <?>
        newline <?>
        pprint v mSubs nextI <>
        indent i ("end module" <+> text name <> newline)
      | otherwise = tooOld v "Module system" Fortran90
      where
        nextI = incIndentation i

    pprint v (PUSubroutine _ _ funcSpec name mArgs body mSubs) i
      | Pure{} <- funcSpec, v < Fortran95 = tooOld v "Pure subroutine" Fortran90
      | Elemental _ _ <- funcSpec, v < Fortran90 = tooOld v "Elemental subroutine" Fortran90
      | functionIsRecursive funcSpec, v < Fortran90 = tooOld v "Recursive subroutine" Fortran90
      | isJust mSubs, v < Fortran90 = tooOld v "Subroutine subprogram" Fortran90
      | otherwise =
        indent curI
          ((case funcSpec of
            (Elemental _ _) -> "elemental"
            Pure{} -> "pure"
            _ -> empty) <+>
          (if functionIsRecursive funcSpec then "recursive" else empty) <+>
          "subroutine" <+> text name <>
          lparen <?> pprint' v mArgs <?> rparen <> newline) <>
        pprint v body nextI <>
        newline <?>
        indent nextI ("contains" <> newline) <?>
        newline <?>
        pprint v mSubs nextI <>
        endGen v "subroutine" name curI
        where
          curI = if v >= Fortran90 then i else fixedForm
          nextI = if v >= Fortran90
                    then incIndentation i
                    else incIndentation fixedForm

    pprint v (PUFunction _ _ mRetType fSpec name mArgs mRes body mSubs) i
      | (Elemental _ _) <- fSpec, v < Fortran95 = tooOld v "Elemental function" Fortran90
      | Pure{} <- fSpec, v < Fortran95 = tooOld v "Pure function" Fortran90
      | functionIsRecursive fSpec, v < Fortran90 = tooOld v "Recursive function" Fortran90
      | isJust mRes, v < Fortran90 = tooOld v "Function result" Fortran90
      | isJust mSubs, v < Fortran90 = tooOld v "Function subprogram" Fortran90
      | otherwise =
        indent curI
          (pprint' v mRetType <+>
          (case fSpec of
            (Elemental _ _) -> "elemental"
            Pure{} -> "pure"
            _ -> empty) <+>
          (if functionIsRecursive fSpec then "recursive" else empty) <+>
          "function" <+> text name <>
          lparen <?> pprint' v mArgs <?> rparen <+>
          "result" <?> lparen <?> pprint' v mRes <?> rparen <> newline) <>
        pprint v body nextI <>
        newline <?>
        indent nextI ("contains" <> newline) <?>
        newline <?>
        pprint v mSubs nextI <>
        endGen v "function" name curI
        where
          curI = if v >= Fortran90 then i else fixedForm
          nextI = if v >= Fortran90
                    then incIndentation i
                    else incIndentation fixedForm

    pprint v (PUBlockData _ _ mName body) i
      | v < Fortran77, isJust mName = tooOld v "Named block data" Fortran77
      | otherwise =
        indent curI ("block data" <+> pprint' v mName <> newline) <>
        pprint v body nextI <>
        endGen v "block data" mName curI
        where
          curI = if v >= Fortran90 then i else fixedForm
          nextI = if v >= Fortran90
                    then incIndentation i
                    else incIndentation fixedForm

    pprint v (PUComment _ _ (Comment comment)) i
      | v >= Fortran90 = indent i (char '!' <> text comment <> newline)
      | otherwise = char 'c' <> text comment <> newline

endGen :: Pretty a => FortranVersion -> Doc -> a -> Indentation -> Doc
endGen v constructName name i = indent i $ "end" <+> middle <> newline
  where
    middle
      | v < Fortran77 = empty
      | v < Fortran90 = constructName
      | otherwise = constructName <?+> pprint' v name

instance IndentablePretty [Block a] where
    pprint v bs i = foldl' (\b a -> b <> pprint v a i) empty bs

instance IndentablePretty (Block a) where
    pprint v (BlForall _ _ mLabel mName _ body mel) i =
      labeledIndent mLabel (pprint' v mName) <> newline <>
      pprint v body nextI <>
      labeledIndent mel ("end forall" <+> pprint' v mName <> newline)
      where
        nextI = incIndentation i
        labeledIndent label stDoc =
          pprint' v label `overlay` indent i stDoc

    pprint v (BlStatement _ _ mLabel st) i =
      if v >= Fortran90
        then indent i (pprint' v mLabel <+> pprint' v st <> newline)
        else pprint' v mLabel `overlay` indent i (pprint' v st <> newline)

    pprint v (BlIf _ _ mLabel mName conds bodies el) i
      | v >= Fortran77 =
        labeledIndent mLabel
          (pprint' v mName <?> colon <+>
          "if" <+> parens (pprint' v firstCond) <+> "then" <> newline) <>
        pprint v firstBody nextI <>
        foldl' (<>) empty (map displayCondBlock restCondsBodies) <>
        labeledIndent el ("end if" <+> pprint' v mName <> newline)
      | otherwise = tooOld v "Structured if" Fortran77
      where
        ((firstCond, firstBody): restCondsBodies) = zip conds bodies
        displayCondBlock (mCond, block) =
          indent i
            (case mCond of {
              Just cond -> "else if" <+> parens (pprint' v cond) <+> "then";
              Nothing -> "else"
            } <> newline) <>
          pprint v block nextI
        nextI = incIndentation i
        labeledIndent label stDoc =
          if v >= Fortran90
            then indent i (pprint' v label <+> stDoc)
            else pprint' v mLabel `overlay` indent i stDoc

    pprint v (BlCase _ _ mLabel mName scrutinee ranges bodies el) i
      | v >= Fortran90 =
        indent i
          (pprint' v mLabel <+>
          pprint' v mName <?> colon <+>
          "select case" <+> parens (pprint' v scrutinee) <> newline) <>
        foldl' (<>) empty (zipWith (curry displayRangeBlock) ranges bodies) <>
        indent i (pprint' v el <+> "end select" <+> pprint' v mName <> newline)
      | otherwise = tooOld v "Select case" Fortran90
      where
        displayRangeBlock (mRanges, block) =
          indent nextI
            ("case" <+>
            case mRanges of {
              Just ranges' -> parens (pprint' v ranges');
              Nothing -> "default" } <> newline) <>
          pprint v block (incIndentation nextI)
        nextI = incIndentation i

    pprint v (BlInterface _ _ mLabel pus moduleProcs) i
      | v >= Fortran90 =
        indent i (pprint' v mLabel <+> "interface" <> newline) <>
        pprint v pus nextI <>
        newline <>
        pprint v moduleProcs nextI <>
        indent i ("end interface" <> newline)
      | otherwise = tooOld v "Interface" Fortran90
      where
        nextI = incIndentation i

    pprint v (BlDo _ _ mLabel mn tl doSpec body el) i
      | v >= Fortran77Extended =
        labeledIndent mLabel
          (pprint' v mn <?> colon <+>
          "do" <+> pprint' v tl <+> pprint' v doSpec <> newline) <>
        pprint v body nextI <>
        if isJust tl && isNothing mn
          then empty
          else labeledIndent el ("end do" <+> pprint' v mn <> newline)
      | otherwise =
        case tl of
          Just tLabel ->
            labeledIndent mLabel
              ("do" <+> pprint' v tLabel <+> pprint' v doSpec <> newline) <>
            pprint v body nextI
          Nothing ->
            error "Fortran 77 and earlier versions only have labeled DO blocks"
      where
        nextI = incIndentation i
        labeledIndent label stDoc =
          if v >= Fortran90
            then indent i (pprint' v label <+> stDoc)
            else pprint' v mLabel `overlay` indent i stDoc

    pprint v (BlDoWhile _ _ mLabel mName mTarget cond body el) i
       | v >= Fortran77Extended =
        labeledIndent mLabel
          (pprint' v mName <?> colon <+>
          "do" <+> pprint' v mTarget <+> "while" <+> parens (pprint' v cond) <> newline) <>
        pprint v body nextI <>
        if isJust mTarget && isNothing mName
          then empty
          else labeledIndent el ("end do" <+> pprint' v mName <> newline)
      | otherwise = tooOld v "Do while loop" Fortran77Extended
      where
        nextI = incIndentation i
        labeledIndent label stDoc =
          if v >= Fortran90
            then indent i (pprint' v label <+> stDoc)
            else pprint' v mLabel `overlay` indent i stDoc

    pprint v (BlComment _ _ (Comment comment)) i
      | v >= Fortran90 = indent i (char '!' <> text comment <> newline)
      | otherwise = char 'c' <> text comment <> newline

class Pretty t where
    pprint' :: FortranVersion -> t -> Doc

instance Pretty a => Pretty (Maybe a) where
    pprint' _ Nothing  = empty
    pprint' v (Just e) = pprint' v e

instance Pretty String where
    pprint' _ = text

instance Pretty (e a) => Pretty (AList e a) where
    pprint' v es = commaSep (map (pprint' v) (aStrip es))

instance Pretty BaseType where
    pprint' _ TypeInteger = "integer"
    pprint' _ TypeReal    = "real"
    pprint' _ TypeDoublePrecision = "double precision"
    pprint' _ TypeComplex = "complex"
    pprint' v TypeDoubleComplex
      | v == Fortran77Extended = "double complex"
      | otherwise = tooOld v "Double complex" Fortran77Extended
    pprint' _ TypeLogical = "logical"
    pprint' v TypeCharacter
      | v >= Fortran77 = "character"
      | otherwise = tooOld v "Character data type" Fortran77
    pprint' v (TypeCustom str)
      | v >= Fortran90 = "type" <+> parens (text str)
      | v >= Fortran77Extended = "record" <+> char '/' <> text str <> char '/'
      | otherwise = tooOld v "User defined type" Fortran90
    pprint' v TypeByte
      | v >= Fortran77Extended = "byte"
      | otherwise = tooOld v "Byte" Fortran77Extended

instance Pretty (TypeSpec a) where
    pprint' v (TypeSpec _ _ baseType mSelector) =
      pprint' v baseType <+> pprint' v mSelector

instance Pretty (Selector a) where
  pprint' v (Selector _ _ mLenSel mKindSel)
    | v < Fortran77 = tooOld v "Length/kind selector" Fortran77
    | v < Fortran90 =
      case (mLenSel, mKindSel) of
        (Just lenSel, Nothing) ->
          char '*' <+> parens (pprint' Fortran77Extended lenSel)
        (Nothing, Just kindSel) ->
          char '*' <+> parens (pprint' Fortran77Extended kindSel)
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
    | otherwise = error "unhandled version"
    where
      len e  = "len=" <> pprint' Fortran90 e
      kind e = "kind=" <> pprint' Fortran90 e

instance Pretty (Statement a) where
    pprint' v (StDeclaration _ _ typeSpec mAttrList declList)
      | v < Fortran90 = pprint' v typeSpec <+> pprint' v declList
      | v >= Fortran90 =
          pprint' v typeSpec <>
          (if isJust mAttrList then comma else empty) <+>
          pprint' v mAttrList <+>
          text "::" <+>
          pprint' v declList
      | otherwise = error "unhandled version"

    pprint' v (StStructure _ _ mName itemList)
      | v /= Fortran77Extended = tooOld v "Structure" Fortran77Extended
      | otherwise =
          "structure" <> (if isJust mName then " /" <> pprint' v mName <> "/" else empty) <> newline <>
          foldl' (\doc item -> doc <> pprint v item (incIndentation (Just 0)) <> newline) empty (aStrip itemList) <>
          "end structure"

    pprint' v (StIntent _ _ intent exps)
      | v >= Fortran90 =
          "intent" <+> parens (pprint' v intent) <+> "::" <+> pprint' v exps
      | otherwise = tooOld v "Intent statement" Fortran90

    pprint' v (StOptional _ _ vars)
      | v >= Fortran90 = "optional ::" <+> pprint' v vars
      | otherwise = tooOld v "Optional statement" Fortran90

    pprint' v (StPublic _ _ mVars)
      | v >= Fortran90 = "public" <> " :: " <?> pprint' v mVars
      | otherwise = tooOld v "Public statement" Fortran90

    pprint' v (StPrivate _ _ mVars)
      | v >= Fortran90 = "private" <> " :: " <?> pprint' v mVars
      | otherwise = tooOld v "Private statement" Fortran90

    pprint' v (StSave _ _ mVars)
      | v >= Fortran90 = "save" <> " :: " <?> pprint' v mVars
      | otherwise = "save" <+> pprint' v mVars

    pprint' v (StDimension _ _ decls)
      | v >= Fortran90 = "dimension ::" <+> pprint' v decls
      | otherwise = "dimension" <+> pprint' v decls

    pprint' v (StAllocatable _ _ decls)
      | v >= Fortran90 = "allocatable ::" <+> pprint' v decls
      | otherwise = tooOld v "Allocatable statement" Fortran90

    pprint' v (StPointer _ _ decls)
      | v >= Fortran90 = "pointer ::" <+> pprint' v decls
      | otherwise = tooOld v "Pointer statement" Fortran90

    pprint' v (StTarget _ _ decls)
      | v >= Fortran90 = "target ::" <+> pprint' v decls
      | otherwise = tooOld v "Target statement" Fortran90

    pprint' v (StData _ _ aDataGroups@(AList _ _ dataGroups))
      | v >= Fortran90 = "data" <+> pprint' v aDataGroups
      | otherwise = "data" <+> hsep (map (pprint' v) dataGroups)

    pprint' v (StAutomatic _ _ decls)
      | v == Fortran77Extended = "automatic" <+> pprint' v decls
      | otherwise = tooOld v "Target statement" Fortran90

    pprint' v (StNamelist _ _ namelist)
      | v >= Fortran90 = "namelist" <+> pprint' v namelist
      | otherwise = tooOld v "Namelist statement" Fortran90

    pprint' v (StParameter _ _ aDecls) = "parameter" <+> parens (pprint' v aDecls)

    pprint' v (StExternal _ _ vars) = "external" <+> pprint' v vars
    pprint' v (StIntrinsic _ _ vars) = "intrinsic" <+> pprint' v vars

    pprint' v (StCommon _ _ aCommonGroups) = "common" <+> pprint' v aCommonGroups

    pprint' v (StEquivalence _ _ (AList _ _ equivGroups)) =
      "equivalence" <+> commaSep (map (parens . pprint' v) equivGroups)

    pprint' v (StFormat _ _ (AList _ _ formatItems)) =
      "format" <+> hcat (map (pprint' v) formatItems)

    pprint' v (StImplicit _ _ mImpLists)
      | Just impLists <- mImpLists = "implicit" <+> pprint' v impLists
      | otherwise = "implicit none"

    pprint' v (StEntry _ _ name mArgs mResult)
      | v < Fortran90 =
        case mResult of
          Nothing ->
            "entry" <+> pprint' v name <+> parens (pprint' v mArgs)
          Just _ -> tooOld v "Explicit result" Fortran90
      | otherwise =
        "entry" <+>
        pprint' v name <+> parens (pprint' v mArgs) <+>
        "result (" <?> pprint' v mResult <?> char ')'

    pprint' v (StInclude _ _ file _) = "include" <+> pprint' v file

    pprint' v (StDo _ _ mConstructor mLabel mDoSpec)
      | v < Fortran90
      , Just _ <- mConstructor = tooOld v "Named DO block" Fortran90
      | v < Fortran77Extended
      , Nothing <- mLabel = tooOld v "Labelless DO block" Fortran90
      | v < Fortran90
      , Nothing <- mDoSpec = tooOld v "Infinite DO loop" Fortran90
      | otherwise =
        pprint' v mConstructor <?> colon <+>
        "do" <+> pprint' v mLabel <+> pprint' v mDoSpec

    pprint' v (StDoWhile _ _ mConstructor mLabel pred)
      | v < Fortran77Extended = tooOld v "While loop" Fortran77Extended
      | otherwise =
        pprint' v mConstructor <?> colon <+>
        "do" <+> pprint' v mLabel <+>
        "while" <+> parens (pprint' v pred)

    pprint' v (StEnddo _ _ mConstructor)
      | v < Fortran77Extended = tooOld v "End do" Fortran77Extended
      | v < Fortran90
      , _ <- mConstructor = tooOld v "Named DO loop" Fortran90
      | otherwise = "end do" <+> pprint' v mConstructor

    pprint' v (StExpressionAssign _ _ lhs rhs) =
      pprint' v lhs <+> equals <+> pprint' v rhs

    pprint' v (StCycle _ _ mConstructor)
      | v >= Fortran90 = "cycle" <+> pprint' v mConstructor
      | otherwise = tooOld v "Cycle" Fortran90

    pprint' v (StExit _ _ mConstructor)
      | v >= Fortran77Extended = "exit" <+> pprint' v mConstructor
      | otherwise = tooOld v "Exit" Fortran77Extended

    pprint' v (StIfLogical _ _ pred st) =
      "if" <+> parens (pprint' v pred) <+> pprint' v st

    pprint' v (StIfArithmetic _ _ exp ltPred eqPred gtPred) =
      "if" <+> parens (pprint' v exp) <+>
      pprint' v ltPred <> comma <+>
      pprint' v eqPred <> comma <+>
      pprint' v gtPred

    pprint' v (StIfThen _ _ mConstructor condition)
      | v >= Fortran90 =
        pprint' v mConstructor <?> colon <+>
        "if" <+> parens (pprint' v condition) <+> "then"
      | v >= Fortran77Extended =
        case mConstructor of
          Nothing -> "if" <+> parens (pprint' v condition) <+> "then"
          _ -> tooOld v "Else" Fortran77Extended
      | otherwise = tooOld v "Structured if" Fortran90

    pprint' v (StElse _ _ mConstructor)
      | v >= Fortran90 = "else" <+> pprint' v mConstructor
      | v >= Fortran77Extended =
        case mConstructor of
          Nothing -> "else"
          Just _ -> tooOld v "Named else" Fortran90
      | otherwise = tooOld v "Else" Fortran77Extended

    pprint' v (StElsif _ _ mConstructor condition)
      | v >= Fortran90 =
        "else if" <+> parens (pprint' v condition) <+> pprint' v mConstructor
      | v >= Fortran77Extended =
        case mConstructor of
          Nothing -> "else if" <+> parens (pprint' v condition)
          _ -> tooOld v "Named else if" Fortran90
      | otherwise = tooOld v "Else if" Fortran77Extended

    pprint' v (StEndif _ _ mConstructor)
      | v >= Fortran90 = "end if" <+> pprint' v mConstructor
      | v >= Fortran77Extended =
        case mConstructor of
          Nothing -> "end if"
          Just _ -> tooOld v "Named end if" Fortran90
      | otherwise = tooOld v "End if" Fortran77Extended

    pprint' v (StSelectCase _ _ mConstructor exp)
      | v >= Fortran90 =
        pprint' v mConstructor <?> colon <+>
        "select case" <+> parens (pprint' v exp)
      | otherwise = tooOld v "Case statement" Fortran90

    pprint' v (StCase _ _ mConstructor mCase)
      | v >= Fortran90 =
        case mCase of
          Just casee ->
            "case" <+> parens (pprint' v casee) <+> pprint' v mConstructor
          Nothing -> "case default" <+> pprint' v mConstructor
      | otherwise = tooOld v "Case statement" Fortran90

    pprint' v (StEndcase _ _ mConstructor)
      | v >= Fortran90 = "end case" <+> pprint' v mConstructor
      | otherwise = tooOld v "Case statement" Fortran90

    pprint' v (StFunction _ _ name args rhs) =
      pprint' v name <> parens (pprint' v args) <+> equals <+> pprint' v rhs

    pprint' v (StPointerAssign _ _ lhs rhs)
      | v >= Fortran90 = pprint' v lhs <+> "=>" <+> pprint' v rhs
      | otherwise = tooOld v "Pointer assignment" Fortran90

    pprint' v (StLabelAssign _ _ label binding) =
      "assign" <+> pprint' v label <+> "to" <+> pprint' v binding

    pprint' v (StGotoUnconditional _ _ label) = "goto" <+> pprint' v label
    pprint' v (StGotoAssigned _ _ target labels) =
      "goto" <+> pprint' v target <+> parens (pprint' v labels)
    pprint' v (StGotoComputed _ _ labels target) =
      "goto" <+> parens (pprint' v labels) <+> pprint' v target

    pprint' v (StCall _ _ name args) = pprint' v name <+> parens (pprint' v args)

    pprint' _ (StContinue _ _) = "continue"

    pprint' v (StReturn _ _ exp) = "return" <+> pprint' v exp

    pprint' v (StStop _ _ code) = "stop" <+> pprint' v code

    pprint' v (StPause _ _ code) = "pause" <+> pprint' v code

    pprint' v (StRead _ _ cilist mIolist) =
      "read" <+> parens (pprint' v cilist) <+> pprint' v mIolist
    pprint' v (StRead2 _ _ formatId mIolist) =
      "read" <+> pprint' v formatId <> comma <?+> pprint' v mIolist

    pprint' v (StWrite _ _ cilist mIolist) =
      "write" <+> parens (pprint' v cilist) <+> pprint' v mIolist
    pprint' v (StPrint _ _ formatId mIolist) =
      "print" <+> pprint' v formatId <> comma <?+> pprint' v mIolist
    pprint' v (StTypePrint _ _ formatId mIolist)
      | v == Fortran77Extended
      = "type" <+> pprint' v formatId <> comma <?+> pprint' v mIolist
      | otherwise = tooOld v "Type (print) statement" Fortran77Extended

    pprint' v (StOpen _ _ cilist) = "open" <+> parens (pprint' v cilist)
    pprint' v (StClose _ _ cilist) = "close" <+> parens (pprint' v cilist)
    pprint' v (StInquire _ _ cilist) = "inquire" <+> parens (pprint' v cilist)

    pprint' v (StRewind _ _ cilist) = "rewind" <+> parens (pprint' v cilist)
    pprint' v (StRewind2 _ _ unit) = "rewind" <+> pprint' v unit

    pprint' v (StBackspace _ _ cilist) =
      "backspace" <+> parens (pprint' v cilist)
    pprint' v (StBackspace2 _ _ unit) = "backspace" <+> pprint' v unit

    pprint' v (StEndfile _ _ cilist) = "endfile" <+> parens (pprint' v cilist)
    pprint' v (StEndfile2 _ _ unit) = "endfile" <+> pprint' v unit

    pprint' v (StAllocate _ _ vars contPair)
      | v >= Fortran90 =
        "allocate" <+> parens (pprint' v vars <> comma <?+> pprint' v contPair)
      | otherwise = tooOld v "Allocate" Fortran90

    pprint' v (StDeallocate _ _ vars contPair)
      | v >= Fortran90 =
        "deallocate" <+> parens (pprint' v vars <> comma <?+> pprint' v contPair)
      | otherwise = tooOld v "Deallocate" Fortran90

    pprint' v (StNullify _ _ vars) = "nullify" <+> pprint' v vars

    pprint' v (StWhere _ _ mask assignment)
      | v >= Fortran90 =
        "where" <+> parens (pprint' v mask) <+> pprint' v assignment
      | otherwise = tooOld v "Where statement" Fortran90

    pprint' v (StWhereConstruct _ _ mask)
      | v >= Fortran90 = "where" <+> parens (pprint' v mask)
      | otherwise = tooOld v "Where construct" Fortran90

    pprint' v (StElsewhere _ _)
      | v >= Fortran90 = "else where"
      | otherwise = tooOld v "Else where" Fortran90

    pprint' v (StEndWhere _ _)
      | v >= Fortran90 = "end where"
      | otherwise = tooOld v "End where" Fortran90

    pprint' v (StUse _ _ moduleName only mappings)
      | v >= Fortran90 =
        "use" <+> pprint' v moduleName <>
        (comma <?+> (pprint' v only <+> pprint' v mappings))
      | otherwise = tooOld v "Module system" Fortran90

    pprint' v (StModuleProcedure _ _ procedures)
      | v >= Fortran90 =
        "module procedure" <+> pprint' v procedures
      | otherwise = tooOld v "Module procedure" Fortran90

    pprint' v (StType _ _ attrs name)
      | v >= Fortran90 = "type" <+> pprint' v attrs <+> pprint' v name
      | otherwise  = tooOld v "Derived type" Fortran90

    pprint' v (StEndType _ _ name)
      | v >= Fortran90 = "end type" <+> pprint' v name
      | otherwise  = tooOld v "Derived type" Fortran90

    pprint' v (StSequence _ _)
      | v >= Fortran90 = "sequence"
      | otherwise = tooOld v "Sequence" Fortran90

    pprint' v (StFormatBogus _ _ blob) = "format" <+> pprint' v blob
    pprint' _ StForall{} = error "unhandled pprint StForall"
    pprint' _ StForallStatement{} = error "unhandled pprint StForallStatement"
    pprint' _ StEndForall{} = error "unhandled pprint StEndForall"

instance Pretty Only where
    pprint' _ Exclusive = "only" <> colon
    pprint' _ Permissive = empty

instance Pretty (Use a) where
    pprint' v use
      | v >= Fortran90 =
        case use of
          UseRename _ _ uSrc uDst -> pprint' v uSrc <+> "=>" <+> pprint' v uDst
          UseID _ _ u -> pprint' v u
      | v < Fortran90 = tooOld v "Module system" Fortran90
      | otherwise = error "unhandled version"

instance Pretty (Argument a) where
    pprint' v (Argument _ _ key e) =
       case key of
         Just keyName -> text keyName <+> char '=' <+> pprint' v e
         Nothing      -> pprint' v e

instance Pretty (Attribute a) where
    pprint' v attr
      | v >= Fortran90 =
        case attr of
          AttrParameter _ _ -> "parameter"
          AttrPublic _ _ -> "public"
          AttrPrivate _ _ -> "private"
          AttrAllocatable _ _ -> "allocatable"
          AttrDimension _ _ dims ->
            "dimesion" <> parens (pprint' v dims)
          AttrExternal _ _ -> "external"
          AttrIntent _ _ intent ->
            "intent" <> parens (pprint' v intent)
          AttrIntrinsic _ _ -> "intrinsic"
          AttrOptional _ _ -> "optional"
          AttrPointer _ _ -> "pointer"
          AttrSave _ _ -> "save"
          AttrTarget _ _ -> "target"
      | otherwise = tooOld v "Declaration attribute" Fortran90

instance Pretty Intent where
    pprint' v intent
      | v >= Fortran90 =
        case intent of
          In -> "in"
          Out -> "out"
          InOut -> "inout"
      | otherwise = tooOld v "Declaration attribute" Fortran90

-- TODO come back to this once edit descriptors are properly handled in the
-- parser.
instance Pretty (FormatItem a) where
    pprint' _ (FIHollerith _ _ (ValHollerith s)) =
      text (show $ length s) <> char 'h' <> text s
    pprint' _ _ = error "Not yet supported."

instance Pretty (DoSpecification a) where
    pprint' v (DoSpecification _ _ s@StExpressionAssign{} limit mStride) =
      pprint' v s <> comma
      <+> pprint' v limit
      <> comma <?+> pprint' v mStride

    -- Given DoSpec. has a single constructor, the only way for pattern
    -- match above to fail is to have the wrong type of statement embedded
    -- in it.
    pprint' _ _ = error "Incorrect initialisation in DO specification."

instance Pretty (ControlPair a) where
    pprint' v (ControlPair _ _ mStr exp)
      | v >= Fortran77
      , Just str <- mStr = text str <> char '=' <> pprint' v exp
      | v < Fortran77
      , Just _ <- mStr = tooOld v "Named control pair" Fortran77
      | otherwise = pprint' v exp

instance Pretty (ImpList a) where
    pprint' v (ImpList _ _ bt els) = pprint' v bt <+> parens (pprint' v els)

instance Pretty (CommonGroup a) where
    pprint' v (CommonGroup _ _ mName elems) =
      char '/' <> pprint' v mName <> char '/' <> pprint' v elems

instance Pretty (Namelist a) where
    pprint' Fortran90 (Namelist _ _ name elems) =
      char '/' <> pprint' Fortran90 name <> char '/' <> pprint' Fortran90 elems
    pprint' v _ = tooOld v "Namelist statement" Fortran90

instance Pretty (DataGroup a) where
    pprint' v (DataGroup _ _ vars exps) =
      pprint' v vars <> char '/' <> pprint' v exps <> char '/'

instance Pretty (ImpElement a) where
    pprint' _ (ImpCharacter _ _ c) = text c
    pprint' _ (ImpRange _ _ beg end) = text beg <> "-" <> text end

instance Pretty (Expression a) where
    pprint' v (ExpValue _ _ val)  =
         pprint' v val

    pprint' v (ExpBinary _ _ op e1 e2) =
        parens (pprint' v e1 <+> pprint' v op <+> pprint' v e2)

    pprint' v (ExpUnary _ _ op e) =
        pprint' v op <+> pprint' v e

    pprint' v (ExpSubscript _ _ e ixs) =
        pprint' v e <> parens (pprint' v ixs)

    pprint' v (ExpDataRef _ _ e1 e2) =
        pprint' v e1 <+> char '%' <+> pprint' v e2

    pprint' v (ExpFunctionCall _ _ e mes) =
        pprint' v e <> parens (pprint' v mes)

    pprint' v (ExpImpliedDo _ _ es dospec) =
        pprint' v es <> comma <+> pprint' v dospec

    pprint' v (ExpInitialisation _ _ es) =
        "(/" <> pprint' v es <> "/)"

    pprint' v (ExpReturnSpec _ _ e) =
        char '*' <> pprint' v e

instance Pretty (Index a) where
    pprint' v (IxSingle _ _ Nothing e) = pprint' v e
    -- This is an intermediate expression form which shouldn't make it
    -- to the pretty printer
    pprint' v (IxSingle _ _ (Just _) e) = pprint' v e
    pprint' v (IxRange _ _ low up stride) =
       pprint' v low <> colon <> pprint' v up <> colon <?> pprint' v stride

-- A subset of Value permit the 'FirstParameter' operation
instance FirstParameter (Value a) String
instance Pretty (Value a) where
    pprint' _ ValStar       = char '*'
    pprint' v ValAssignment
      | v >= Fortran90 = "assignment (=)"
      -- TODO better error message is needed. Assignment is too vague.
      | otherwise = tooOld v "Assignment" Fortran90
    pprint' v (ValOperator op)
      | v >= Fortran90 = "operator" <+> parens (text op)
      -- TODO better error message is needed. Operator is too vague.
      | otherwise = tooOld v "Operator" Fortran90
    pprint' v (ValComplex e1 e2) = parens $ commaSep [pprint' v e1, pprint' v e2]
    pprint' _ (ValString str) = quotes $ text str
    pprint' _ valLit = text . getFirstParameter $ valLit

instance IndentablePretty (StructureItem a) where
  pprint v (StructFields a s spec mAttrs decls) _ = pprint' v (StDeclaration a s spec mAttrs decls)
  pprint v (StructUnion _ _ maps) i =
    "union" <> newline <>
    foldl' (\doc item -> doc <> pprint v item (incIndentation i) <> newline) empty (aStrip maps) <>
    "end union"
  pprint v (StructStructure a s mName items) _ = pprint' v (StStructure a s mName items)

instance IndentablePretty (UnionMap a) where
  pprint v (UnionMap _ _ items) i =
    "map" <> newline <>
    foldl' (\doc item -> doc <> pprint v item (incIndentation i) <> newline) empty (aStrip items) <>
    "end map"

instance Pretty (Declarator a) where
    pprint' v (DeclVariable _ _ e mLen mInit)
      | v >= Fortran90 =
        pprint' v e <>
        char '*' <?> pprint' v mLen <+>
        char '=' <?+> pprint' v mInit

    pprint' v (DeclVariable _ _ e mLen mInit)
      | v >= Fortran77 =
        case mInit of
          Nothing -> pprint' v e <>
                     char '*' <?> pprint' v mLen
          _ -> tooOld v "Variable initialisation" Fortran90

    pprint' v (DeclVariable _ _ e mLen mInit)
      | Nothing <- mLen
      , Nothing <- mInit = pprint' v e
      | Just _ <- mInit = tooOld v "Variable initialisation" Fortran90
      | Just _ <- mLen = tooOld v "Variable width" Fortran77

    pprint' v (DeclArray _ _ e dims mLen mInit)
      | v >= Fortran90 =
        pprint' v e <> parens (pprint' v dims) <+>
        "*" <?> pprint' v mLen <+>
        equals <?> pprint' v mInit

    pprint' v (DeclArray _ _ e dims mLen mInit)
      | v >= Fortran77 =
        case mInit of
          Nothing -> pprint' v e <> parens (pprint' v dims) <>
                     "*" <?> pprint' v mLen
          _ -> tooOld v "Variable initialisation" Fortran90
    pprint' v (DeclArray _ _ e dims mLen mInit)
      | Nothing <- mLen
      , Nothing <- mInit = pprint' v e <> parens (pprint' v dims)
      | Just _ <- mInit = tooOld v "Variable initialisation" Fortran90
      | Just _ <- mLen = tooOld v "Variable width" Fortran77

instance Pretty (DimensionDeclarator a) where
    pprint' v (DimensionDeclarator _ _ me1 me2) =
      pprint' v me1 <?> colon <> pprint' v me2

instance Pretty UnaryOp where
    pprint' _ Plus  = char '+'
    pprint' _ Minus = char '-'
    pprint' _ Not   = ".not."
    pprint' v (UnCustom custom)
      | v >= Fortran90 = text $ "." ++ custom ++ "."
      | otherwise = tooOld v "Custom unary operator" Fortran90

instance Pretty BinaryOp where
    pprint' _ Addition       = char '+'
    pprint' _ Subtraction    = char '-'
    pprint' _ Multiplication = char '*'
    pprint' _ Division       = char '/'
    pprint' _ Exponentiation = "**"
    pprint' v Concatenation
      | v >= Fortran77 = "//"
      | otherwise = tooOld v "Character type" Fortran77
    pprint' v GT  = if v <= Fortran77Extended then ".gt." else ">"
    pprint' v LT  = if v <= Fortran77Extended then ".lt." else "<"
    pprint' v LTE = if v <= Fortran77Extended then ".le." else "<="
    pprint' v GTE = if v <= Fortran77Extended then ".ge." else ">="
    pprint' v EQ  = if v <= Fortran77Extended then ".eq." else "=="
    pprint' v NE  = if v <= Fortran77Extended then ".ne." else "/="
    pprint' _ Or  = ".or."
    pprint' _ XOr = ".xor."
    pprint' _ And = ".and."
    pprint' v Equivalent
      | v >= Fortran77 = ".eqv."
      | otherwise = tooOld v ".EQV. operator" Fortran77
    pprint' v NotEquivalent
      | v >= Fortran77 = ".neqv."
      | otherwise = tooOld v ".NEQV. operator" Fortran77
    pprint' v (BinCustom custom)
      | v >= Fortran90 = "." <> text custom <> "."
      | otherwise = tooOld v "Custom binary operator" Fortran90

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate ", "
