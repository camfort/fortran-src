-- -*- Mode: Haskell -*-
{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran.Lexer.FreeForm where

import Prelude hiding (span)
import Data.Data
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Char (toLower)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B

import Control.Monad (join)
import Control.Monad.State (get)

import GHC.Generics

import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter

}

$digit = 0-9
$octalDigit = 0-7
$hexDigit = [a-f $digit]
$bit = 0-1

$letter = a-z
$alphanumeric = [$letter $digit \_]

@label = $digit{1,5}
@name = $letter $alphanumeric*

@binary = b\'$bit+\'
@octal = o\'$octalDigit+\'
@hex = z\'$hexDigit+\'

@digitString = $digit+
@kindParam = (@digitString|@name)
@intLiteralConst = @digitString (\_ @kindParam)?
@bozLiteralConst = (@binary|@octal|@hex)

$expLetter = [ed]
@exponent = [\-\+]? @digitString
@significand = @digitString? \. @digitString
@realLiteral = @significand ($expLetter @exponent)? (\_ @kindParam)?
             | @digitString $expLetter @exponent (\_ @kindParam)?
             -- The following two complements @altRealLiteral the reason they
             -- are included in the general case is to reduce the number of
             -- semantic predicates to be made while lexing.
             | @digitString \. $expLetter @exponent (\_ @kindParam)?
             | @digitString \. \_ @kindParam
@altRealLiteral = @digitString \.

@characterLiteralBeg = (@kindParam \_)? (\'|\")

@bool = ".true." | ".false."
@logicalLiteral = @bool (\_ @kindParam)?

--------------------------------------------------------------------------------
-- Start codes | Explanation
--------------------------------------------------------------------------------
-- 0           | For statement starters
-- scI         | For statements that can come after logical IF
-- scC         | To be used in lexCharacter, it only appears to force Happy to
--             | resolve it.
-- scT         | For types
-- scN         | For everything else
--------------------------------------------------------------------------------
tokens :-

<0,scN> "!".*$                                    { adjustComment $ addSpanAndMatch TComment }

<0,scN,scT> (\n\r|\r\n|\n)                        { resetPar >> toSC 0 >> addSpan TNewline }
<0,scN,scI,scT> [\t\ ]+                           ;

<scN> "("                                         { leftPar }
<scN> ")" / { ifConditionEndP }                   { decPar >> toSC scI >> addSpan TRightPar }
<scN> ")"                                         { decPar >> addSpan TRightPar }
<scN> "(/" / { notDefinedOperP }                  { addSpan TLeftInitPar }
<scN> "/)" / { notDefinedOperP }                  { addSpan TRightInitPar }
<scN> "[" / { notDefinedOperP }                   { addSpan TLeftInitPar }
<scN> "]" / { notDefinedOperP }                   { addSpan TRightInitPar }
<scN> ","                                         { comma }
<scN> ";"                                         { resetPar >> toSC 0 >> addSpan TSemiColon }
<scN> ":"                                         { addSpan TColon }
<scN> "::"                                        { addSpan TDoubleColon }
<scN> "="                                         { addSpan TOpAssign}
<scN> "=>"                                        { addSpan TArrow }
<scN> "%"                                         { addSpan TPercent }

<0,scI> @name / { partOfExpOrPointerAssignmentP } { addSpanAndMatch TId }
<0> @name / { constructNameP }                    { addSpanAndMatch TId }

-- Program units
<0> "program"                                     { addSpan TProgram }
<0> "end"\ *"program"                             { addSpan TEndProgram }
<0> "function"                                    { addSpan TFunction }
<scN> "function" / { typeSpecP }                  { addSpan TFunction }
<0> "end"\ *"function"                            { addSpan TEndFunction }
<scN> "result" / { resultP }                      { addSpan TResult }
<0> "pure"                                        { toSC 0 >> addSpan TPure }
<0> "elemental"                                   { toSC 0 >> addSpan TElemental }
<0> "recursive"                                   { toSC 0 >> addSpan TRecursive }
<scN> "pure" / { typeSpecP }                      { toSC 0 >> addSpan TPure }
<scN> "elemental" / { typeSpecP }                 { toSC 0 >> addSpan TElemental }
<scN> "recursive" / { typeSpecP }                 { toSC 0 >> addSpan TRecursive }
<0> "subroutine"                                  { addSpan TSubroutine }
<0> "end"\ *"subroutine"                          { addSpan TEndSubroutine }
<0> "block"\ *"data"                              { addSpan TBlockData }
<0> "end"\ *"block"\ *"data"                      { addSpan TEndBlockData }
<0> "module"                                      { addSpan TModule }
<0> "end"\ *"module"                              { addSpan TEndModule }
<0> "contains"                                    { addSpan TContains }
<0> "use"                                         { addSpan TUse }
<scN> "only" / { useStP }                         { addSpan TOnly }
<0> "import"                                      { addSpan TImport }
<0> "abstract"                                    { addSpan TAbstract }
<0> "interface"                                   { addSpan TInterface }
<scN> "interface" / { genericSpecP }              { addSpan TInterface }
<0> "end"\ *"interface"                           { addSpan TEndInterface }
<0> "procedure"                                   { addSpan TProcedure }
<0> "module"\ \ *"procedure"                      { addSpan TModuleProcedure }
<scN> "assignment"\ *"("\ *"="\ *")" / { genericSpecP } { addSpan TAssignment }
<scN> "operator" / { genericSpecP }               { addSpan TOperator }
<0,scI> "call"                                    { addSpan TCall }
<0,scI> "return"                                  { addSpan TReturn }
<0> "entry"                                       { addSpan TEntry }
<0> "include"                                     { addSpan TInclude }

-- Type def related
<0,scT> "type"                                    { addSpan TType }
<scN> "type" / { allocateP }                      { addSpan TType }
<0> "end"\ *"type"                                { addSpan TEndType }
<scN> "class" / { followsProcedureP }             { addSpan TClass }
<0> "sequence"                                    { addSpan TSequence }
<0> "enum"                                        { addSpan TEnum }
<0> "end"\ *"enum"                                { addSpan TEndEnum }
<0> "enumerator"                                  { addSpan TEnumerator }

-- Intrinsic types
<0,scT> "integer"                                 { addSpan TInteger }
<scN> "integer" / { allocateP }                   { addSpan TInteger }
<0,scT> "real"                                    { addSpan TReal }
<scN> "real" / { allocateP }                      { addSpan TReal }
<0,scT> "double"\ *"precision"                    { addSpan TDoublePrecision }
<scN> "double"\ *"precision" / { allocateP }      { addSpan TDoublePrecision }
<0,scT> "logical"                                 { addSpan TLogical }
<scN> "logical" / { allocateP }                   { addSpan TLogical }
<0,scT> "character"                               { addSpan TCharacter }
<scN> "character" / { allocateP }                 { addSpan TCharacter }
<0,scT> "complex"                                 { addSpan TComplex }
<scN> "complex" / { allocateP }                   { addSpan TComplex }

<scN> "kind" / { selectorP }                      { addSpan TKind }
<scN> "len" / { selectorP }                       { addSpan TLen }

-- Attributes
<0> "public"                                      { addSpan TPublic }
<scN> "public" / { attributeP }                   { addSpan TPublic }
<0> "private"                                     { addSpan TPrivate }
<scN> "private" / { attributeP }                  { addSpan TPrivate }
<0> "protected"                                   { addSpan TProtected }
<scN> "protected" / { attributeP }                { addSpan TProtected }
<0> "parameter"                                   { addSpan TParameter }
<scN> "parameter" / { attributeP }                { addSpan TParameter }
<0> "allocatable"                                 { addSpan TAllocatable }
<scN> "allocatable" / { attributeP }              { addSpan TAllocatable }
<0> "asynchronous"                                { addSpan TAsynchronous }
<scN> "asynchronous" / { attributeP }             { addSpan TAsynchronous }
<0> "dimension"                                   { addSpan TDimension }
<scN> "dimension" / { attributeP }                { addSpan TDimension }
<0> "external"                                    { addSpan TExternal }
<scN> "external" / { attributeP }                 { addSpan TExternal }
<0> "intent"                                      { addSpan TIntent }
<scN> "intent" / { attributeP }                   { addSpan TIntent }
<0> "intrinsic"                                   { addSpan TIntrinsic }
<scN> "intrinsic" / { attributeP }                { addSpan TIntrinsic }
<0> "non_intrinsic"                               { addSpan TNonIntrinsic }
<scN> "non_intrinsic" / { attributeP }            { addSpan TNonIntrinsic }
<0> "optional"                                    { addSpan TOptional }
<scN> "optional" / { attributeP }                 { addSpan TOptional }
<0> "pointer"                                     { addSpan TPointer }
<scN> "pointer" / { attributeP }                  { addSpan TPointer }
<0> "save"                                        { addSpan TSave }
<scN> "save" / { attributeP }                     { addSpan TSave }
<0> "target"                                      { addSpan TTarget }
<scN> "target" / { attributeP }                   { addSpan TTarget }
<0> "save"                                        { addSpan TSave }
<scN> "save" / { attributeP }                     { addSpan TSave }
<0> "value"                                       { addSpan TValue }
<scN> "value" / { attributeP }                    { addSpan TValue }
<0> "volatile"                                    { addSpan TVolatile }
<scN> "volatile" / { attributeP }                 { addSpan TVolatile }

-- Attribute values
<scN> "in"\ *"out" / { followsIntentP }           { addSpan TInOut }
<scN> "in" / { followsIntentP }                   { addSpan TIn }
<scN> "out" / { followsIntentP }                  { addSpan TOut }

-- language-binding-spec
<scN> "bind" / { bindP }                          { addSpan TBind }
<scN> "name" / { followsCP }                      { addSpan TName }
<scN> "c" / { followsBindP }                      { addSpan TC }

-- Control flow
<0> "do"                                          { addSpan TDo }
<scN> "do" / { followsColonP }                    { addSpan TDo }
<0> "end"\ *"do"                                  { addSpan TEndDo }
<scN> "while" / { followsDoP }                    { addSpan TWhile }
<0> "if"                                          { addSpan TIf }
<scN> "if" / { followsColonP }                    { addSpan TIf }
<scI> "then"                                      { addSpan TThen }
<0> "else"                                        { addSpan TElse }
<0> "else"\ *"if"                                 { addSpan TElsif }
<0> "end"\ *"if"                                  { addSpan TEndIf }
<0> "select"\ *"case"                             { addSpan TSelectCase }
<scN> "select"\ *"case" / { followsColonP }       { addSpan TSelectCase }
<0> "case"                                        { addSpan TCase }
<0> "end"\ *"select"                              { addSpan TEndSelect }
<scN> "default" / { caseStP }                     { addSpan TDefault }
<0,scI> "cycle"                                   { addSpan TCycle }
<0,scI> "exit"                                    { addSpan TExit }
<0,scI> "go"\ *"to"                               { addSpan TGoto }
<0,scI> "assign"                                  { addSpan TAssign }
<scN> "to" / { assignStP }                        { addSpan TTo }
<0,scI> "continue"                                { addSpan TContinue }
<0,scI> "stop"                                    { addSpan TStop }
<0,scI> "pause"                                   { addSpan TPause }
<0> "forall"                                      { addSpan TForall }
<0> "end"\ *"forall"                              { addSpan TEndForall }


-- Where construct
<0,scI> "where"                                   { addSpan TWhere }
<scN> "where" / { labelledWhereP }                { addSpan TWhere }
<0> "elsewhere"                                   { addSpan TElsewhere }
<0> "end"\ *"where"                               { addSpan TEndWhere }

-- Beginning keyword
<0> "data"                                        { addSpan TData }
<0,scI> "allocate"                                { addSpan TAllocate }
<scN> "stat" / { allocateP }                      { addSpan TStat }
<scN> "errmsg" / { allocateP }                    { addSpan TErrMsg }
<scN> "source" / { allocateP }                    { addSpan TSource }
<0,scI> "deallocate"                              { addSpan TDeallocate }
<0,scI> "nullify"                                 { addSpan TNullify }
<0> "namelist"                                    { addSpan TNamelist }
<0> "implicit"                                    { toSC scT >> addSpan TImplicit }
<0> "equivalence"                                 { addSpan TEquivalence }
<0> "common"                                      { addSpan TCommon }
<0> "end"                                         { addSpan TEnd }

<scT> "none"                                      { addSpan TNone }

-- I/O
<0,scI> "open"                                    { addSpan TOpen }
<0,scI> "close"                                   { addSpan TClose }
<0,scI> "read"                                    { addSpan TRead }
<0,scI> "write"                                   { addSpan TWrite }
<0,scI> "print"                                   { addSpan TPrint }
<0,scI> "backspace"                               { addSpan TBackspace }
<0,scI> "rewind"                                  { addSpan TRewind }
<0,scI> "inquire"                                 { addSpan TInquire }
<0,scI> "end"\ *"file"                            { addSpan TEndfile }
<0> "flush"                                       { addSpan TFlush }
<scN> "unit" / { followsFlushP }                  { addSpan TUnit }
<scN> "iostat" / { followsFlushP }                { addSpan TIOStat }
<scN> "iomsg" / { followsFlushP }                 { addSpan TIOMsg }
<scN> "err" / { followsFlushP }                   { addSpan TErr }

-- Format
<0> "format"                                      { addSpan TFormat }
<scN> "(".*")" / { formatP }                      { addSpanAndMatch TBlob }

-- Literals
<0> @label                                        { toSC 0 >> addSpanAndMatch TIntegerLiteral }
<scN,scI> @intLiteralConst                        { addSpanAndMatch TIntegerLiteral  }
<scN> @bozLiteralConst                            { addSpanAndMatch TBozLiteral }

<scN> @realLiteral                                { addSpanAndMatch TRealLiteral }
<scN> @altRealLiteral / { notPrecedingDotP }      { addSpanAndMatch TRealLiteral }

<scN,scC> @characterLiteralBeg                    { lexCharacter }

<scN> @logicalLiteral                             { addSpanAndMatch TLogicalLiteral }

-- Operators
<scN> ("."$letter+"."|"**"|\*|\/|\+|\-) / { opP } { addSpanAndMatch TOpCustom }
<scN> "**"                                        { addSpan TOpExp }
<scN> "+"                                         { addSpan TOpPlus }
<scN> "-"                                         { addSpan TOpMinus }
<scN> "*"                                         { addSpan TStar }
<scN> "/"                                         { slashOrDivision }
<scN> ".or."                                      { addSpan TOpOr }
<scN> ".and."                                     { addSpan TOpAnd }
<scN> ".not."                                     { addSpan TOpNot }
<scN> ".eqv."                                     { addSpan TOpEquivalent }
<scN> ".neqv."                                    { addSpan TOpNotEquivalent }
<scN> (".eq."|"==")                               { addSpan TOpEQ }
<scN> (".ne."|"/=")                               { addSpan TOpNE }
<scN> (".lt."|"<")                                { addSpan TOpLT }
<scN> (".le."|"<=")                               { addSpan TOpLE }
<scN> (".gt."|">")                                { addSpan TOpGT }
<scN> (".ge."|">=")                               { addSpan TOpGE }
<scN> "." $letter+ "."                            { addSpanAndMatch TOpCustom }

<scN> @name                                       { addSpanAndMatch TId }

{

--------------------------------------------------------------------------------
-- Predicated lexer helpers
--------------------------------------------------------------------------------

formatP :: User -> AlexInput -> Int -> AlexInput -> Bool
formatP _ _ _ ai
  | Just TFormat{} <- aiPreviousToken ai = True
  | otherwise = False

followsDoP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsDoP _ _ _ ai
  | Just TDo {} <- aiPreviousToken ai = True
  | otherwise = False

followsColonP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsColonP _ _ _ ai
  | Just TColon{} <- aiPreviousToken ai = True
  | otherwise = False

labelledWhereP :: User -> AlexInput -> Int -> AlexInput -> Bool
labelledWhereP _ _ _ ai
  | TId{}:TColon{}:[] <- prevTokens = True
  | otherwise                       = False
  where
    prevTokens = reverse . aiPreviousTokensInLine $ ai

selectorP :: User -> AlexInput -> Int -> AlexInput -> Bool
selectorP user _ _ ai =
    followsType && nextTokenIsOpAssign && precedesDoubleColon ai
  where
    nextTokenIsOpAssign = nextTokenConstr user ai == (Just . fillConstr $ TOpAssign)
    followsType =
      case searchBeforePar (aiPreviousTokensInLine ai) of
        Just x -> isTypeSpec x
        Nothing -> False
    searchBeforePar [] = Nothing
    searchBeforePar (x:xs)
      | TLeftPar{} <- x = if null xs then Nothing else (Just $ head xs)
      | otherwise = searchBeforePar xs

ifConditionEndP :: User -> AlexInput -> Int -> AlexInput -> Bool
ifConditionEndP (User _ pc) _ _ ai
    | (TIf{}:_) <- prevTokens = pc == ParanthesesCount 1 False
    | (TIntegerLiteral{}:TIf{}:_) <- prevTokens = pc == ParanthesesCount 1 False
    | (TId{}:TColon{}:TIf{}:_) <- prevTokens = pc == ParanthesesCount 1 False
    | (TElsif{}:_) <- prevTokens = pc == ParanthesesCount 1 False
    | otherwise = False
  where
    prevTokens = reverse . aiPreviousTokensInLine $ ai

opP :: User -> AlexInput -> Int ->AlexInput -> Bool
opP _ _ _ ai
  | (TLeftPar{}:TOperator{}:_) <- aiPreviousTokensInLine ai = True
  | otherwise = False

partOfExpOrPointerAssignmentP :: User -> AlexInput -> Int -> AlexInput -> Bool
partOfExpOrPointerAssignmentP (User fv pc) _ _ ai =
    case unParse (lexer $ f False (0::Integer)) ps of
      ParseOk True _ -> True
      _ -> False
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = StartCode scN Return }
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = pc
      , psContext = [ ConStart ] }
    f leftParSeen parCount token
      | not leftParSeen =
        case token of
          TNewline{} -> return False
          TSemiColon{} -> return False
          TEOF{} -> return False
          TPercent{} -> return True
          TArrow{} -> return True
          TOpAssign{} -> return True
          TLeftPar{} -> lexer $ f True 1
          TLeftPar2{} -> lexer $ f True 1
          _ -> return False
      | parCount == 0 =
        case token of
          TOpAssign{} -> return True
          TArrow{} -> return True
          TPercent{} -> return True
          TLeftPar{} -> lexer $ f True 1
          TLeftPar2{} -> lexer $ f True 1
          _ -> return False
      | parCount > 0 =
        case token of
          TNewline{} -> return False
          TSemiColon{} -> return False
          TEOF{} -> return False
          TLeftPar{} -> lexer $ f True (parCount + 1)
          TLeftPar2{} -> lexer $ f True (parCount + 1)
          TRightPar{} -> lexer $ f True (parCount - 1)
          _ -> lexer $ f True parCount
      | otherwise =
        error "Error while executing part of expression assignment predicate."

precedesDoubleColon :: AlexInput -> Bool
precedesDoubleColon ai = not . flip seenConstr ai . fillConstr $ TDoubleColon

parenLevel :: [Token] -> Int
parenLevel = foldl' f 0
  where
    f n tok | fillConstr TLeftPar == toConstr tok  = n + 1
            | fillConstr TRightPar == toConstr tok = n - 1
            | otherwise                            = n

allocateP :: User -> AlexInput -> Int -> AlexInput -> Bool
allocateP _ _ _ ai
  | alloc:lpar:rest <- prevTokens
  , fillConstr TAllocate == toConstr alloc
  , fillConstr TLeftPar  == toConstr lpar
  = null rest || (followsComma && parenLevel prevTokens == 1)
  | otherwise = False
  where
    prevTokens = reverse . aiPreviousTokensInLine $ ai
    followsComma
      | Just TComma{} <- aiPreviousToken ai = True
      | otherwise = False

attributeP :: User -> AlexInput -> Int -> AlexInput -> Bool
attributeP _ _ _ ai = followsComma && precedesDoubleColon ai && lineStartOK
  where
    followsComma
      | Just TComma{} <- aiPreviousToken ai = True
      | otherwise = False

    lineStartOK
      -- matches e.g.: TYPE (FOO), ATTR
      | typ:lpar:_:rpar:com:_ <- prevTokens
      , toConstr typ `elem` [fillConstr TType, fillConstr TClass]
      , toConstr lpar == fillConstr TLeftPar
      , toConstr rpar == fillConstr TRightPar
      = fillConstr TComma == toConstr com

      -- matches e.g.: TYPE FOO, ATTR
      | typ:com:_ <- prevTokens
      , toConstr typ == fillConstr TType
      = fillConstr TComma == toConstr com

      -- matches e.g.: INTEGER (KIND=...), ATTR
      -- or: PROCEDURE (...), ATTR
      | tok:lpar:rest <- prevTokens
      , isTypeSpec tok || fillConstr TProcedure == toConstr tok
      , fillConstr TLeftPar == toConstr lpar
      , (_, _:com:_) <- break ((fillConstr TRightPar ==) . toConstr) rest
      = fillConstr TComma == toConstr com

      -- matches e.g.: INTEGER*NUM, ATTR
      | tok:star:num:com:_ <- prevTokens
      , isTypeSpec tok
      , fillConstr TStar == toConstr star
      , TIntegerLiteral{} <- num
      = fillConstr TComma == toConstr com

      -- matches e.g.: INTEGER, ATTR
      -- or: USE, ATTR
      | tok:com:_ <- prevTokens
      , isTypeSpec tok || fillConstr TUse == toConstr tok
      = fillConstr TComma == toConstr com

      | otherwise = False

    prevTokens = reverse . aiPreviousTokensInLine $ ai

bindP :: User -> AlexInput -> Int -> AlexInput -> Bool
bindP _ _ _ ai = (followsRightPar && isFunSub) || (followsComma && isProcEnum)
  where
    followsComma
      | Just TComma{} <- aiPreviousToken ai = True
      | otherwise = False
    followsRightPar
      | Just TRightPar{} <- aiPreviousToken ai = True
      | otherwise = False
    isFunSub = flip any prevTokens $ \ token ->
      fillConstr TFunction == toConstr token ||
      fillConstr TSubroutine == toConstr token
    isProcEnum = flip any prevTokens $ \ token ->
      fillConstr TProcedure == toConstr token ||
      fillConstr TEnum == toConstr token
    prevTokens = reverse . aiPreviousTokensInLine $ ai

constructNameP :: User -> AlexInput -> Int -> AlexInput -> Bool
constructNameP user _ _ ai =
  case nextTokenConstr user ai of
    Just constr -> constr == fillConstr TColon
    _ -> False

genericSpecP :: User -> AlexInput -> Int -> AlexInput -> Bool
genericSpecP _ _ _ ai = Just True == do
  constr <- prevTokenConstr ai
  if constr `elem` fmap fillConstr [ TAbstract, TInterface, TPublic, TPrivate, TProtected ]
  then return True
  else if constr `elem` fmap fillConstr [ TComma, TDoubleColon ]
  then return $ seenConstr (fillConstr TPublic) ai ||
                seenConstr (fillConstr TPrivate) ai ||
                seenConstr (fillConstr TProtected) ai
  else Nothing

notDefinedOperP :: User -> AlexInput -> Int -> AlexInput -> Bool
notDefinedOperP _ _ _ ai
  | prevToken:_ <- prevTokens
  , fillConstr TOperator == toConstr prevToken  = False
  | prevToken:prevToken':_ <- prevTokens
  , fillConstr TLeftPar  == toConstr prevToken
  , fillConstr TOperator == toConstr prevToken' = False
  | otherwise                                   = True
  where
    prevTokens = aiPreviousTokensInLine ai

typeSpecP :: User -> AlexInput -> Int -> AlexInput -> Bool
typeSpecP _ _ _ ai
  | (prevToken:_) <- prevTokens
  , isTypeSpec prevToken = True
  | otherwise = isTypeSpecImmediatelyBefore $ reverse prevTokens
  where
    isTypeSpecImmediatelyBefore tokens@(_:xs)
      | isTypeSpec tokens = True
      | otherwise = isTypeSpecImmediatelyBefore xs
    isTypeSpecImmediatelyBefore [] = False
    prevTokens = aiPreviousTokensInLine ai

resultP :: User -> AlexInput -> Int -> AlexInput -> Bool
resultP _ _ _ ai =
    (flip seenConstr ai . fillConstr $ TFunction) &&
    prevTokenConstr ai == (Just $ fillConstr TRightPar)

notPrecedingDotP :: User -> AlexInput -> Int -> AlexInput -> Bool
notPrecedingDotP user _ _ ai = not $
  nextTokenConstr user ai == (Just $ toConstr (TId undefined undefined))

followsIntentP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsIntentP _ _ _ ai =
  (map toConstr . take 2 . aiPreviousTokensInLine) ai ==
  map fillConstr [ TLeftPar, TIntent ]

followsProcedureP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsProcedureP _ _ _ ai =
  (map toConstr . take 2 . aiPreviousTokensInLine) ai ==
  map fillConstr [ TLeftPar, TProcedure ]

followsBindP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsBindP _ _ _ ai =
  (map toConstr . take 2 . aiPreviousTokensInLine) ai ==
  map fillConstr [ TLeftPar, TBind ]

followsCP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsCP _ _ _ ai =
  (map toConstr . take 2 . aiPreviousTokensInLine) ai ==
  map fillConstr [ TComma, TC ]

followsFlushP :: User -> AlexInput -> Int -> AlexInput -> Bool
followsFlushP _ _ _ ai = not (null toks) && fillConstr TFlush == toConstr (last toks)
  where toks = aiPreviousTokensInLine ai

useStP :: User -> AlexInput -> Int -> AlexInput -> Bool
useStP _ _ _ ai = seenConstr (toConstr $ TUse undefined) ai

caseStP :: User -> AlexInput -> Int -> AlexInput -> Bool
caseStP _ _ _ ai = prevTokenConstr ai == (Just $ fillConstr TCase)

assignStP :: User -> AlexInput -> Int -> AlexInput -> Bool
assignStP _ _ _ ai = seenConstr (fillConstr TAssign) ai

prevTokenConstr :: AlexInput -> Maybe Constr
prevTokenConstr ai = toConstr <$> aiPreviousToken ai

nextTokenConstr :: User -> AlexInput -> Maybe Constr
nextTokenConstr (User fv pc) ai =
    case unParse lexer' parseState of
      ParseOk token _ -> Just $ toConstr token
      _ -> Nothing
  where
    parseState = ParseState
      { psAlexInput = ai
      , psParanthesesCount = pc
      , psVersion = fv
      , psFilename = "<unknown>"
      , psContext = [ ConStart ] }

seenConstr :: Constr -> AlexInput -> Bool
seenConstr candidateConstr ai =
  candidateConstr `elem` (toConstr <$> aiPreviousTokensInLine ai)

fillConstr = toConstr . ($ undefined)

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

adjustComment :: LexAction (Maybe Token) -> LexAction (Maybe Token)
adjustComment action = do
  mTok <- action
  case mTok of
    Just (TComment s (_:xs)) -> return $ Just $ TComment s xs
    _ -> error "Either not a comment token or matched empty."

leftPar :: LexAction (Maybe Token)
leftPar = do
    incPar
    context <- topContext
    if context == ConImplicit
      then do
        parseState <- get
        case unParse f parseState of
          ParseOk tokenCons _ -> do
            span <- getLexemeSpan
            return $ Just $ tokenCons span
          ParseFailed _ -> fail "Left parantheses is not matched."
      else addSpan TLeftPar
  where
    f :: LexAction (SrcSpan -> Token)
    f = do
      (ParanthesesCount pc _) <- getParanthesesCount
      mPrevToken <- aiPreviousToken <$> getAlex
      case mPrevToken of
        Just TRightPar{} | pc == 0 -> do
          _ <- getLexemeSpan
          curToken <- lexer'
          case curToken of
            TComma{} -> return TLeftPar2
            TNewline{} -> return TLeftPar2
            TSemiColon{} -> return TLeftPar2
            TEOF{} -> return TLeftPar2
            _ -> return TLeftPar
        _ -> lexer' >> f

comma :: LexAction (Maybe Token)
comma = do
  context <- topContext
  case context of
    ConImplicit -> do
      mToken <- aiPreviousToken <$> getAlex
      case mToken of
        Just TRightPar{} -> toSC scT >> addSpan TComma
        _ -> addSpan TComma
    ConNamelist -> secondCommaIfSlashFollows
    ConCommon -> secondCommaIfSlashFollows
    _ -> addSpan TComma
  where
    secondCommaIfSlashFollows = do
      parseState <- get
      case unParse lexer' parseState of
        ParseOk TOpDivision{} _ -> addSpan TComma2
        ParseFailed _ -> fail "Expecting variable name or slash."
        _ -> addSpan TComma

slashOrDivision :: LexAction (Maybe Token)
slashOrDivision = do
  context <- topContext
  case context of
    ConData -> addSpan TSlash
    _ -> addSpan TOpDivision

addSpan :: (SrcSpan -> Token) -> LexAction (Maybe Token)
addSpan cons = do
  s <- getLexemeSpan
  return $ Just $ cons s

addSpanAndMatch :: (SrcSpan -> String -> Token) -> LexAction (Maybe Token)
addSpanAndMatch cons = do
  s <- getLexemeSpan
  m <- getMatch
  return $ Just $ cons s m

getLexeme :: LexAction Lexeme
getLexeme = do
  ai <- getAlex
  return $ aiLexeme ai

putLexeme :: Lexeme -> LexAction ()
putLexeme lexeme = do
  ai <- getAlex
  putAlex $ ai { aiLexeme = lexeme }

resetLexeme :: LexAction ()
resetLexeme = putLexeme initLexeme

getMatch :: LexAction String
getMatch = do
  lexeme <- getLexeme
  return $ (reverse . lexemeMatch) lexeme

putMatch :: String -> LexAction ()
putMatch newMatch = do
  lexeme <- getLexeme
  putLexeme $ lexeme { lexemeMatch = reverse newMatch }

instance Spanned Lexeme where
  getSpan lexeme = SrcSpan (lexemeStart lexeme) (lexemeEnd lexeme)
  setSpan _ = error "Lexeme span cannot be set."

updatePreviousToken :: Maybe Token -> LexAction ()
updatePreviousToken maybeToken = do
  ai <- getAlex
  putAlex $ ai { aiPreviousToken = maybeToken }

addToPreviousTokensInLine :: Token -> LexAction ()
addToPreviousTokensInLine token = do
  ai <- getAlex
  putAlex $
    case token of
      TNewline _ -> updatePrevTokens ai [ ]
      TSemiColon _ -> updatePrevTokens ai [ ]
      t -> updatePrevTokens ai $ t : aiPreviousTokensInLine ai
  where
    updatePrevTokens ai tokens = ai { aiPreviousTokensInLine = tokens }

checkPreviousTokensInLine :: (Token -> Bool) -> AlexInput -> Bool
checkPreviousTokensInLine prop ai = any prop $ aiPreviousTokensInLine ai

getLexemeSpan :: LexAction SrcSpan
getLexemeSpan = do
  lexeme <- getLexeme
  return $ getSpan lexeme

-- Automata for character literal parsing is given below. Wherever it says '
-- you can replace ", whichever is used depends on what the first matched
-- character is and they are dual in their nature.
--
--      else
--       +-+
--       | v
--       +-+  Nothing  +-+
-- +---> |0|---------->|3|
--   +-> +-+           +-+
--   |    |
-- ' |    | '
--   |    v
--   |   +-+  Nothing  +-+
--   +---|1|---------->|2|
--       +-+           +-+
--        |             ^
--        +-------------+
--             else
--
-- For more information please refer to Fortran 90 standard's section related
-- to character constants.
lexCharacter :: LexAction (Maybe Token)
lexCharacter = do
    alex <- getAlex
    putAlex $ alex { aiStartCode = StartCode scC Stable }
    match <- getMatch
    let boundaryMarker = last match
    _lexChar (0::Integer) boundaryMarker
  where
    _lexChar 0 bm = do
      alex <- getAlex
      case alexGetByte alex of
        Just (_, newAlex) -> do
          putAlex newAlex
          m <- getMatch
          if last m == bm
          then _lexChar 1 bm
          else _lexChar 0 bm
        Nothing -> fail "Unmatched character literal."
    _lexChar 1 bm = do
      alex <- getAlex
      case alexGetByte alex of
        Just (_, newAlex) -> do
          let m = lexemeMatch . aiLexeme $ newAlex
          if head m == bm
          then do
            putAlex newAlex
            putMatch . reverse . tail $ m
            _lexChar 0 bm
          else _lexChar 2 bm
        Nothing -> _lexChar 2 bm
    _lexChar 2 _ = do
      alex <- getAlex
      putAlex $ alex { aiStartCode = StartCode scN Return }
      match <- getMatch
      putMatch . init . tail $ match
      addSpanAndMatch TString
    _lexChar _ _ = do fail "unhandled lexCharacter"

toSC :: Int -> LexAction ()
toSC startCode = do
  alex <- getAlex
  putAlex $ alex { aiStartCode = StartCode startCode Return }

stabiliseStartCode :: LexAction ()
stabiliseStartCode = do
  alex <- getAlex
  let sc = aiStartCode alex
  putAlex $ alex { aiStartCode = sc { scStatus = Stable } }

normaliseStartCode :: LexAction ()
normaliseStartCode = do
  alex <- getAlex
  let startCode = aiStartCode alex
  case scStatus startCode of
    Return -> putAlex $ alex { aiStartCode = StartCode scN Stable }
    Stable -> return ()

--------------------------------------------------------------------------------
-- AlexInput & related definitions
--------------------------------------------------------------------------------

invalidPosition :: Position
invalidPosition = Position 0 0 0 ""

{-# INLINE isValidPosition #-}
isValidPosition :: Position -> Bool
isValidPosition pos = posLine pos > 0

data Lexeme = Lexeme
  { lexemeMatch :: !String
  , lexemeStart :: {-# UNPACK #-} !Position
  , lexemeEnd   :: {-# UNPACK #-} !Position
  , lexemeIsCmt :: !Bool
  } deriving (Show)

initLexeme :: Lexeme
initLexeme = Lexeme
  { lexemeMatch = ""
  , lexemeStart = invalidPosition
  , lexemeEnd   = invalidPosition
  , lexemeIsCmt = False }

data StartCodeStatus = Return | Stable deriving (Show)

data StartCode = StartCode
  { scActual :: {-# UNPACK #-} !Int
  , scStatus :: !StartCodeStatus }
  deriving (Show)

data AlexInput = AlexInput
  { aiSourceBytes               :: !B.ByteString
  , aiPosition                  :: {-# UNPACK #-} !Position
  , aiEndOffset                 :: {-# UNPACK #-} !Int
  , aiPreviousChar              :: {-# UNPACK #-} !Char
  , aiLexeme                    :: {-# UNPACK #-} !Lexeme
  , aiStartCode                 :: {-# UNPACK #-} !StartCode
  , aiPreviousToken             :: !(Maybe Token)
  , aiPreviousTokensInLine      :: !([ Token ])
  } deriving (Show)

instance Loc AlexInput where
  getPos = aiPosition

instance LastToken AlexInput Token where
  getLastToken = aiPreviousToken

type LexAction a = Parse AlexInput Token a

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput
  { aiSourceBytes          = B.empty
  , aiPosition             = initPosition
  , aiEndOffset            = 0
  , aiPreviousChar         = '\n'
  , aiLexeme               = initLexeme
  , aiStartCode            = StartCode 0 Return
  , aiPreviousToken        = Nothing
  , aiPreviousTokensInLine = [ ] }

updateLexeme :: Char -> Position -> AlexInput -> AlexInput
updateLexeme !char !p !ai = ai { aiLexeme = Lexeme (char:match) start' p isCmt' }
  where
    Lexeme match start _ isCmt = aiLexeme ai
    start'                     = if isValidPosition start then start else p
    isCmt'                     = isCmt || (null match && char == '!')

-- Fortran version and parantheses count to be used by alexScanUser
data User = User FortranVersion ParanthesesCount

--------------------------------------------------------------------------------
-- Definitions needed for alexScanUser
--------------------------------------------------------------------------------

data Move = Continuation | Char | Newline

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte !ai
  -- When all characters are already read
  | posAbsoluteOffset _position == aiEndOffset ai = Nothing
  -- Skip the continuation line altogether
  | isContinuation ai = alexGetByte . skipContinuation $ ai
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise =
      Just ( fromIntegral . fromEnum $ _curChar
           , updateLexeme _curChar _position
               ai
               { aiPosition =
                   case _curChar of
                     '\n'  -> advance Newline _position
                     _     -> advance Char _position
               , aiPreviousChar = _curChar })
  where
    _curChar = currentChar ai
    _position = aiPosition ai

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = aiPreviousChar ai

currentChar :: AlexInput -> Char
currentChar !ai
  -- case sensitivity matters only in character literals
  | sCode == scC              = _currentChar
  | 'A' <= _currentChar &&
     _currentChar <= 'Z'      = {-# SCC toLower_currentChar #-} toLower _currentChar
  | otherwise                 = _currentChar
  where
    sCode        = scActual (aiStartCode ai)
    -- _currentChar = w2c (BU.unsafeIndex srcBytes i)
    _currentChar = B.index srcBytes absOff
    srcBytes     = aiSourceBytes ai
    absOff       = posAbsoluteOffset pos
    pos          = aiPosition ai

advanceWithoutContinuation :: AlexInput -> Maybe AlexInput
advanceWithoutContinuation !ai
  -- When all characters are already read
  | posAbsoluteOffset _position == aiEndOffset ai =
    Nothing
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise =
    Just $! ai { aiPosition =
                  case _curChar of
                    '\n'  -> advance Newline _position
                    _     -> advance Char _position
               , aiPreviousChar = _curChar }
  where
    _curChar = currentChar ai
    _position = aiPosition ai

isContinuation :: AlexInput -> Bool
isContinuation !ai =
    -- No continuation while lexing a character literal.
    (scActual . aiStartCode) ai /= scC
    -- No continuation while lexing a comment.
    && (null match || not (lexemeIsCmt lexeme))
    && _isContinuation ai (0::Integer)
  where
    match  = lexemeMatch lexeme
    lexeme = aiLexeme $ ai
    _isContinuation !ai' 0 =
      if currentChar ai' == '&'
      then _advance ai'
      else False
    _isContinuation !ai' 1 =
      case currentChar ai' of
        ' ' -> _advance ai'
        '\t' -> _advance ai'
        '\r' -> _advance ai'
        '!' -> True
        '\n' -> True
        _ -> False
    _isContinuation _ _ = False
    _advance :: AlexInput -> Bool
    _advance !ai' =
      case advanceWithoutContinuation ai' of
        Just ai'' -> _isContinuation ai'' (1::Integer)
        Nothing -> False

-- Here's the skip continuation automaton:
--
--              white     white,\n
--               +-+        +-+
--               | v        | v        +---+
--     +-+   &   +-+   \n   +-+   &    |---|
-- +-->|0|------>|1|------->|3|------->||4||
--     +-+       +-+        +-+----+   |---|
--                |          ^     |   +---+
--                |!         |     |
--                v          |     |else
--            +->+-+         |     v
--        else|  |2|---------+   +---+
--            +--+-+             |---|
--                               ||5||
--                               |---|
--                               +---+
--
-- For more information refer to Fortran 90 standard.
-- This version is more permissive than the specification
-- as it allows empty lines to be used between continuations.
skipContinuation :: AlexInput -> AlexInput
skipContinuation ai' = _skipCont ai' (0::Integer)
  where
    _skipCont ai 0 =
      if currentChar ai == '&'
      then _advance ai 1
      else error "This case is excluded by isContinuation."
    _skipCont ai 1 =
      let _curChar = currentChar ai in
        if _curChar `elem` [' ', '\t', '\r']
        then _advance ai 1
        else if _curChar == '!'
        then _advance ai 2
        else if _curChar == '\n'
        then _advance ai 3
        else
          error $
            join [ "Did not expect non-blank/non-comment character after "
                 , "continuation symbol (&)." ]
    _skipCont ai 2 =
      if currentChar ai == '\n'
      then _advance ai 3
      else _advance ai 2
    _skipCont ai 3 =
      let _curChar = currentChar ai in
        if _curChar `elem` [' ', '\t', '\r', '\n']
        then _advance ai 3
        else if _curChar == '!'
        then _advance ai 2
        else if _curChar == '&'
        -- This state accepts as if there were no spaces between the broken
        -- line and whatever comes after second &. This is implicitly state (4)
        then fromMaybe (error "File has ended prematurely during a continuation.")
                       (advanceWithoutContinuation ai)
        -- This state accepts but the broken line delimits the previous token.
        -- This is implicitly state (5). To achieve this, it returns the
        -- previous ai, which either has whitespace or newline, so it will
        -- nicely delimit.
        else ai
    _skipCont _ _ = error "unhandled _skipCont in skipContinuation"
    _advance ai state =
      case advanceWithoutContinuation ai of
        Just ai'' -> _skipCont ai'' state
        Nothing -> error "File has ended prematurely during a continuation."

advance :: Move -> Position -> Position
advance move position =
  case move of
    Newline ->
      position
        { posAbsoluteOffset = _absl + 1 , posColumn = 1 , posLine = _line + 1 }
    Char ->
      position { posAbsoluteOffset = _absl + 1 , posColumn = _col + 1 }
-- for now just return the original position
    _ -> position { posAbsoluteOffset = _absl, posColumn = _col }
  where
    _col = posColumn position
    _line = posLine position
    _absl = posAbsoluteOffset position

--------------------------------------------------------------------------------
-- Lexer definition
--------------------------------------------------------------------------------

lexer :: (Token -> LexAction a) -> LexAction a
lexer cont = cont =<< lexer'

lexer' :: LexAction Token
lexer' = do
  resetLexeme
  alex <- getAlex
  let startCode = scActual . aiStartCode $ alex
  normaliseStartCode
  newAlex' <- getAlex
  version <- getVersion
  paranthesesCount <- getParanthesesCount
  let user = User version paranthesesCount
  case alexScanUser user newAlex' startCode of
    AlexEOF -> return $ TEOF $ SrcSpan (getPos alex) (getPos alex)
    AlexError _ -> do
      parseState <- get
      fail $ psFilename parseState ++ ": lexing failed. "
#ifdef DEBUG
        ++ '\n' : show newAlex ++ "\n"
#endif
    AlexSkip newAlex _ -> do
      putAlex $ newAlex { aiStartCode = StartCode startCode Return }
      lexer'
    AlexToken newAlex _ action -> do
      putAlex newAlex
      maybeToken <- action
      case maybeToken of
        Just token -> do
          updatePreviousToken maybeToken
          addToPreviousTokensInLine token
          return token
        Nothing -> lexer'

alexScanUser :: User -> AlexInput -> Int -> AlexReturn (LexAction (Maybe Token))

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

data Token =
    TId                 SrcSpan String
  | TComment            SrcSpan String
  | TString             SrcSpan String
  | TIntegerLiteral     SrcSpan String
  | TRealLiteral        SrcSpan String
  | TBozLiteral         SrcSpan String
  | TComma              SrcSpan
  | TComma2             SrcSpan
  | TSemiColon          SrcSpan
  | TColon              SrcSpan
  | TDoubleColon        SrcSpan
  | TOpAssign           SrcSpan
  | TArrow              SrcSpan
  | TPercent            SrcSpan
  | TLeftPar            SrcSpan
  | TLeftPar2           SrcSpan
  | TRightPar           SrcSpan
  | TLeftInitPar        SrcSpan
  | TRightInitPar       SrcSpan
  -- Mainly operators
  | TOpCustom           SrcSpan String
  | TOpExp              SrcSpan
  | TOpPlus             SrcSpan
  | TOpMinus            SrcSpan
  | TStar               SrcSpan
  | TOpDivision         SrcSpan
  | TSlash              SrcSpan
  | TOpOr               SrcSpan
  | TOpAnd              SrcSpan
  | TOpNot              SrcSpan
  | TOpEquivalent       SrcSpan
  | TOpNotEquivalent    SrcSpan
  | TOpLT               SrcSpan
  | TOpLE               SrcSpan
  | TOpEQ               SrcSpan
  | TOpNE               SrcSpan
  | TOpGT               SrcSpan
  | TOpGE               SrcSpan
  | TLogicalLiteral     SrcSpan String
  -- Keywords
  -- Program unit related
  | TProgram            SrcSpan
  | TEndProgram         SrcSpan
  | TFunction           SrcSpan
  | TEndFunction        SrcSpan
  | TResult             SrcSpan
  | TPure               SrcSpan
  | TElemental          SrcSpan
  | TRecursive          SrcSpan
  | TSubroutine         SrcSpan
  | TEndSubroutine      SrcSpan
  | TBlockData          SrcSpan
  | TEndBlockData       SrcSpan
  | TModule             SrcSpan
  | TEndModule          SrcSpan
  | TContains           SrcSpan
  | TUse                SrcSpan
  | TOnly               SrcSpan
  | TImport             SrcSpan
  | TAbstract           SrcSpan
  | TInterface          SrcSpan
  | TEndInterface       SrcSpan
  | TProcedure          SrcSpan
  | TModuleProcedure    SrcSpan
  | TAssignment         SrcSpan
  | TOperator           SrcSpan
  | TCall               SrcSpan
  | TReturn             SrcSpan
  | TEntry              SrcSpan
  | TInclude            SrcSpan
  -- language-binding-spec
  | TBind               SrcSpan
  | TC                  SrcSpan
  | TName               SrcSpan
  -- Attributes
  | TAllocatable        SrcSpan
  | TAsynchronous       SrcSpan
  | TDimension          SrcSpan
  | TExternal           SrcSpan
  | TIntent             SrcSpan
  | TIntrinsic          SrcSpan
  | TNonIntrinsic       SrcSpan
  | TOptional           SrcSpan
  | TParameter          SrcSpan
  | TPointer            SrcSpan
  | TPrivate            SrcSpan
  | TPublic             SrcSpan
  | TProtected          SrcSpan
  | TSave               SrcSpan
  | TTarget             SrcSpan
  | TValue              SrcSpan
  | TVolatile           SrcSpan
  -- Attribute values
  | TIn                 SrcSpan
  | TOut                SrcSpan
  | TInOut              SrcSpan
  -- Beginning keyword
  | TData               SrcSpan
  | TNamelist           SrcSpan
  | TImplicit           SrcSpan
  | TEquivalence        SrcSpan
  | TCommon             SrcSpan
  | TFormat             SrcSpan
  | TBlob               SrcSpan String
  | TAllocate           SrcSpan
  | TStat               SrcSpan
  | TErrMsg             SrcSpan
  | TSource             SrcSpan
  | TDeallocate         SrcSpan
  | TNullify            SrcSpan
  -- Misc
  | TNone               SrcSpan
  -- Control flow
  | TGoto               SrcSpan
  | TAssign             SrcSpan
  | TTo                 SrcSpan
  | TContinue           SrcSpan
  | TStop               SrcSpan
  | TPause              SrcSpan
  | TDo                 SrcSpan
  | TEndDo              SrcSpan
  | TWhile              SrcSpan
  | TIf                 SrcSpan
  | TThen               SrcSpan
  | TElse               SrcSpan
  | TElsif              SrcSpan
  | TEndIf              SrcSpan
  | TCase               SrcSpan
  | TSelectCase         SrcSpan
  | TEndSelect          SrcSpan
  | TDefault            SrcSpan
  | TCycle              SrcSpan
  | TExit               SrcSpan
  | TForall             SrcSpan
  | TEndForall          SrcSpan
  -- Where construct
  | TWhere              SrcSpan
  | TElsewhere          SrcSpan
  | TEndWhere           SrcSpan
  -- Type related
  | TType               SrcSpan
  | TEndType            SrcSpan
  | TSequence           SrcSpan
  | TClass              SrcSpan
  | TEnum               SrcSpan
  | TEnumerator         SrcSpan
  | TEndEnum            SrcSpan
  -- Selector
  | TKind               SrcSpan
  | TLen                SrcSpan
  -- Intrinsic types
  | TInteger            SrcSpan
  | TReal               SrcSpan
  | TDoublePrecision    SrcSpan
  | TLogical            SrcSpan
  | TCharacter          SrcSpan
  | TComplex            SrcSpan
  -- I/O
  | TOpen               SrcSpan
  | TClose              SrcSpan
  | TRead               SrcSpan
  | TWrite              SrcSpan
  | TPrint              SrcSpan
  | TBackspace          SrcSpan
  | TRewind             SrcSpan
  | TInquire            SrcSpan
  | TEndfile            SrcSpan
  -- Etc.
  | TEnd                SrcSpan
  | TNewline            SrcSpan
  | TEOF                SrcSpan
  | TFlush              SrcSpan
  | TUnit               SrcSpan
  | TIOStat             SrcSpan
  | TIOMsg              SrcSpan
  | TErr                SrcSpan
  deriving (Eq, Show, Data, Typeable, Generic)

instance FirstParameter Token SrcSpan
instance FirstParameter Token SrcSpan => Spanned Token where
  getSpan = getFirstParameter
  setSpan = setFirstParameter

instance Tok Token where
  eofToken TEOF{} = True
  eofToken _ = False

class SpecifiesType a where
  isTypeSpec :: a -> Bool

instance SpecifiesType Token where
  isTypeSpec TInteger{} = True
  isTypeSpec TReal{} = True
  isTypeSpec TDoublePrecision{} = True
  isTypeSpec TLogical{} = True
  isTypeSpec TCharacter{} = True
  isTypeSpec TComplex{} = True
  isTypeSpec _ = False

instance SpecifiesType [ Token ] where
  isTypeSpec tokens
    | [ TType{}, TLeftPar{}, _, TRightPar{} ] <- tokens = True
    -- This is an approximation but should hold for almost all legal programs.
    | (typeToken:TLeftPar{}:rest) <- tokens =
      isTypeSpec typeToken &&
      case last rest of
        TRightPar{} -> True
        _ -> False
    | (TCharacter{}:TStar{}:rest) <- tokens =
      case rest of
        [ TIntegerLiteral{} ] -> True
        (TLeftPar{}:rest') | TRightPar{} <- last rest' -> True
        _ -> False
    | otherwise = False

--------------------------------------------------------------------------------
-- Functions to help testing & output
--------------------------------------------------------------------------------

initParseState :: B.ByteString -> FortranVersion -> String -> ParseState AlexInput
initParseState srcBytes fortranVersion filename =
  _vanillaParseState { psAlexInput = _vanillaAlexInput }
  where
    _vanillaParseState = ParseState
      { psAlexInput = undefined
      , psVersion = fortranVersion
      , psFilename = filename
      , psParanthesesCount = ParanthesesCount 0 False
      , psContext = [ ConStart ] }
    _vanillaAlexInput = vanillaAlexInput
      { aiSourceBytes = srcBytes
      , aiEndOffset   = B.length srcBytes
      , aiPosition    = initPosition {filePath = filename} }

collectFreeTokens :: FortranVersion -> B.ByteString -> [Token]
collectFreeTokens version srcInput =
    collectTokens lexer' $ initParseState srcInput version "<unknown>"

}
