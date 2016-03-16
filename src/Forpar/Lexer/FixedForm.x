{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Forpar.Lexer.FixedForm where

import Data.Word (Word8)
import Data.Char (toLower, isDigit, ord)
import Data.List (isPrefixOf, isSuffixOf, any)
import Data.Maybe (fromJust, isNothing)
import Data.Data
import Data.Typeable
import qualified Data.Bits

import Control.Exception
import Control.Monad.State
import Control.Monad (liftM2)

import GHC.Exts
import GHC.Generics

import Forpar.ParserMonad

import Forpar.Util.FirstParameter
import Forpar.Util.Position

import Debug.Trace

}

$digit = [0-9]
$letter = [a-z]
$alphanumeric = [$letter $digit]
$special = [\ \=\+\-\*\/\(\)\,\.\$]

@id = $letter $alphanumeric{0,5}
@label = [1-9] $digit{0,4}

@datatype = "integer" | "real" | "doubleprecision" | "complex" | "logical"

-- Numbers
@integerConst = $digit+ -- Integer constant
@posIntegerConst = [1-9] $digit*

-- For reals
@exponent = [ed] [\+\-]? @integerConst

-- For format items
@repeat = @posIntegerConst?
@width = @posIntegerConst

tokens :-

  <0> [c!\*d] / { commentP }                  { lexComment Nothing }
  <0> @label / { withinLabelColsP }           { addSpanAndMatch TLabel }
  <0> . / { \_ ai _ _ -> atColP 6 ai }        { toSC keyword }
  <0> " "                                     ;

  <0,st,keyword,iif> \n                       { toSC 0 >> addSpan TNewline }
  <0,st,keyword,iif> \r                       ;

  <st> "("                                    { addSpan TLeftPar }
  <iif> "("                                   { incPar >> addSpan TLeftPar }
  <st> ")"                                    { addSpan TRightPar }
  <iif> ")"                                   { maybeToKeyword >> addSpan TRightPar }
  <st,iif,keyword> ","                        { addSpan TComma }
  <st,iif> "."                                { addSpan TDot }
  <st,iif> ":" / { fortran77P }               { addSpan TColon }

  <keyword> @id / { idP }                     { toSC st >> addSpanAndMatch TId }

  -- Tokens related to procedures and subprograms
  <keyword> "program"                         { toSC st >> addSpan TProgram }
  <keyword> "function"                        { toSC st >> addSpan TFunction  }
  <keyword> "subroutine"                      { toSC st >> addSpan TSubroutine  }
  <keyword> "blockdata"                       { toSC st >> addSpan TBlockData  }
  <keyword> "end"                             { toSC st >> addSpan TEnd  }

  -- Tokens related to assignment statements
  <keyword> "assign"                          { toSC st >> addSpan TAssign  }
  <st,iif> "="                                { addSpan TOpAssign  }
  <st> "to"                                   { addSpan TTo  }

  -- Tokens related to control statements
  <keyword> "goto"                            { toSC st >> addSpan TGoto  }
  <keyword> "if"                              { toSC iif >> addSpan TIf  }
  <st> "if" / { fortran77P }                  { toSC iif >> addSpan TIf  }
  <st,keyword> "then" / { fortran77P }        { toSC keyword >> addSpan TThen  }
  <keyword> "else" / {fortran77P }            { addSpan TElse  }
  <keyword> "elseif" / {fortran77P }          { toSC st >> addSpan TElsif  }
  <keyword> "endif" / {fortran77P }           { addSpan TEndif  }
  <keyword> "call"                            { toSC st >> addSpan TCall  }
  <keyword> "return"                          { toSC st >> addSpan TReturn  }
  <keyword> "save" / { fortran77P }           { toSC st >> addSpan TSave  }
  <keyword> "continue"                        { toSC st >> addSpan TContinue  }
  <keyword> "stop"                            { toSC st >> addSpan TStop  }
  <keyword> "pause"                           { toSC st >> addSpan TPause  }
  <keyword> "do"                              { toSC st >> addSpan TDo  }

  -- Tokens related to I/O statements
  <keyword> "read"                            { toSC st >> addSpan TRead  }
  <keyword> "write"                           { toSC st >> addSpan TWrite  }
  <keyword> "rewind"                          { toSC st >> addSpan TRewind  }
  <keyword> "backspace"                       { toSC st >> addSpan TBackspace  }
  <keyword> "endfile"                         { toSC st >> addSpan TEndfile  }
  <keyword> "inquire" / { fortran77P }        { toSC st >> addSpan TInquire  }
  <keyword> "open" / { fortran77P }           { toSC st >> addSpan TOpen  }
  <keyword> "close" / { fortran77P }          { toSC st >> addSpan TClose  }
  <keyword> "print" / { fortran77P }          { toSC st >> addSpan TPrint  }

  -- Tokens related to non-executable statements

  -- Tokens related to speification statements
  <keyword> "dimension"                       { toSC st >> addSpan TDimension  }
  <keyword> "common"                          { toSC st >> addSpan TCommon  }
  <keyword> "equivalence"                     { toSC st >> addSpan TEquivalence  }
  <keyword> "external"                        { toSC st >> addSpan TExternal  }
  <keyword> "intrinsic" / { fortran77P }      { toSC st >> addSpan TIntrinsic  }
  <keyword> @datatype                         { typeSCChange >> addSpanAndMatch TType }
  <st> @datatype / { implicitTypeP }          { addSpanAndMatch TType }
  <keyword> "character" / { fortran77P }      { toSC st >> addSpanAndMatch TType }
  <st> "character" / { implicitTypeP }        { addSpanAndMatch TType }
  <keyword> "implicit" / { fortran77P }       { toSC st >> addSpan TImplicit  }
  <st> "none" / { fortran77P }                { addSpan TNone  }
  <keyword> "parameter" / { fortran77P }      { toSC st >> addSpan TParameter  }
  <keyword> "entry" / { fortran77P }          { toSC st >> addSpan TEntry  }

  -- Tokens related to data initalization statement
  <keyword> "data"                            { toSC st >> addSpan TData  }

  -- Tokens related to format statement
  <keyword> "format"                          { toSC st >> addSpan TFormat  }

  -- Tokens needed to parse integers, reals, double precision and complex 
  -- constants
  <st,iif> @exponent / { exponentP }          { addSpanAndMatch TExponent }
  <st,iif,keyword> @integerConst              { addSpanAndMatch TInt }

  -- String
  <st,iif> \' / { fortran77P }                { strAutomaton 0 }

  -- Logicals
  <st,iif> ".true."                           { addSpan TTrue  }
  <st,iif> ".false."                          { addSpan TFalse  }

  -- Arithmetic operators
  <st,iif> "+"                                { addSpan TOpPlus  }
  <st,iif> "-"                                { addSpan TOpMinus  }
  <st,iif> "**"                               { addSpan TOpExp  }
  <st,iif> "*"                                { addSpan TStar  }
  <st,iif> "/"                                { addSpan TSlash  }

  -- Logical operators
  <st,iif> ".or."                             { addSpan TOpOr  }
  <st,iif> ".and."                            { addSpan TOpAnd  }
  <st,iif> ".not."                            { addSpan TOpNot  }
  <st,iif> ".eqv." / { fortran77P }           { addSpan TOpEquivalent  }
  <st,iif> ".neqv." / { fortran77P }          { addSpan TOpNotEquivalent  }

  -- Relational operators
  <st,iif> ".lt."                             { addSpan TOpLT  }
  <st,iif> ".le."                             { addSpan TOpLE  }
  <st,iif> ".eq."                             { addSpan TOpEQ  }
  <st,iif> ".ne."                             { addSpan TOpNE  }
  <st,iif> ".gt."                             { addSpan TOpGT  }
  <st,iif> ".ge."                             { addSpan TOpGE  }

  -- Field descriptors
  <st> @repeat [defg] @width \. @integerConst { lexFieldDescriptorDEFG }
  <st> @repeat [ail] @width                   { lexFieldDescriptorAIL }
  <st> @width x                               { lexBlankDescriptor }
  <st> "-"? @posIntegerConst p                { lexScaleFactor }

  -- ID
  <st,iif> @id                                { addSpanAndMatch TId }

  -- Strings
  <st> @posIntegerConst "h" / { fortran66P }  { lexHollerith }

{

--------------------------------------------------------------------------------
-- Predicated lexer helpers
--------------------------------------------------------------------------------

implicitTypeP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
implicitTypeP a b c d = implicitStP a b c d

implicitStP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
implicitStP fv _ _ ai = checkPreviousTokensInLine f ai
  where
    f (TImplicit _) = True
    f _ = False


idP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
idP fv _ _ ai = not (doP ai) && equalFollowsP fv ai

doP :: AlexInput -> Bool
doP ai = isPrefixOf "do" (reverse . lexemeMatch . aiLexeme $ ai)

equalFollowsP :: FortranVersion -> AlexInput -> Bool
equalFollowsP fv ai = evalParse (lexerM $ f False 0) ps
  where
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = 0 }
    f False 0 t = 
      case t of 
        Just (TNewline _) -> return False
        Just (TEOF _) -> return False
        Just (TOpAssign _) -> return True
        Just (TLeftPar _) -> lexerM $ f True 1 
        _ -> return False
    f True 0 t = 
      case t of
        Just (TOpAssign _) -> return True
        _ -> return False
    f True n t = 
      case t of
        Just (TNewline _) -> return False
        Just (TEOF _) -> return False
        Just (TLeftPar _) -> lexerM $ f True (n + 1)
        Just (TRightPar _) -> lexerM $ f True (n - 1)
        _ -> lexerM $ f True n


commentP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
commentP _ aiOld _ aiNew = atColP 1 aiOld && _endsWithLine
  where
    _endsWithLine = (posColumn . aiPosition) aiNew /= 1

withinLabelColsP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
withinLabelColsP _ aiOld _ aiNew = getCol aiOld >= 1 && getCol aiNew <= 6
  where
    getCol = posColumn . aiPosition

atColP :: Integer -> AlexInput -> Bool
atColP n ai = (posColumn . aiPosition) ai == n

-- This predicate allows to distinguish identifiers and real exponent tokens
-- by looking at previous token. Since exponent can only follow a "." or an
-- integer token. Anything other previous token will prevent matching the input
-- as an exponent token.
exponentP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
exponentP _ _ _ ai = 
  case aiPreviousToken ai of
    Just (TInt _ _) -> True
    Just (TDot _) -> True
    _ -> False

fortran66P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
fortran66P Fortran66 _ _ _ = True
fortran66P _ _ _ _ = False

fortran77P :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
fortran77P Fortran77 _ _ _ = True
fortran77P _ _ _ _ = False

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

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

incWhiteSensitiveCharCount :: LexAction ()
incWhiteSensitiveCharCount = do
  ai <- getAlex
  let wsc = aiWhiteSensitiveCharCount ai
  putAlex $ ai { aiWhiteSensitiveCharCount = wsc + 1 }

resetWhiteSensitiveCharCount :: LexAction ()
resetWhiteSensitiveCharCount = do
  ai <- getAlex
  putAlex $ ai { aiWhiteSensitiveCharCount = 0 }

instance Spanned Lexeme where
  getSpan lexeme = 
    let ms = lexemeStart lexeme 
        me = lexemeEnd lexeme in
      SrcSpan (fromJust ms) (fromJust me)
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
      t -> updatePrevTokens ai $ t : aiPreviousTokensInLine ai
  where
    updatePrevTokens ai tokens = ai { aiPreviousTokensInLine = tokens }

checkPreviousTokensInLine :: (Token -> Bool) -> AlexInput -> Bool
checkPreviousTokensInLine prop ai = any prop $ aiPreviousTokensInLine ai

getLexemeSpan :: LexAction SrcSpan
getLexemeSpan = do
  lexeme <- getLexeme
  return $ getSpan lexeme

-- With the existing alexGetByte implementation comments are matched without
-- whitespace characters. However, we have access to final column number,
-- we know the comment would start at column, and we have access to the absolute
-- offset so instead of using match, lexComment takes a slice from the original
-- source input
lexComment :: Maybe Char -> LexAction (Maybe Token)
lexComment mc = do
  m <- getMatch
  s <- getLexemeSpan
  alex <- getAlex
  let modifiedAlex = alex { aiWhiteSensitiveCharCount = 1 }
  case mc of
    Just '\n' -> return $ Just $ TComment s $ tail m
    Just _ -> 
      case alexGetByte modifiedAlex of
        Just (_, newAlex) -> do
          putAlex newAlex
          lexComment Nothing
        Nothing -> fail "Comment abruptly ended."
    Nothing -> 
      case alexGetByte modifiedAlex of
        Just (_, newAlex) -> lexComment (Just $ (head . lexemeMatch . aiLexeme) newAlex)
        Nothing -> return $ Just $ TComment s $ tail m


{-
     Chars
      +-+
      | |
      | |
      | v
      +-+  Nothing  +-+
+---> |0|---------->+3|
  +-> +++           +-+
  |    |
' |    | '
  |    v
  |   +++  Nothing  +-+
  +---|1|----------->2|
      +++           +++
       |             ^
       +-------------+
            Chars
-}
strAutomaton :: Int -> LexAction (Maybe Token)
strAutomaton 0 = do
  incWhiteSensitiveCharCount
  alex <- getAlex
  case alexGetByte alex of
    Just (_, newAlex) -> do
      putAlex newAlex
      m <- getMatch
      if last m == '\''
      then strAutomaton 1
      else strAutomaton 0
    Nothing -> strAutomaton 3
strAutomaton 1 = do
  incWhiteSensitiveCharCount
  alex <- getAlex
  case alexGetByte alex of
    Just (_, newAlex) -> do
      let m = lexemeMatch . aiLexeme $ newAlex
      if head m == '\''
      then do
        putAlex newAlex
        putMatch $ reverse . tail $ m
        strAutomaton 0
      else strAutomaton 2
    Nothing -> strAutomaton 2
strAutomaton 2 = do
  s <- getLexemeSpan
  m <- getMatch
  resetWhiteSensitiveCharCount
  return $ Just $ TString s $ (init . tail) m
strAutomaton 3 = fail "Unmatched string."

lexHollerith :: LexAction (Maybe Token)
lexHollerith = do
  match' <- getMatch
  let len = read $ init match' -- Get n of "nH" from string
  putMatch ""
  ai <- getAlex
  putAlex $ ai { aiWhiteSensitiveCharCount = len } 
  lexed <- lexN len
  s <- getLexemeSpan
  return $ do
    hollerith <- lexed
    return $ THollerith s hollerith

lexN :: Int -> LexAction (Maybe String)
lexN n = do
  alex <- getAlex
  match' <- getMatch
  let len = length match'
  if n == len
  then return $ Just match'
  else 
    case alexGetByte alex of
      Just (_, newAlex) -> do
        putAlex newAlex
        lexN n
      Nothing -> return Nothing

-- Lexing various field descriptors

lexFieldDescriptorDEFG :: LexAction (Maybe Token)
lexFieldDescriptorDEFG = do
  match <- getMatch
  let (repeat, descriptor, width, rest) = takeRepeatDescriptorWidth match
  let fractionWidth = (read $ fst $ takeNumber $ tail rest) :: Integer
  s <- getLexemeSpan
  return $ Just $ TFieldDescriptorDEFG s repeat descriptor width fractionWidth

lexFieldDescriptorAIL :: LexAction (Maybe Token)
lexFieldDescriptorAIL = do
  match <- getMatch
  let (repeat, descriptor, width, rest) = takeRepeatDescriptorWidth match
  s <- getLexemeSpan
  return $ Just $ TFieldDescriptorAIL s repeat descriptor width

lexBlankDescriptor :: LexAction (Maybe Token)
lexBlankDescriptor = do
  match <- getMatch
  let (width, _) = takeNumber match
  s <- getLexemeSpan
  return $ Just $ TBlankDescriptor s (read width :: Integer)

lexScaleFactor :: LexAction (Maybe Token)
lexScaleFactor = do
  match <- getMatch
  let (sign, rest) = if head match == '-' then (-1, tail match) else (1, match)
  let (width, _) = takeNumber rest
  s <- getLexemeSpan
  return $ Just $ TScaleFactor s $ (read width) * sign

takeRepeatDescriptorWidth :: String -> (Maybe Integer, Char, Integer, String)
takeRepeatDescriptorWidth str = 
  let (repeatStr, rest) = takeNumber str
      repeat = if repeatStr == [] then Nothing else Just $ (read repeatStr :: Integer)
      descriptor = head rest
      (widthStr, rest') = takeNumber $ tail rest
      width = read widthStr :: Integer in
    (repeat, descriptor, width, rest')

takeNumber :: String -> (String, String)
takeNumber str = span isDigit str

maybeToKeyword :: LexAction (Maybe Token)
maybeToKeyword = do
  decPar
  ps <- get
  if psParanthesesCount ps == 0
  then toSC keyword
  else return Nothing

typeSCChange :: LexAction (Maybe Token)
typeSCChange = do 
  ps <- get  
  let hypotheticalPs = ps { psAlexInput = (psAlexInput ps) { aiStartCode = keyword } }
  let isFunction = evalParse (lexerM f) hypotheticalPs
  if isFunction 
  then return Nothing
  else toSC st
  where
    f t = case t of { Just (TFunction _) -> return True; _ -> return False }

toSC :: Int -> LexAction (Maybe Token)
toSC startCode = do
  ai <- getAlex
  if startCode == 0
  then putAlex $ ai { aiStartCode = startCode, aiWhiteSensitiveCharCount = 6 }
  else putAlex $ ai { aiStartCode = startCode }
  return Nothing

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

data Token = TLeftPar             SrcSpan
           | TRightPar            SrcSpan
           | TComma               SrcSpan
           | TDot                 SrcSpan
           | TColon               SrcSpan
           | TProgram             SrcSpan
           | TFunction            SrcSpan
           | TSubroutine          SrcSpan
           | TBlockData           SrcSpan
           | TEnd                 SrcSpan
           | TAssign              SrcSpan
           | TOpAssign            SrcSpan
           | TTo                  SrcSpan
           | TGoto                SrcSpan
           | TIf                  SrcSpan
           | TThen                SrcSpan
           | TElse                SrcSpan
           | TElsif               SrcSpan
           | TEndif               SrcSpan
           | TCall                SrcSpan
           | TReturn              SrcSpan
           | TSave                SrcSpan
           | TContinue            SrcSpan
           | TStop                SrcSpan
           | TPause               SrcSpan
           | TDo                  SrcSpan
           | TRead                SrcSpan
           | TWrite               SrcSpan
           | TRewind              SrcSpan
           | TBackspace           SrcSpan
           | TEndfile             SrcSpan
           | TInquire             SrcSpan
           | TOpen                SrcSpan
           | TClose               SrcSpan
           | TPrint               SrcSpan
           | TDimension           SrcSpan
           | TCommon              SrcSpan
           | TEquivalence         SrcSpan
           | TExternal            SrcSpan
           | TIntrinsic           SrcSpan
           | TType                SrcSpan String 
           | TEntry               SrcSpan
           | TImplicit            SrcSpan
           | TNone                SrcSpan
           | TParameter           SrcSpan
           | TData                SrcSpan
           | TFormat              SrcSpan
           | TFieldDescriptorDEFG SrcSpan (Maybe Integer) Char Integer Integer
           | TFieldDescriptorAIL  SrcSpan (Maybe Integer) Char Integer
           | TBlankDescriptor     SrcSpan Integer
           | TScaleFactor         SrcSpan Integer
           | TInt                 SrcSpan String
           | TExponent            SrcSpan String
           | TTrue                SrcSpan
           | TFalse               SrcSpan
           | TOpPlus              SrcSpan
           | TOpMinus             SrcSpan
           | TOpExp               SrcSpan
           | TStar                SrcSpan
           | TSlash               SrcSpan
           | TOpOr                SrcSpan
           | TOpAnd               SrcSpan
           | TOpNot               SrcSpan
           | TOpEquivalent        SrcSpan
           | TOpNotEquivalent     SrcSpan
           | TOpLT                SrcSpan
           | TOpLE                SrcSpan
           | TOpEQ                SrcSpan
           | TOpNE                SrcSpan
           | TOpGT                SrcSpan
           | TOpGE                SrcSpan
           | TId                  SrcSpan String
           | TComment             SrcSpan String
           | TString              SrcSpan String
           | THollerith           SrcSpan String
           | TLabel               SrcSpan String
           | TNewline             SrcSpan
           | TEOF                 SrcSpan
           deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance FirstParameter Token SrcSpan
instance FirstParameter Token SrcSpan => Spanned Token where
  getSpan a = getFirstParameter a
  setSpan e a = setFirstParameter e a

instance Tok Token where
  eofToken (TEOF _) = True
  eofToken _ = False

--------------------------------------------------------------------------------
-- AlexInput & related definitions
--------------------------------------------------------------------------------

data Lexeme = Lexeme 
  { lexemeMatch :: String
  , lexemeStart :: Maybe Position 
  , lexemeEnd   :: Maybe Position 
  } deriving (Show)

initLexeme :: Lexeme
initLexeme = Lexeme
  { lexemeMatch = ""
  , lexemeStart = Nothing
  , lexemeEnd   = Nothing }

data AlexInput = AlexInput 
  { aiSourceInput               :: String
  , aiPosition                  :: Position
  , aiBytes                     :: [Word8]
  , aiPreviousChar              :: Char
  , aiLexeme                    :: Lexeme
  , aiWhiteSensitiveCharCount   :: Int
  , aiStartCode                 :: Int
  , aiPreviousToken             :: Maybe Token
  , aiPreviousTokensInLine      :: [ Token ]
  } deriving (Show)

instance Loc AlexInput where
  getPos = aiPosition

instance LastToken AlexInput Token where
  getLastToken = aiPreviousToken

type LexAction a = Parse AlexInput Token a

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput 
  { aiSourceInput = ""
  , aiPosition = initPosition
  , aiBytes = []
  , aiPreviousChar = '\n'
  , aiLexeme = initLexeme
  , aiWhiteSensitiveCharCount = 6
  , aiStartCode = 0
  , aiPreviousToken = Nothing 
  , aiPreviousTokensInLine = [ ] }

updateLexeme :: Maybe Char -> Position -> AlexInput -> AlexInput
updateLexeme maybeChar p ai =
  let lexeme = aiLexeme ai
      match = lexemeMatch lexeme
      newMatch = 
        case maybeChar of
          Just c -> toLower c : match
          Nothing -> match
      start = lexemeStart lexeme
      newStart = if isNothing start then Just p else start
      newEnd = Just p in
    ai { aiLexeme = Lexeme newMatch newStart newEnd }

--------------------------------------------------------------------------------
-- Definitions needed for alexScanUser
--------------------------------------------------------------------------------

data Move = Continuation | Char | Newline

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai 
  -- The process of reading individual bytes of the character
  | _bytes /= [] = Just (head _bytes, ai { aiBytes = tail _bytes })
  -- When all characters are already read
  | posAbsoluteOffset _position == (toInteger . length . aiSourceInput) ai = Nothing
  -- Skip the continuation line altogether
  | isContinuation ai && _isWhiteInsensitive = skip Continuation ai 
  -- If we are not parsing a Hollerith skip whitespace
  | _curChar == ' ' && _isWhiteInsensitive = skip Char ai
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise = 
      let (_b:_bs) = (utf8Encode . toLower) _curChar in
        Just(_b, updateLexeme (Just _curChar) _position
          ai {
            aiPosition =
              case _curChar of
                '\n'  -> advance Newline _position
                _     -> advance Char _position,
            aiBytes = _bs,
            aiPreviousChar = _curChar,
            aiWhiteSensitiveCharCount = 
              if _isWhiteInsensitive
              then 0
              else aiWhiteSensitiveCharCount ai - 1
          })
  where
    _curChar = currentChar ai
    _bytes = aiBytes ai
    _position = aiPosition ai
    _isWhiteInsensitive = aiWhiteSensitiveCharCount ai == 0

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = aiPreviousChar ai

takeNChars :: Integer -> AlexInput -> String
takeNChars n ai = 
  take (fromIntegral n) . drop (fromIntegral _dropN) $ aiSourceInput ai
  where 
    _dropN = posAbsoluteOffset . aiPosition $ ai 

currentChar :: AlexInput -> Char 
currentChar ai = head (takeNChars 1 ai)

isContinuation :: AlexInput -> Bool
isContinuation ai = 
  take 6 _next7 == "\n     " && not (last _next7 `elem` [' ', '0'])
  where 
    _next7 = takeNChars 7 ai

skip :: Move -> AlexInput -> Maybe (Word8, AlexInput)
skip move ai = 
  let _newPosition = advance move $ aiPosition ai in
    alexGetByte $ updateLexeme Nothing _newPosition $ ai { aiPosition = _newPosition }

advance :: Move -> Position -> Position
advance move position =
  case move of 
    Char -> 
      position { posAbsoluteOffset = _absl + 1, posColumn = _col + 1 }
    Continuation -> 
      position { posAbsoluteOffset = _absl + 7, posColumn = 7, posLine = _line + 1 }
    Newline -> 
      position { posAbsoluteOffset = _absl + 1, posColumn = 1, posLine = _line + 1 }
  where
    _col = posColumn position
    _line = posLine position
    _absl = posAbsoluteOffset position

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . _go . ord
  where
    _go oc
      | oc <= 0x7f   = [oc]
      | oc <= 0x7ff  = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]
      | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                       , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]
      | otherwise    = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                       , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                       , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                       , 0x80 + oc Data.Bits..&. 0x3f
                       ]

--------------------------------------------------------------------------------
-- Lexer definition
--------------------------------------------------------------------------------

lexer :: (Token -> LexAction a) -> LexAction a
lexer cont = do
   mToken <- lexer'
   case mToken of
     Just token -> cont token
     Nothing -> fail "Unrecognised token. "

lexerM :: ((Maybe Token) -> LexAction a) -> LexAction a
lexerM cont = lexer' >>= \mToken -> cont mToken

lexer' :: LexAction (Maybe Token)
lexer' = do
  resetLexeme
  alexInput <- getAlex
  let startCode = aiStartCode alexInput
  version <- getVersion
  case alexScanUser version alexInput startCode of
    AlexEOF -> return $ Just $ TEOF $ SrcSpan (getPos alexInput) (getPos alexInput)
    AlexError _ -> return Nothing
    AlexSkip newAlex _ -> putAlex newAlex >> lexer'
    AlexToken newAlex startCode action -> do
      putAlex newAlex
      maybeToken <- action
      case maybeToken of
        Just token -> do
          updatePreviousToken maybeToken 
          addToPreviousTokensInLine token
          return maybeToken
        Nothing -> lexer'

alexScanUser :: FortranVersion -> AlexInput -> Int -> AlexReturn (LexAction (Maybe Token))

--------------------------------------------------------------------------------
-- Functions to help testing & output
--------------------------------------------------------------------------------

initParseState :: String -> FortranVersion -> String -> ParseState AlexInput
initParseState srcInput fortranVersion filename = 
  _vanillaParseState { psAlexInput = vanillaAlexInput { aiSourceInput = srcInput } }
  where
    _vanillaParseState = ParseState 
      { psAlexInput = undefined
      , psVersion = fortranVersion
      , psFilename = filename 
      , psParanthesesCount = 0 }
    
collectFixedTokens :: FortranVersion -> String -> Maybe [Token]
collectFixedTokens version srcInput = 
    collectTokens lexer' $ initParseState srcInput version "<unknown>"

}
