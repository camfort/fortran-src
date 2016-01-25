{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Forpar.Lexer.FixedForm where

import Data.Word (Word8)
import Data.Char (toLower, isDigit)
import Data.List (isPrefixOf, isSuffixOf, any)
import Data.Maybe (fromJust, isNothing)
import Data.Data
import Data.Typeable
import qualified Data.Bits

import Control.Exception
import Control.Monad.State

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

  <0> "c" / { commentP }                { lexComment Nothing }
  <0> @label / { withinLabelColsP }     { getMatch >>= \m -> getLexemeSpan >>= \s -> return $ Just $ TLabel s m }
  <0> . / { \_ ai _ _ -> atColP 6 ai }  { toStartCode keyword }
  <0> " "                               ;

  <0,st,keyword,iif> \n                                { toStartCode 0 >> getLexemeSpan >>= \s -> return $ Just $ TNewline s }
  <0,st,keyword,iif> \r                                ;

  <st> "("                              { getLexemeSpan >>= \s -> return $ Just $ TLeftPar s }
  <iif> "("                              { incPar >> getLexemeSpan >>= \s -> return $ Just $ TLeftPar s }
  <st> ")"                              { getLexemeSpan >>= \s -> return $ Just $ TRightPar s }
  <iif> ")"                              { maybeToKeyword >> getLexemeSpan >>= \s -> return $ Just $ TRightPar s }
  <st,iif,keyword> ","                              { getLexemeSpan >>= \s -> return $ Just $ TComma s }
  <st,iif> "."                              { getLexemeSpan >>= \s -> return $ Just $ TDot s }
  <st,iif> ":" / { fortran77P }             { getLexemeSpan >>= \s -> return $ Just $ TColon s }

  <keyword> @id / { equalFollowsP }     { toStartCode st >> getLexemeSpan >>= \s -> getMatch >>= \m -> return $ Just $ TId s m }

  -- Tokens related to procedures and subprograms
  <keyword> "program"                   { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TProgram s }
  <keyword> "function"                  { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TFunction s }
  <keyword> "subroutine"                { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TSubroutine s }
  <keyword> "blockdata"                 { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TBlockData s }
  <keyword> "end"                       { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TEnd s }

  -- Tokens related to assignment statements
  <keyword,iif> "assign"                    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TAssign s }
  <st,iif> "="                              { getLexemeSpan >>= \s -> return $ Just $ TOpAssign s }
  <st,iif> "to"                             { getLexemeSpan >>= \s -> return $ Just $ TTo s }

  -- Tokens related to control statements
  <keyword,iif> "goto"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TGoto s }
  <keyword> "if"                        { toStartCode iif >> getLexemeSpan >>= \s -> return $ Just $ TIf s }
  <keyword> "else"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TElse s }
  <keyword,iif> "call"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TCall s }
  <keyword,iif> "return"                    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TReturn s }
  <keyword,iif> "save" / { fortran77P }     { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TSave s }
  <keyword,iif> "continue"                  { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TContinue s }
  <keyword,iif> "stop"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TStop s }
  <keyword,iif> "pause"                     { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TPause s }
  <keyword> "do"                        { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TDo s }

  -- Tokens related to I/O statements
  <keyword,iif> "read"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TRead s }
  <keyword,iif> "write"                     { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TWrite s }
  <keyword,iif> "rewind"                    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TRewind s }
  <keyword,iif> "backspace"                 { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TBackspace s }
  <keyword,iif> "endfile"                   { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TEndfile s }
  <keyword,iif> "inquire" / { fortran77P }  { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TInquire s }
  <keyword,iif> "open" / { fortran77P }     { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TOpen s }
  <keyword,iif> "close" / { fortran77P }    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TClose s }
  <keyword,iif> "print" / { fortran77P }    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TPrint s }

  -- Tokens related to non-executable statements

  -- Tokens related to speification statements
  <keyword> "dimension"                 { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TDimension s }
  <keyword> "common"                    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TCommon s }
  <keyword> "equivalence"               { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TEquivalence s }
  <keyword> "external"                  { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TExternal s }
  <keyword> "intrinsic" / { fortran77P }{ toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TIntrinsic s }
  <keyword,st> @datatype                   { toStartCode st >> getLexemeSpan >>= \s -> getMatch >>= \m -> return $ Just $ TType s m }
  <keyword,st> "character" / { fortran77P }{ toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TType s "character" }
  <keyword> "implicit" / { fortran77P } { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TImplicit s }
  <st> "none" / { fortran77P }          { getLexemeSpan >>= \s -> return $ Just $ TNone s }
  <keyword> "parameter" / { fortran77P }{ toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TParameter s }
  <keyword> "entry" / { fortran77P }    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TEntry s }

  -- Tokens related to data initalization statement
  <keyword,iif> "data"                      { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TData s }

  -- Tokens related to format statement
  <keyword> "format"                    { toStartCode st >> getLexemeSpan >>= \s -> return $ Just $ TFormat s }

  -- Tokens needed to parse integers, reals, double precision and complex 
  -- constants
  <st,iif> @exponent / { exponentP }        { getLexemeSpan >>= \s -> getMatch >>= \m -> return $ Just $ TExponent s m }
  <st,iif,keyword> @integerConst            { getLexemeSpan >>= \s -> getMatch >>= \m -> return $ Just $ TInt s m }

  -- String
  <st,iif> \' / { fortran77P }              { strAutomaton 0 }

  -- Logicals
  <st,iif> ".true."                         { getLexemeSpan >>= \s -> return $ Just $ TTrue s }
  <st,iif> ".false."                        { getLexemeSpan >>= \s -> return $ Just $ TFalse s }

  -- Arithmetic operators
  <st,iif> "+"                              { getLexemeSpan >>= \s -> return $ Just $ TOpPlus s }
  <st,iif> "-"                              { getLexemeSpan >>= \s -> return $ Just $ TOpMinus s }
  <st,iif> "**"                             { getLexemeSpan >>= \s -> return $ Just $ TOpExp s }
  <st,iif> "*"                              { getLexemeSpan >>= \s -> return $ Just $ TStar s }
  <st,iif> "/"                              { getLexemeSpan >>= \s -> return $ Just $ TSlash s }

  -- Logical operators
  <st,iif> ".or."                           { getLexemeSpan >>= \s -> return $ Just $ TOpOr s }
  <st,iif> ".and."                          { getLexemeSpan >>= \s -> return $ Just $ TOpAnd s }
  <st,iif> ".not."                          { getLexemeSpan >>= \s -> return $ Just $ TOpNot s }
  <st,iif> ".eqv." / { fortran77P }         { getLexemeSpan >>= \s -> return $ Just $ TOpEquivalent s }
  <st,iif> ".neqv." / { fortran77P }        { getLexemeSpan >>= \s -> return $ Just $ TOpNotEquivalent s }

  -- Relational operators
  <st,iif> ".lt."                           { getLexemeSpan >>= \s -> return $ Just $ TOpLT s }
  <st,iif> ".le."                           { getLexemeSpan >>= \s -> return $ Just $ TOpLE s }
  <st,iif> ".eq."                           { getLexemeSpan >>= \s -> return $ Just $ TOpEQ s }
  <st,iif> ".ne."                           { getLexemeSpan >>= \s -> return $ Just $ TOpNE s }
  <st,iif> ".gt."                           { getLexemeSpan >>= \s -> return $ Just $ TOpGT s }
  <st,iif> ".ge."                           { getLexemeSpan >>= \s -> return $ Just $ TOpGE s }

  -- Field descriptors
  <st> @repeat [defg] @width \. @integerConst { lexFieldDescriptorDEFG }
  <st> @repeat [ail] @width                   { lexFieldDescriptorAIL }
  <st> @width x                               { lexBlankDescriptor }
  <st> "-"? @posIntegerConst p                { lexScaleFactor }

  -- ID
  <st,iif> @id                              { getLexemeSpan >>= \s -> getMatch >>= \m -> return $ Just $ TId s m }

  -- Strings
  <st> @posIntegerConst "h" / { fortran66P }  { lexHollerith }

{

--------------------------------------------------------------------------------
-- Predicated lexer helpers
--------------------------------------------------------------------------------

equalFollowsP :: FortranVersion -> AlexInput -> Int -> AlexInput -> Bool
equalFollowsP fv _ _ ai = isNotSuffixOf "od" && isNotSuffixOf "fi" && evalParse (lexer f) ps
  where
    isNotSuffixOf suffix = not $ isSuffixOf suffix match
    match = lexemeMatch . aiLexeme $ ai
    ps = ParseState
      { psAlexInput = ai { aiStartCode = st}
      , psVersion = fv
      , psFilename = "<unknown>"
      , psParanthesesCount = 0 }
    f t = 
      case t of 
        TNewline _ -> return False
        TEOF _ -> return False
        TOpAssign _ -> return True
        _ -> lexer f

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

incPar :: LexAction ()
incPar = do
  ps <- get
  put $ ps { psParanthesesCount = psParanthesesCount ps + 1}

decPar :: LexAction ()
decPar = do
  ps <- get
  put $ ps { psParanthesesCount = psParanthesesCount ps - 1}

resetWhiteSensitiveCharCount :: LexAction ()
resetWhiteSensitiveCharCount = do
  ai <- getAlex
  putAlex $ ai { aiWhiteSensitiveCharCount = 0 }

instance Spanned Lexeme where
  getSpan lexeme = 
    let ms = lexemeStart lexeme 
        me = lexemeEnd lexeme in
      SrcSpan (fromJust ms) (fromJust me)
  setSpan _ = error "Should not be called"

updatePreviousToken :: Maybe Token -> LexAction ()
updatePreviousToken maybeToken = do
  ai <- getAlex
  putAlex $ ai { aiPreviousToken = maybeToken }

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
  then toStartCode keyword
  else return Nothing

toStartCode :: Int -> LexAction (Maybe Token)
toStartCode startCode = do
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
           | TElse                SrcSpan
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
           deriving (Show, Eq, Data, Typeable, Generic)

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
  , aiPreviousToken = Nothing }

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
    AlexToken newAlex _ action -> do
      putAlex newAlex
      maybeToken <- action
      case maybeToken of
        Just _ -> updatePreviousToken maybeToken >> return maybeToken
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
      , psParanthesesCount = 0
      }
    
collectFixedTokens :: FortranVersion -> String -> Maybe [Token]
collectFixedTokens version srcInput = 
    collectTokens lexer' $ initParseState srcInput version "<unknown>"

}
