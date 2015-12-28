{
module Forpar.Lexer.FixedForm where

import Data.Word (Word8)
import Data.Char (toLower, isDigit)
import Data.List (isPrefixOf, any)
import qualified Data.Bits
import Control.Monad.Trans.Cont
import Control.Exception
import GHC.Exts

import Forpar.ParserMonad
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

-- Real numbers
@basicReal = (@integerConst '.' @integerConst | @integerConst '.' | '.' @integerConst)
@exponent = [ed] [\+\-]? @integerConst
@realConst = (@basicReal | @integerConst) @exponent?

-- For format items
@repeat = @posIntegerConst?
@width = @posIntegerConst

tokens :-

  <0> "c".* / { commentP }              { lexComment }
  <0> @label / { withinLabelColsP }     { getMatch >>= \s -> return $ Just $ TLabel s }
  <0> . / { \_ ai _ _ -> atColP 6 ai }  { toStartCode st }
  <0> " "                               ;
  <0> \n                                { toStartCode 0 }
  <0> \r                                ;

  <st> \n                               { toStartCode 0 }
  <st> \r                               ;

  <st> "("                              { return $ Just TLeftPar }
  <st> ")"                              { return $ Just TRightPar }
  <st> ","                              { return $ Just TComma }
  <st> "."                              { return $ Just TDot }

  -- Tokens related to procedures and subprograms
  <st> "function"                       { return $ Just TFunction }
  <st> "subroutine"                     { return $ Just TSubroutine }
  <st> "blockdata"                      { return $ Just TBlockData }
  <st> "end"                            { return $ Just TEnd }

  -- Tokens related to assignment statements
  <st> "assign"                         { return $ Just TAssign }
  <st> "="                              { return $ Just TOpAssign }
  <st> "to"                             { return $ Just TTo }

  -- Tokens related to control statements
  <st> "goto"                           { return $ Just TGoto }
  <st> "if"                             { return $ Just TIf }
  <st> "call"                           { return $ Just TCall }
  <st> "return"                         { return $ Just TReturn }
  <st> "continue"                       { return $ Just TContinue }
  <st> "stop"                           { return $ Just TStop }
  <st> "pause"                          { return $ Just TPause }
  <st> "do"                             { return $ Just TDo }

  -- Tokens related to I/O statements
  <st> "read"                           { return $ Just TRead }
  <st> "write"                          { return $ Just TWrite }
  <st> "rewind"                         { return $ Just TRewind }
  <st> "backspace"                      { return $ Just TBackspace }
  <st> "endfile"                        { return $ Just TEndfile }

  -- Tokens related to non-executable statements

  -- Tokens related to speification statements
  <st> "dimension"                      { return $ Just TDimension }
  <st> "common"                         { return $ Just TCommon }
  <st> "equivalence"                    { return $ Just TEquivalence }
  <st> "external"                       { return $ Just TExternal }
  <st> @datatype                        { getMatch >>= \s -> return $ Just $ TType s }

  -- Tokens related to data initalization statement
  <st> "data"                           { return $ Just TData }

  -- Tokens related to format statement
  <st> "format"                         { return $ Just TFormat }

  -- Tokens needed to parse integers, reals, double precision and complex 
  -- constants
  <st> @integerConst                    { getMatch >>= \s -> return $ Just $ TInt s }
  <st> @realConst                       { getMatch >>= \s -> return $ Just $ TReal s }

  -- Logicals
  <st> ".true."                         { return $ Just TTrue }
  <st> ".false."                        { return $ Just TFalse }

  -- Arithmetic operators
  <st> "+"                              { return $ Just TOpPlus }
  <st> "-"                              { return $ Just TOpMinus }
  <st> "**"                             { return $ Just TOpExp }
  <st> "*"                              { return $ Just TStar }
  <st> "/"                              { return $ Just TSlash }

  -- Logical operators
  <st> ".or."                           { return $ Just TOpOr }
  <st> ".and."                          { return $ Just TOpAnd }
  <st> ".not."                          { return $ Just TOpNot }

  -- Relational operators
  <st> ".lt."                           { return $ Just TOpLT }
  <st> ".le."                           { return $ Just TOpLE }
  <st> ".eq."                           { return $ Just TOpEQ }
  <st> ".ne."                           { return $ Just TOpNE }
  <st> ".gt."                           { return $ Just TOpGT }
  <st> ".ge."                           { return $ Just TOpGE }

  -- Field descriptors
  <st> @repeat [defg] @width \. @integerConst  { lexFieldDescriptorDEFG }
  <st> @repeat [ail] @width                    { lexFieldDescriptorAIL }
  <st> @width x                              { lexBlankDescriptor }
  <st> "-"? @posIntegerConst p               { lexScaleFactor }

  -- ID
  <st> @id / { isNotPrefixOfKeywordP }  { getMatch >>= \s -> return $ Just $ TId s }

  -- Strings
  <st> @posIntegerConst "h"             { lexHollerith }

{

--------------------------------------------------------------------------------
-- Predicated lexer helpers
--------------------------------------------------------------------------------

-- No identifier can start with a keyword according to the specification.
-- Since identifiers are at most 6 characters long and alex takes the longest
-- match, we only need to check if the matched pattern starts with keywords with
-- fewer than 7 characters.
isNotPrefixOfKeywordP :: user -> AlexInput -> Int -> AlexInput -> Bool
isNotPrefixOfKeywordP _ _ _ ai = 
  let match = reverse . aiMatch $ ai in
    not $ any (\_keyword -> _keyword `isPrefixOf` match) _shortKeywords
  where
    _shortKeywords = [
      "end", "assign", "goto", "if", "call",
      "return", "stop", "pause", "do", "read",
      "write", "rewind", "common", "data", "format",
      "real" ]

commentP :: user -> AlexInput -> Int -> AlexInput -> Bool
commentP _ aiOld _ aiNew = atColP 1 aiOld && _endsWithLine
  where
    _endsWithLine = (posColumn . aiPosition) aiNew /= 1

withinLabelColsP :: user -> AlexInput -> Int -> AlexInput -> Bool
withinLabelColsP _ aiOld _ aiNew = getCol aiOld >= 1 && getCol aiNew <= 6
  where
    getCol = posColumn . aiPosition

atColP :: Integer -> AlexInput -> Bool
atColP n ai = (posColumn . aiPosition) ai == n

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

type FixedLex a b = Lex AlexInput a b

getMatch :: Parse AlexInput String
getMatch = do
  ai <- getAlexP
  return $ (reverse . aiMatch) ai

putMatch :: String -> Parse AlexInput ()
putMatch newMatch = do
  ai <- getAlexP
  putAlexP $ ai { aiMatch = newMatch }
  return ()

-- With the existing alexGetByte implementation comments are matched without
-- whitespace characters. However, we have access to final column number,
-- we know the comment would start at column, and we have access to the absolute
-- offset so instead of using match, lexComment takes a slice from the original
-- source input
lexComment :: Parse AlexInput (Maybe Token)
lexComment = do
  ai <- getAlexP
  let _col = (posColumn . aiPosition) ai
  let _absl = (posAbsoluteOffset . aiPosition) ai
  let _src =  aiSourceInput ai
  let _nToTake = fromIntegral (_col - 2)
  let _nToDrop = fromIntegral (_absl - _col + 2)
  return $ Just $ TComment $ take _nToTake . drop _nToDrop $ _src

lexHollerith :: Parse AlexInput (Maybe Token)
lexHollerith = do
  match' <- getMatch
  let len = read $ init match' -- Get n of "nH" from string
  putMatch ""
  ai <- getAlexP
  putAlexP $ ai { aiWhiteSensitiveCharCount = len } 
  lexed <- lexN len
  return $ do
    hollerith <- lexed
    return $ THollerith hollerith

lexN :: Int -> Parse AlexInput (Maybe String)
lexN n = do
  alex <- getAlexP
  match' <- getMatch
  let len = length match'
  if n == len
  then return $ Just match'
  else 
    case alexGetByte alex of
      Just (_, newAlex) -> do
        putAlexP newAlex
        lexN n
      Nothing -> return Nothing

-- Lexing various field descriptors

lexFieldDescriptorDEFG :: Parse AlexInput (Maybe Token)
lexFieldDescriptorDEFG = do
  match <- getMatch
  let (repeat, descriptor, width, rest) = takeRepeatDescriptorWidth match
  let fractionWidth = (read $ fst $ takeNumber $ tail rest) :: Integer
  return $ Just $ TFieldDescriptorDEFG repeat descriptor width fractionWidth

lexFieldDescriptorAIL :: Parse AlexInput (Maybe Token)
lexFieldDescriptorAIL = do
  match <- getMatch
  let (repeat, descriptor, width, rest) = takeRepeatDescriptorWidth match
  return $ Just $ TFieldDescriptorAIL repeat descriptor width

lexBlankDescriptor :: Parse AlexInput (Maybe Token)
lexBlankDescriptor = do
  match <- getMatch
  let (width, _) = takeNumber match
  return $ Just $ TBlankDescriptor $ (read width :: Integer)

lexScaleFactor :: Parse AlexInput (Maybe Token)
lexScaleFactor = do
  match <- getMatch
  let (sign, rest) = if head match == '-' then (-1, tail match) else (1, match)
  let (width, _) = takeNumber rest
  return $ Just $ TScaleFactor $ (read width) * sign

takeRepeatDescriptorWidth :: String -> (Integer, Char, Integer, String)
takeRepeatDescriptorWidth str = 
  let (repeatStr, rest) = takeNumber str
      repeat = if repeatStr == [] then 1 else read repeatStr :: Integer
      descriptor = head rest
      (widthStr, rest') = takeNumber $ tail rest
      width = read widthStr :: Integer in
    (repeat, descriptor, width, rest')

takeNumber :: String -> (String, String)
takeNumber str = span isDigit str

toStartCode :: Int -> Parse AlexInput (Maybe Token)
toStartCode startCode = do
  ai <- getAlexP
  if startCode == 0
  then putAlexP $ ai { aiStartCode = startCode, aiWhiteSensitiveCharCount = 6 }
  else putAlexP $ ai { aiStartCode = startCode }
  return Nothing

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

data Token = TLeftPar
           | TRightPar
           | TComma
           | TDot
           | TFunction
           | TSubroutine
           | TBlockData
           | TEnd
           | TAssign
           | TOpAssign
           | TTo
           | TGoto
           | TIf
           | TCall
           | TReturn
           | TContinue
           | TStop
           | TPause
           | TDo
           | TRead
           | TWrite
           | TRewind
           | TBackspace
           | TEndfile
           | TDimension
           | TCommon
           | TEquivalence
           | TExternal
           | TType String
           | TData
           | TFormat
           | TFieldDescriptorDEFG Integer Char Integer Integer
           | TFieldDescriptorAIL  Integer Char Integer
           | TBlankDescriptor     Integer
           | TScaleFactor         Integer
           | TInt String
           | TReal String
           | TTrue
           | TFalse
           | TOpPlus
           | TOpMinus
           | TOpExp
           | TStar
           | TSlash
           | TOpOr
           | TOpAnd
           | TOpNot
           | TOpLT
           | TOpLE
           | TOpEQ
           | TOpNE
           | TOpGT
           | TOpGE
           | TId String
           | TComment String
           | THollerith String
           | TLabel String
           | TEOF
           deriving (Show, Eq)

--------------------------------------------------------------------------------
-- AlexInput & related definitions
--------------------------------------------------------------------------------

data AlexInput = AlexInput 
  { aiSourceInput                 :: String
  , aiPosition                    :: Position
  , aiBytes                       :: [Word8]
  , aiPreviousChar                :: Char
  , aiMatch                       :: String
  , aiWhiteSensitiveCharCount     :: Int
  , aiStartCode                   :: Int
  } deriving (Show)

instance Loc AlexInput where
  getPos = aiPosition

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput 
  { aiSourceInput = ""
  , aiPosition = initPosition
  , aiBytes = []
  , aiPreviousChar = '\n'
  , aiMatch = ""
  , aiWhiteSensitiveCharCount = 6
  , aiStartCode = 0 }

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
        Just(_b,
          ai {
            aiPosition =
              case _curChar of
                '\n'  -> advance Newline _position
                _     -> advance Char _position,
            aiBytes = _bs,
            aiPreviousChar = _curChar,
            aiMatch = (toLower _curChar):(aiMatch ai),
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
    alexGetByte $ ai { aiPosition = _newPosition }

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

lexer :: (Token -> Parse AlexInput a) -> Parse AlexInput a
lexer cont = do
   mToken <- lexer'
   case mToken of
     Just token -> cont token
     Nothing -> fail "Unrecognised Token"
            
lexer' :: Parse AlexInput (Maybe Token)
lexer' = do
  putMatch ""
  alexInput <- getAlexP
  let startCode = aiStartCode alexInput
  case alexScanUser undefined alexInput startCode of
    AlexEOF -> return $ Just TEOF
    AlexError _ -> return Nothing
    AlexSkip newAlex _ -> putAlexP newAlex >> lexer'
    AlexToken newAlex _ action -> do
      putAlexP newAlex
      maybeTok <- action
      case maybeTok of
        Just _ -> return maybeTok
        Nothing -> lexer'

alexScanUser :: () -> AlexInput -> Int -> AlexReturn (Parse AlexInput (Maybe Token))

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
      }
    
collectFixedFormTokens :: String -> Maybe [Token]
collectFixedFormTokens srcInput = 
    collectTokens TEOF lexer' $ initParseState srcInput Fortran66 "<unknown>"

}
