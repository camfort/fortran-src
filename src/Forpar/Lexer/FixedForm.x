{
module Forpar.Lexer.FixedForm where

import Forpar.ParserMonad

import Data.Word (Word8)
import Data.Char (toLower)
import Data.List (isPrefixOf, any)
import qualified Data.Bits
import Debug.Trace

import Control.Monad.Trans.Cont
import Control.Exception
import GHC.Exts
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
  <st> @integerConst                    { getMatch >>= \s -> return $ Just $ TNum s }
  <st> "e"                              { return $ Just TRealExp }
  <st> "d"                              { return $ Just TDoubleExp }

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

  -- Format items
  <st> @width "x"                       { getMatch >>= \s -> return $ Just $ TFormatItem s }
  <st> @repeat [ail] @width             { getMatch >>= \s -> return $ Just $ TFormatItem s }
  <st> @repeat [defg] @width "." @integerConst  { getMatch >>= \s -> return $ Just $ TFormatItem s }

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
  let match = reverse . rMatch $ ai in
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
    _endsWithLine = (rColumn . rPosition) aiNew /= 1

withinLabelColsP :: user -> AlexInput -> Int -> AlexInput -> Bool
withinLabelColsP _ aiOld _ aiNew = getCol aiOld >= 1 && getCol aiNew <= 6
  where
    getCol = rColumn . rPosition

atColP :: Integer -> AlexInput -> Bool
atColP n ai = (rColumn . rPosition) ai == n

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

type FixedLex a b = Lex AlexInput a b

getMatch :: FixedLex a String
getMatch = do
  ai <- getAlexL
  return $ (reverse . rMatch) ai

putMatch :: String -> FixedLex a ()
putMatch newMatch = do
  ai <- getAlexL
  putAlexL $ ai { rMatch = newMatch }
  return ()

-- With the existing alexGetByte implementation comments are matched without
-- whitespace characters. However, we have access to final column number,
-- we know the comment would start at column, and we have access to the absolute
-- offset so instead of using match, lexComment takes a slice from the original
-- source input
lexComment :: FixedLex a (Maybe Token)
lexComment = do
  ai <- getAlexL
  let _col = (rColumn . rPosition) ai
  let _absl = (rAbsoluteOffset . rPosition) ai
  let _src =  rSourceInput ai
  let _nToTake = fromIntegral (_col - 2)
  let _nToDrop = fromIntegral (_absl - _col + 2)
  return $ Just $ TComment $ take _nToTake . drop _nToDrop $ _src

lexHollerith :: FixedLex a (Maybe Token)
lexHollerith = do
  match' <- getMatch
  let len = read $ init match' -- Get n of "nH" from string
  putMatch ""
  ai <- getAlexL 
  putAlexL $ ai { rWhiteSensitiveCharCount = len } 
  lexed <- lexN len
  return $ do
    hollerith <- lexed
    return $ THollerith hollerith

lexN :: Int -> FixedLex a (Maybe String)
lexN n = do
  alex <- getAlexL
  match' <- getMatch
  let len = length match'
  if n == len
  then return $ Just match'
  else 
    case alexGetByte alex of
      Just (_, newAlex) -> do
        putAlexL newAlex
        lexN n
      Nothing -> return Nothing

toStartCode :: Int -> FixedLex a (Maybe Token)
toStartCode startCode = do
  ai <- getAlexL
  if startCode == 0
  then putAlexL $ ai { rStartCode = startCode, rWhiteSensitiveCharCount = 6 }
  else putAlexL $ ai { rStartCode = startCode }
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
           | TFormatItem String
           | TNum String
           | TRealExp
           | TDoubleExp
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

data Position = Position 
  { rAbsoluteOffset   :: Integer
  , rColumn           :: Integer
  , rLine             :: Integer
  } deriving (Show)

initPosition :: Position
initPosition = Position 
  { rAbsoluteOffset = 0
  , rColumn = 1
  , rLine = 1 
  }

data AlexInput = AlexInput 
  { rSourceInput                :: String
  , rPosition                   :: Position
  , rBytes                      :: [Word8]
  , rPreviousChar               :: Char
  , rMatch                      :: String
  , rWhiteSensitiveCharCount    :: Int
  , rStartCode                  :: Int
  } deriving (Show)

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput 
  { rSourceInput = ""
  , rPosition = initPosition
  , rBytes = []
  , rPreviousChar = '\n'
  , rMatch = ""
  , rWhiteSensitiveCharCount = 6
  , rStartCode = 0 }

--------------------------------------------------------------------------------
-- Definitions needed for alexScanUser
--------------------------------------------------------------------------------

data Move = Continuation | Char | Newline

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai 
  -- The process of reading individual bytes of the character
  | _bytes /= [] = Just (head _bytes, ai { rBytes = tail _bytes })
  -- When all characters are already read
  | rAbsoluteOffset _position == (toInteger . length . rSourceInput) ai = Nothing
  -- Skip the continuation line altogether
  | isContinuation ai && _isWhiteInsensitive = skip Continuation ai 
  -- If we are not parsing a Hollerith skip whitespace
  | _curChar == ' ' && _isWhiteInsensitive = skip Char ai
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise = 
      let (_b:_bs) = (utf8Encode . toLower) _curChar in
        Just(_b,
          ai {
            rPosition =
              case _curChar of
                '\n'  -> advance Newline _position
                _     -> advance Char _position,
            rBytes = _bs,
            rPreviousChar = _curChar,
            rMatch = (toLower _curChar):(rMatch ai),
            rWhiteSensitiveCharCount = 
              if _isWhiteInsensitive
              then 0
              else rWhiteSensitiveCharCount ai - 1
          })
  where
    _curChar = currentChar ai
    _bytes = rBytes ai
    _position = rPosition ai
    _isWhiteInsensitive = rWhiteSensitiveCharCount ai == 0

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = rPreviousChar ai

takeNChars :: Integer -> AlexInput -> String
takeNChars n ai = 
  take (fromIntegral n) . drop (fromIntegral _dropN) $ rSourceInput ai
  where 
    _dropN = rAbsoluteOffset . rPosition $ ai 

currentChar :: AlexInput -> Char 
currentChar ai = head (takeNChars 1 ai)

isContinuation :: AlexInput -> Bool
isContinuation ai = 
  take 6 _next7 == "\n     " && not (last _next7 `elem` [' ', '0'])
  where 
    _next7 = takeNChars 7 ai

skip :: Move -> AlexInput -> Maybe (Word8, AlexInput)
skip move ai = 
  let _newPosition = advance move $ rPosition ai in
    alexGetByte $ ai { rPosition = _newPosition }

advance :: Move -> Position -> Position
advance move position =
  case move of 
    Char -> 
      position { rAbsoluteOffset = _absl + 1, rColumn = _col + 1 }
    Continuation -> 
      position { rAbsoluteOffset = _absl + 7, rColumn = 7, rLine = _line + 1 }
    Newline -> 
      position { rAbsoluteOffset = _absl + 1, rColumn = 1, rLine = _line + 1 }
  where
    _col = rColumn position
    _line = rLine position
    _absl = rAbsoluteOffset position

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

lexer :: (Token -> Parse AlexInput (Maybe Token)) -> Parse AlexInput (Maybe Token)
lexer = runContT $ do
   mToken <- lexer'
   case mToken of
     Just tok -> return tok -- :: Lex AlexInput Token Token
     Nothing -> fail "Unrecognised Token"
            
lexer' :: FixedLex (Maybe Token) (Maybe Token)
lexer' = do
  putMatch ""
  alexInput <- getAlexL
  let startCode = rStartCode alexInput
  case alexScanUser undefined alexInput startCode of
    AlexEOF -> return $ Just TEOF
    AlexError _ -> return Nothing
    AlexSkip newAlex _ -> putAlexL newAlex >> lexer'
    AlexToken newAlex _ action -> do
      putAlexL newAlex
      maybeTok <- action
      case maybeTok of
        Just _ -> return maybeTok
        Nothing -> lexer'

--------------------------------------------------------------------------------
-- Functions to help testing & output
--------------------------------------------------------------------------------

initParseState :: String -> FortranVersion -> String -> ParseState AlexInput
initParseState srcInput fortranVersion filename = 
  _vanillaParseState { rAlexInput = vanillaAlexInput { rSourceInput = srcInput } }
  where
    _vanillaParseState = ParseState 
      { rAlexInput = undefined
      , rVersion = fortranVersion
      , rFilename = filename 
      }
    
collectFixedFormTokens :: String -> Maybe [Token]
collectFixedFormTokens srcInput = 
    collectTokens TEOF lexer' $ initParseState srcInput Fortran66 "<unknown>"

}
