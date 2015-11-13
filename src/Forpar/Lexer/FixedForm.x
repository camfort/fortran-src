{
module Forpar.Lexer.FixedForm where

import Forpar.ParserMonad

import Data.Word (Word8)
import Data.Char (toLower)
import qualified Data.Bits

import Control.Monad.Trans.Cont
}

$digit = [0-9]
$letter = [a-z]
$alphanumeric = [$letter $letter]
$special = [\ \=\+\-\*\/\(\)\,\.\$]

@id = $letter $alphanumeric{0,5}
@label = [1-9] $digit{0,4} -- Questionable necessity

@datatype = "integer" | "real" | "doubleprecision" | "complex" | "logical"

-- Numbers
@integerConst = $digit+ -- Integer constant

tokens :-
 "("                          { return TLeftPar }
  ")"                         { return TRightPar }
  ","                         { return TComma }
  "."                         { return TDot }

  -- Tokens related to procedures and subprograms
  "function"                  { return TFunction }
  "subroutine"                { return TSubroutine }
  "blockdata"                 { return TBlockData }
  "end"                       { return TEnd }

  -- Tokens related to assignment statements
  "assign"                    { return TAssign }
  "="                         { return TOpAssign }
  "to"                        { return TTo }

  -- Tokens related to control statements
  "goto"                      { return TGoto }
  "if"                        { return TIf }
  "call"                      { return TCall }
  "return"                    { return TReturn }
  "continue"                  { return TContinue }
  "stop"                      { return TStop }
  "pause"                     { return TPause }
  "do"                        { return TDo }

  -- Tokens related to I/O statements
  "read"                      { return TRead }
  "write"                     { return TWrite }
  "rewind"                    { return TRewind }
  "backspace"                 { return TBackspace }
  "endfile"                   { return TEndfile }

  -- Tokens related to non-executable statements

  -- Tokens related to speification statements
  "dimension"                 { return TDimension }
  "common"                    { return TCommon }
  "equivalence"               { return TEquivalence }
  "external"                  { return TExternal }
  @datatype                   { getMatch >>= \s -> return $ TType s }

  -- Tokens related to data initalization statement
  "data"                      { return TData }

  -- Tokens related to format statement
  "format"                    { return TFormat }

  -- Tokens needed to parse integers, reals, double precision and complex 
  -- constants
  @integerConst               { getMatch >>= \s -> return $ TNum s }
  "e"                         { return TRealExp }
  "d"                         { return TDoubleExp }

  -- Logicals
  ".true."                    { return TTrue }
  ".false."                   { return TFalse }

  -- Arithmetic operators
  "+"                         { return TOpPlus }
  "-"                         { return TOpMinus }
  "**"                        { return TOpExp }
  "*"                         { return TOpMul }
  "/"                         { return TOpDiv }

  -- Logical operators
  ".or."                      { return TOpOr }
  ".and."                     { return TOpAnd }
  ".not."                     { return TOpNot }

  -- Relational operators
  ".lt."                      { return TOpLT }
  ".le."                      { return TOpLE }
  ".eq."                      { return TOpEQ }
  ".ne."                      { return TOpNE }
  ".gt."                      { return TOpGT }
  ".ge."                      { return TOpGE }

  @id                         { getMatch >>= \s -> return $ TId s }
  "c".*"\n" / { atFirstCol }  { lexComment }

  -- Strings
  @integerConst "h"           { lexHollerith }

{

--------------------------------------------------------------------------------
-- Lexer helpers
--------------------------------------------------------------------------------

atFirstCol :: user -> AlexInput -> Int -> AlexInput -> Bool
atFirstCol _ ai _ _ = (column . position $ ai) == 1

getMatch :: Lex AlexInput a String
getMatch = do
  ai <- getAlexL
  return $ match ai

putMatch :: String -> Lex AlexInput a AlexInput
putMatch newMatch = do
  ai <- getAlexL
  putAlexL $ ai { match = newMatch }

lexComment :: Lex AlexInput a Token
lexComment = do
  match' <- getMatch
  return (TComment $ (take (fromIntegral . length $ match') . tail) match')

lexHollerith :: Lex AlexInput a Token
lexHollerith = do
  match' <- getMatch
  let len = read $ take (length match' - 1) match' -- Get n of "nH" from string
  lexed <- lexN len
  case lexed of
    Just hollerith -> return $ THollerith hollerith
    Nothing -> fail $ "Unrecognisable token"

lexN :: Int -> Lex AlexInput a (Maybe String)
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
        lexN $ n - len
      Nothing -> return Nothing

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
           | TNum String
           | TRealExp
           | TDoubleExp
           | TTrue
           | TFalse
           | TOpPlus
           | TOpMinus
           | TOpExp
           | TOpMul
           | TOpDiv
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
           | TEOF
           deriving (Show, Eq)

--------------------------------------------------------------------------------
-- AlexInput & related definitions
--------------------------------------------------------------------------------

data Position = Position {
  absoluteOffset  :: Integer,
  column          :: Integer,
  line            :: Integer
}

initPosition :: Position
initPosition = Position {
 absoluteOffset = 0,  
 column = 1,  
 line = 1
}

data AlexInput = AlexInput {
  sourceInput               :: String,
  position                  :: Position,
  bytes                     :: [Word8],
  previousChar              :: Char,
  match                     :: String,
  matchCaseSensitive        :: String,
  whiteSensitiveCharCount   :: Integer
}

vanillaAlexInput :: AlexInput
vanillaAlexInput = AlexInput {
  sourceInput = "",
  position = initPosition,
  bytes = [],
  previousChar = '\n',
  match = "",
  matchCaseSensitive = "",
  whiteSensitiveCharCount = 0
}

--------------------------------------------------------------------------------
-- Definitions needed for alexScanUser
--------------------------------------------------------------------------------

data Move = Continuation | Char | Newline

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai 
  -- The process of reading individual bytes of the character
  | bytes' /= [] = Just (head bytes', ai { bytes = tail bytes' })
  -- When all characters are already read
  | absoluteOffset position' + 1 == (toInteger . length . sourceInput) ai = Nothing
  -- Skip the continuation line altogether
  | isContinuation ai && isWhiteInsensitive = skip Continuation ai 
  -- If we are not parsing a Hollerith skip whitespace
  | curChar == ' ' && isWhiteInsensitive = skip Char ai
  -- Read genuine character and advance. Also covers white sensitivity.
  | otherwise = 
      let (b:bs) = (utf8Encode . toLower) curChar in
        Just(b,
          ai {
            position =
              case curChar of
                '\n'  -> advance Newline position'
                _     -> advance Char position',
            bytes = bs,
            previousChar = curChar,
            match = (toLower curChar):(match ai),
            matchCaseSensitive = curChar:(matchCaseSensitive ai),
            whiteSensitiveCharCount = 
              if isWhiteInsensitive
              then 0
              else whiteSensitiveCharCount ai - 1
          })
  where
    curChar = currentChar ai
    bytes' = bytes ai
    position' = position ai
    isWhiteInsensitive = whiteSensitiveCharCount ai == 0

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar ai = previousChar ai

takeNChars :: Integer -> AlexInput -> String
takeNChars n ai = 
  take (fromIntegral n) . drop (fromIntegral dropN) $ sourceInput ai
  where 
    dropN = absoluteOffset . position $ ai 

currentChar :: AlexInput -> Char 
currentChar ai = head (takeNChars 1 ai)

isContinuation :: AlexInput -> Bool
isContinuation ai = 
  take 6 next7 == "\n     " && not (last next7 `elem` [' ', '0'])
  where next7 = takeNChars 7 ai

skip :: Move -> AlexInput -> Maybe (Word8, AlexInput)
skip move ai = 
  let newPosition = advance move $ position ai in
    alexGetByte $ ai { position = newPosition }

advance :: Move -> Position -> Position
advance move position' =
  case move of 
    Char -> 
      position' { absoluteOffset = absl + 1, column = col + 1 }
    Continuation -> 
      position' { absoluteOffset = absl + 7, column = 7, line = line' + 1 }
    Newline -> 
      position' { absoluteOffset = absl + 1, column = 1, line = line' + 1 }
  where
    col = column position'
    line' = line position'
    absl = absoluteOffset position'

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
    go oc
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

lexer :: (Token -> Parse AlexInput Token) -> Parse AlexInput Token
lexer = runContT lexer'
            
lexer' :: Lex AlexInput Token Token
lexer' = do
  putMatch ""
  alexInput <- getAlexL
  case (alexScanUser undefined alexInput 0) of
    AlexEOF -> return TEOF
    AlexError _ -> fail $ "Unrecognisable token"
    AlexSkip newAlex _ -> putAlexL newAlex >> lexer'
    AlexToken newAlex _ action -> do
      putAlexL newAlex
      action

}
