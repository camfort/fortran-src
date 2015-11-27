{
module Forpar.Parser.Fortran66 where

import Forpar.ParserMonad (Parse)
import Forpar.Lexer.FixedForm

}

%name parser66
%monad { Parse AlexInput }
%tokentype { Token }
%error { parseError }

%token
  '('           { TLeftPar }
  ')'           { TRightPar }
  ','           { TComma }
  '.'           { TDot }
  function      { TFunction }
  subroutine    { TSubroutine }
  blockData     { TBlockData }
  end           { TEnd }
  '='           { TOpAssign }
  to            { TTo }
  goto          { TGoto }
  if            { TIf }
  call          { TCall }
  return        { TReturn }
  continue      { TContinue }
  stop          { TStop }
  pause         { TPause }
  do            { TDo }
  read          { TRead }
  write         { TWrite }
  rewind        { TRewind }
  backspace     { TBackspace }
  endfile       { TEndfile }
  common        { TCommon }
  equivalence   { TEquivalence }
  external      { TExternal }
  type          { TType $$ }
  data          { TData }
  format        { TFormat }
  formatItem    { TFormatItem $$ }
  int           { TNum $$ }
  e             { TRealExp }
  d             { TDoubleExp }
  true          { TTrue }
  false         { TFalse }
  '+'           { TOpPlus }
  '-'           { TOpMinus }
  '**'          { TOpExp }
  '*'           { TStar }
  '/'           { TSlash }
  or            { TOpOr }
  and           { TOpAnd }
  not           { TOpNot }
  '<'           { TOpLT }
  '<='          { TOpLE }
  '>'           { TOpGT }
  '>='          { TOpGE }
  id            { TId $$ }
  comment       { TComment $$ }
  hollerith     { THollerith $$ }
  label         { TLabel $$ }
  EOF           { TEOF }

%%
  

{

-- parseError :: Token -> P a
parseError = fail "blah blah"

}
