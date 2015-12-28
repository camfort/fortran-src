{
module Forpar.Parser.Fortran66 where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST

import Forpar.Util.Position (SrcLoc(..))
import Forpar.ParserMonad (Parse(..), getSrcSpan, getSrcLoc)
import Forpar.Lexer.FixedForm
import Forpar.AST

import Debug.Trace

}

%name expressionParser EXPRESSION
%monad { Parse AlexInput }
%lexer { lexer } { TEOF }
%tokentype { Token }
%error { parseError }

%token
  '('                   { TLeftPar }
  ')'                   { TRightPar }
  ','                   { TComma }
  '.'                   { TDot }
  function              { TFunction }
  subroutine            { TSubroutine }
  blockData             { TBlockData }
  end                   { TEnd }
  '='                   { TOpAssign }
  to                    { TTo }
  goto                  { TGoto }
  if                    { TIf }
  call                  { TCall }
  return                { TReturn }
  continue              { TContinue }
  stop                  { TStop }
  pause                 { TPause }
  do                    { TDo }
  read                  { TRead }
  write                 { TWrite }
  rewind                { TRewind }
  backspace             { TBackspace }
  endfile               { TEndfile }
  common                { TCommon }
  equivalence           { TEquivalence }
  external              { TExternal }
  type                  { (TType $$) }
  data                  { TData }
  format                { TFormat }
  fieldDescriptorDEFG   { (TFieldDescriptorDEFG _ _ _ _) }
  fieldDescriptorIAL    { (TFieldDescriptorAIL _ _ _) }
  blankDescriptor       { (TBlankDescriptor $$) }
  scaleFactor           { (TScaleFactor $$) }
  int                   { (TInt $$) }
  real                  { (TReal $$) }
  true                  { TTrue }
  false                 { TFalse }
  '+'                   { TOpPlus }
  '-'                   { TOpMinus }
  '**'                  { TOpExp }
  '*'                   { TStar }
  '/'                   { TSlash }
  or                    { TOpOr }
  and                   { TOpAnd }
  not                   { TOpNot }
  '<'                   { TOpLT }
  '<='                  { TOpLE }
  '>'                   { TOpGT }
  '>='                  { TOpGE }
  '=='                  { TOpEQ }
  '!='                  { TOpNE }
  id                    { (TId $$) }
  comment               { (TComment $$) }
  hollerith             { (THollerith $$) }
  label                 { (TLabel $$) }
  EOF                   { TEOF }

%%

EXPRESSION :: { Expression A0 }
EXPRESSION
: ARITHMETIC_EXPRESSION { $1 }
| LOGICAL_EXPRESSION    { $1 }

ARITHMETIC_EXPRESSION :: { Expression A0 }
ARITHMETIC_EXPRESSION
: srcloc ARITHMETIC_EXPRESSION '+' ARITHMETIC_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Addition $2 $4 }
| srcloc ARITHMETIC_EXPRESSION '-' ARITHMETIC_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Subtraction $2 $4 }
| ARITHMETIC_LEVEL1  { $1 }

ARITHMETIC_LEVEL1 :: { Expression A0 }
ARITHMETIC_LEVEL1
: srcloc ARITHMETIC_LEVEL1 '*' ARITHMETIC_LEVEL2 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Multiplication $2 $4 }
| srcloc ARITHMETIC_LEVEL1 '/' ARITHMETIC_LEVEL2 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Division $2 $4 }
| ARITHMETIC_LEVEL2  { $1 }

ARITHMETIC_LEVEL2 :: { Expression A0 }
ARITHMETIC_LEVEL2
: srcloc ARITHMETIC_LEVEL2 '**' ARITHMETIC_LEVEL3 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Exponentiation $2 $4 }
| ARITHMETIC_LEVEL3 { $1 }

ARITHMETIC_LEVEL3 :: { Expression A0 }
ARITHMETIC_LEVEL3 
: srcloc ARITHMETIC_SIGN ARITHMETIC_PRIMARY {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| ARITHMETIC_PRIMARY { $1 }

ARITHMETIC_PRIMARY :: { Expression A0 }
ARITHMETIC_PRIMARY
: '(' ARITHMETIC_EXPRESSION ')' { $2 }
| INTEGER_LITERAL               { $1 }
| REAL_LITERAL                  { $1 }
| COMPLEX_LITERAL               { $1 }
| SUBSCRIPT                     { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| VARIABLE                      { $1 }

LOGICAL_EXPRESSION :: { Expression A0 }
LOGICAL_EXPRESSION
: srcloc LOGICAL_EXPRESSION or LOGICAL_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s Or $2 $4 }
| LOGICAL_LEVEL1  { $1 }

LOGICAL_LEVEL1 :: { Expression A0 }
LOGICAL_LEVEL1
: srcloc LOGICAL_LEVEL1 and LOGICAL_LEVEL2 {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s And $2 $4 }
| LOGICAL_LEVEL2  { $1 }

LOGICAL_LEVEL2 :: { Expression A0 }
LOGICAL_LEVEL2
: srcloc not LOGICAL_LEVEL2 {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s Not $3 }
| LOGICAL_PRIMARY  { $1 }

LOGICAL_PRIMARY :: { Expression A0 }
LOGICAL_PRIMARY
: '(' LOGICAL_EXPRESSION ')'  { $2 }
| LOGICAL_LITERAL             { $1 }
| RELATIONAL_EXPRESSION       { $1 }
| SUBSCRIPT                   { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| VARIABLE                    { $1 }

RELATIONAL_EXPRESSION :: { Expression A0 }
RELATIONAL_EXPRESSION
: srcloc ARITHMETIC_EXPRESSION RELATIONAL_OPERATOR ARITHMETIC_EXPRESSION {% getSrcSpan $1 >>= \s -> return $ ExpBinary () s $3 $2 $4 }

RELATIONAL_OPERATOR :: { BinaryOp }
RELATIONAL_OPERATOR
: '=='  { EQ }
| '!='  { NE }
| '>'   { GT }
| '>='  { GTE }
| '<'   { LT }
| '<='  { LTE }

SUBSCRIPT :: { Expression A0 }
SUBSCRIPT : srcloc id ELEMENT_LIST {% getSrcSpan $1 >>= \s -> return $ ExpSubscript () s (ValArray $2) $3 } 

ELEMENT_LIST :: { AList (Expression A0) A0 }
ELEMENT_LIST 
: srcloc '(' ELEMENT_LIST_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ AList () s $3 }

ELEMENT_LIST_LEVEL1 :: { [Expression A0] }
ELEMENT_LIST_LEVEL1
: ELEMENT ',' ELEMENT_LIST_LEVEL1 { $1:$3 }
| ELEMENT ')' { [$1] }
| ')' { [] }

ELEMENT :: { Expression A0 }
ELEMENT 
: LITERAL   { $1 }
| VARIABLE  { $1 }

ARITHMETIC_SIGN :: { UnaryOp }
ARITHMETIC_SIGN
: '-' { Minus }
| '+' { Plus }

VARIABLE :: { Expression A0 }
VARIABLE : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValVariable $2) }

LITERAL :: { Expression A0 }
LITERAL
: INTEGER_LITERAL { $1 }
| REAL_LITERAL    { $1 }
| COMPLEX_LITERAL { $1 }
| LOGICAL_LITERAL { $1 }

INTEGER_LITERAL :: { Expression A0 }
INTEGER_LITERAL
: srcloc ARITHMETIC_SIGN INTEGER_LITERAL_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| INTEGER_LITERAL_LEVEL1 { $1 }

INTEGER_LITERAL_LEVEL1 :: { Expression A0 }
INTEGER_LITERAL_LEVEL1: srcloc int {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValInteger $2) }

REAL_LITERAL :: { Expression A0 }
REAL_LITERAL
: srcloc ARITHMETIC_SIGN REAL_LITERAL_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| REAL_LITERAL_LEVEL1 { $1 }

REAL_LITERAL_LEVEL1 :: { Expression A0 }
REAL_LITERAL_LEVEL1: srcloc real {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValReal $2) }

NUMERIC_LITERAL :: { Expression A0 }
NUMERIC_LIETERAL
: INTEGER_LITERAL { $1 }
| REAL_LITERAL    { $1 }

COMPLEX_LITERAL :: { Expression A0 }
COMPLEX_LITERAL
: srcloc '(' NUMERIC_LITERAL ',' NUMERIC_LITERAL ')' {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValComplex $3 $5)}

LOGICAL_LITERAL :: { Expression A0 }
LOGICAL_LITERAL
: srcloc true   {% getSrcSpan $1 >>= \s -> return $ ExpValue () s ValTrue }
| srcloc false  {% getSrcSpan $1 >>= \s -> return $ ExpValue () s ValFalse }

srcloc :: { SrcLoc }
srcloc :  {- EMPTY -} {% getSrcLoc }

{

type A0 = ()

parseError :: Token -> Parse AlexInput a
parseError _ = fail "Blah blah"

}
