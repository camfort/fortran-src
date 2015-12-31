{
module Forpar.Parser.Fortran66(expressionParser,
                               statementParser) where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST

import Forpar.Util.Position (SrcLoc(..), SrcSpan(..))
import Forpar.ParserMonad (Parse(..), getSrcSpan, getSrcLoc)
import Forpar.Lexer.FixedForm
import Forpar.AST

import Debug.Trace

}

%name statementParser STATEMENT
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
  dimension             { TDimension }
  type                  { (TType $$) }
  data                  { TData }
  format                { TFormat }
  fieldDescriptorDEFG   { (TFieldDescriptorDEFG _ _ _ _) }
  fieldDescriptorAIL    { (TFieldDescriptorAIL _ _ _) }
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

%left or
%left and
%right not

%nonassoc '>' '<' '>=' '<=' '==' '!='

%left '+' '-'
%left '*' '/'
%right NEGATION
%right '**'

%%

STATEMENT :: { Statement A0 }
STATEMENT
: external PROCEDURES { let rev = reverse $2 in StExternal (AList () (getListSpan rev) $ rev) }
| dimension ARRAY_DECLARATORS { let rev = reverse $2 in StDimension (AList () (getListSpan rev) $ rev) }
| common COMMON_GROUPS { let rev = reverse $2 in StCommon (AList () (getListSpan rev) $ rev) }
| equivalence EQUIVALENCE_GROUPS { let rev = reverse $2 in StEquivalence (AList () (getListSpan rev) $ rev) }
| data  DATA_GROUPS { let rev = reverse $2 in StData (AList () (getListSpan rev) $ rev) }
| format srcloc FORMAT_ITEMS ')' {% getSrcSpan $2 >>= \s -> return $ StFormat (AList () s $ reverse $3) }
| format srcloc '(' ')' {% getSrcSpan $2 >>= \s -> return $ StFormat (AList () s []) }

FORMAT_ITEMS :: { [ FormatItem A0 ] }
FORMAT_ITEMS
: FORMAT_ITEMS ',' FORMAT_ITEM { $3 : $1 }
| FORMAT_ITEMS FORMAT_ITEM_DELIMETER FORMAT_ITEM { $3 : $2 : $1 }
| FORMAT_ITEMS FORMAT_ITEM_DELIMETER { $2 : $1 }
| '(' FORMAT_ITEM { [ $2 ] }
| '(' FORMAT_ITEM_DELIMETER { [ $2 ] }

FORMAT_ITEM_DELIMETER :: { FormatItem A0 }
FORMAT_ITEM_DELIMETER: srcloc '/' {% getSrcSpan $1 >>= \s -> return $ FIDelimiter () s }

FORMAT_ITEM :: { FormatItem A0 }
FORMAT_ITEM
: HOLLERITH { let (ExpValue _ s val) = $1 in FIHollerith () s val }
| srcloc fieldDescriptorDEFG {% getSrcSpan $1 >>= \s -> return $ let (TFieldDescriptorDEFG a b c d) = $2 in FIFieldDescriptorDEFG () s a b c d }
| srcloc fieldDescriptorAIL {% getSrcSpan $1 >>= \s -> return $ let (TFieldDescriptorAIL a b c) = $2 in FIFieldDescriptorAIL () s a b c }
| srcloc blankDescriptor {% getSrcSpan $1 >>= \s -> return $ FIBlankDescriptor () s $2 }
| srcloc scaleFactor {% getSrcSpan $1 >>= \s -> return $ FIScaleFactor () s $2 }

DATA_GROUPS :: { [ DataGroup A0 ] }
DATA_GROUPS
: DATA_GROUPS ',' DECLARATORS srcloc '/' DATA_ITEMS '/' {% dataGroup $3 $4 $6 >>= \g -> return $ g : $1}
| DECLARATORS srcloc '/' DATA_ITEMS '/' {% dataGroup $1 $2 $4 >>= \g -> return $ [g] }

DATA_ITEMS :: { [ Expression A0 ] }
DATA_ITEMS
: DATA_ITEMS ',' DATA_ITEM { $3 : $1}
| DATA_ITEM { [ $1 ] }

DATA_ITEM :: { Expression A0 }
DATA_ITEM
: INTEGER_LITERAL '*' DATA_ITEM_LEVEL1 { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| DATA_ITEM_LEVEL1 { $1 }

DATA_ITEM_LEVEL1 :: { Expression A0 }
DATA_ITEM_LEVEL1
: SIGNED_NUMERIC_LITERAL  { $1 }
| COMPLEX_LITERAL         { $1 }
| LOGICAL_LITERAL         { $1 }
| HOLLERITH               { $1 }

EQUIVALENCE_GROUPS :: { [AList (Expression A0) A0] }
EQUIVALENCE_GROUPS
: EQUIVALENCE_GROUPS ',' srcloc '(' DECLARATORS ')' {% getSrcSpan $3 >>= \s -> return $ (AList () s $ reverse $5) : $1}
| srcloc '(' DECLARATORS ')' {% getSrcSpan $1 >>= \s -> return $ [ AList () s $ reverse $3 ] }

DECLARATORS :: { [Expression A0] }
DECLARATORS
: DECLARATORS ',' DECLARATOR { $3 : $1 }
| DECLARATOR { [ $1 ] }

COMMON_GROUPS :: { [CommonGroup A0] }
COMMON_GROUPS
: COMMON_GROUPS COMMON_GROUP { $2 : $1 }
| COMMON_GROUP { [$1] }

COMMON_GROUP :: { CommonGroup A0 }
COMMON_GROUP
: srcloc '/' id '/' COMMON_ELEMENTS {% getSrcSpan $1 >>= \s -> return $ CommonGroup () s (Just $3) $ let rev = reverse $5 in AList () (getListSpan rev) rev }
| srcloc '/' '/' COMMON_ELEMENTS {% getSrcSpan $1 >>= \s -> return $ CommonGroup () s Nothing $ let rev = reverse $4 in AList () (getListSpan rev) rev }
| COMMON_ELEMENTS { let rev = reverse $1; s = getListSpan rev in CommonGroup () s Nothing (AList () s rev) }

COMMON_ELEMENTS :: { [Expression A0] }
COMMON_ELEMENTS
: COMMON_ELEMENTS ',' DECLARATOR { $3 : $1 }
| DECLARATOR  { [$1] }

-- Array name is also a possibility, but there is no way to differentiate it 
-- from a variable.
-- Also subscript is technically not correct an array with size only specified
-- by positive integer values (specifically no variables) is allowed. Here 
-- subscript is used to simplify the matters.
DECLARATOR :: { Expression A0 }
DECLARATOR
: VARIABLE    { $1 }
| SUBSCRIPT   { $1 }

-- Technically, it is not a subscript, but the syntax is identical and there is
-- not meaningful differentiation.
ARRAY_DECLARATORS :: { [Expression A0] }
ARRAY_DECLARATORS
: ARRAY_DECLARATORS ',' SUBSCRIPT { $3 : $1 }
| SUBSCRIPT { [$1] }

-- Here the procedure should be either a function or subroutine name, but 
-- since they are syntactically identical at this stage subroutine names
-- are also emitted as function names.
PROCEDURES :: { [Expression A0] }
PROCEDURES
: PROCEDURES ',' srcloc id {% getSrcSpan $3 >>= \s -> return $ (ExpValue () s (ValFunctionName $4)) : $1 }
| srcloc id {% getSrcSpan $1 >>= \s -> return $ [ ExpValue () s (ValFunctionName $2) ] }

EXPRESSION :: { Expression A0 }
EXPRESSION
: ARITHMETIC_EXPRESSION { $1 }
| LOGICAL_EXPRESSION    { $1 }

ARITHMETIC_EXPRESSION :: { Expression A0 }
ARITHMETIC_EXPRESSION
: ARITHMETIC_EXPRESSION '+' ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| ARITHMETIC_EXPRESSION '-' ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| ARITHMETIC_EXPRESSION '*' ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| ARITHMETIC_EXPRESSION '/' ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| ARITHMETIC_EXPRESSION '**' ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| srcloc ARITHMETIC_SIGN ARITHMETIC_EXPRESSION %prec NEGATION {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| srcloc '(' ARITHMETIC_EXPRESSION ')' { $3 }
| INTEGER_LITERAL               { $1 }
| REAL_LITERAL                  { $1 }
| COMPLEX_LITERAL               { $1 }
| SUBSCRIPT                     { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| VARIABLE                      { $1 }

LOGICAL_EXPRESSION :: { Expression A0 }
LOGICAL_EXPRESSION
: LOGICAL_EXPRESSION or LOGICAL_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| LOGICAL_EXPRESSION and LOGICAL_EXPRESSION { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| srcloc not LOGICAL_EXPRESSION {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s Not $3 }
| srcloc '(' LOGICAL_EXPRESSION ')'  { $3 }
| LOGICAL_LITERAL             { $1 }
| ARITHMETIC_EXPRESSION RELATIONAL_OPERATOR ARITHMETIC_EXPRESSION { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| SUBSCRIPT                   { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| VARIABLE                    { $1 }

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
| EXPRESSION { $1 }

ARITHMETIC_SIGN :: { UnaryOp }
ARITHMETIC_SIGN
: '-' { Minus }
| '+' { Plus }

-- This also parses a function name, or an array name. Since at this stage 
-- they are equivalent introducing separate productions just increase 
-- reduce/reduce conflicts
VARIABLE :: { Expression A0 }
VARIABLE : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValVariable $2) }

LITERAL :: { Expression A0 }
LITERAL
: INTEGER_LITERAL { $1 }
| REAL_LITERAL    { $1 }
| COMPLEX_LITERAL { $1 }
| LOGICAL_LITERAL { $1 }

SIGNED_INTEGER_LITERAL :: { Expression A0 }
SIGNED_INTEGER_LITERAL
: srcloc ARITHMETIC_SIGN INTEGER_LITERAL {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| INTEGER_LITERAL { $1 }

INTEGER_LITERAL :: { Expression A0 } : srcloc int {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValInteger $2) }

SIGNED_REAL_LITERAL :: { Expression A0 }
SIGNED_REAL_LITERAL
: srcloc ARITHMETIC_SIGN REAL_LITERAL {% getSrcSpan $1 >>= \s -> return $ ExpUnary () s $2 $3 }
| REAL_LITERAL { $1 }

REAL_LITERAL :: { Expression A0 } : srcloc real {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValReal $2) }

SIGNED_NUMERIC_LITERAL :: { Expression A0 }
SIGNED_NUMERIC_LIETERAL
: SIGNED_INTEGER_LITERAL { $1 }
| SIGNED_REAL_LITERAL    { $1 }

COMPLEX_LITERAL :: { Expression A0 }
COMPLEX_LITERAL
: srcloc '(' SIGNED_NUMERIC_LITERAL ',' SIGNED_NUMERIC_LITERAL ')' {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValComplex $3 $5)}

LOGICAL_LITERAL :: { Expression A0 }
LOGICAL_LITERAL
: srcloc true   {% getSrcSpan $1 >>= \s -> return $ ExpValue () s ValTrue }
| srcloc false  {% getSrcSpan $1 >>= \s -> return $ ExpValue () s ValFalse }

HOLLERITH :: { Expression A0 } : srcloc hollerith {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValHollerith $2) }

srcloc :: { SrcLoc }
srcloc :  {- EMPTY -} {% getSrcLoc }

{

type A0 = ()

dataGroup :: [Expression A0] -> SrcLoc -> [Expression A0] -> Parse AlexInput (DataGroup A0)
dataGroup revDeclaratorL location revDataItemL = do
  let declaratorL = reverse revDeclaratorL
  let declaratorsSpan@(SrcSpan ds1 ds2) = getListSpan declaratorL
  let declarators = AList () declaratorsSpan declaratorL
  dataItemsSpan@(SrcSpan dis1 dis2) <- getSrcSpan location
  let dataItems = AList () dataItemsSpan $ reverse revDataItemL
  let mergedSpan = SrcSpan ds1 dis2
  let dataGroup = DataGroup () mergedSpan declarators dataItems
  return dataGroup

parseError :: Token -> Parse AlexInput a
parseError _ = fail "Blah blah"

}
