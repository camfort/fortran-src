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
  function              { TFunction }
  subroutine            { TSubroutine }
  blockData             { TBlockData }
  end                   { TEnd }
  '='                   { TOpAssign }
  assign                { TAssign }
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

BLOCKS :: { [ (Maybe (Expression A0), Block A0) ] }
BLOCKS
: BLOCKS_LEVEL1 COMMENTS { $1 }

BLOCKS_LEVEL1 :: { [ (Maybe (Expression A0), Block A0) ] }
: BLOCKS_LEVEL1 COMMENTS LABELED_BLOCK { (fst $3, setComments (snd $3) (reverse $2)) : $1 }
| BLOCKS_LEVEL1 LABELED_BLOCK LABELED_BLOCK { $3 : $2 : $1 }
| LABELED_BLOCK { [ $1 ] }
| {- EMPTY -} { [ ] }

-- TODO In this version an empty line followed by a block doesn't work.
LABELED_BLOCK :: { (Maybe (Expression A0), Block A0) }
LABELED_BLOCK
: LABEL_IN_6COLUMN BLOCK { (Just $1, $2) }
| BLOCK       { (Nothing, $1) }

BLOCK :: { Block A0 }
BLOCK : srcloc STATEMENT {% getSrcSpan $1 >>= \s -> return $ BlStatement () s $2 [] }

COMMENTS :: { [ Comment a ] }
COMMENTS
: COMMENTS COMMENT { $2 : $1 }
| {- EMPTY -} { [ ] }

COMMENT :: { Comment a }
COMMENT : srcloc comment {% getSrcSpan $1 >>= \s -> return $ Comment () s $2 }

ARITHMETIC_IF_BLOCK :: { Block A0 }
ARITHMETIC_IF_BLOCK
: srcloc OTHER_EXECUTABLE_STATEMENT {% getSrcSpan $1 >>= \s -> return $ BlStatement () s $2 [] }

EXPRESSION_ASSIGNMENT_BLOCK :: { Block A0 }
EXPRESSION_ASSIGNMENT_BLOCK
: srcloc EXPRESSION_ASSIGNMENT_STATEMENT {% getSrcSpan $1 >>= \s -> return $ BlStatement () s $2 [] }

STATEMENT :: { Statement A0 }
STATEMENT
: LOGICAL_IF_STATEMENT { $1 }
| DO_STATEMENT { $1 }
| OTHER_EXECUTABLE_STATEMENT { $1 }
| NONEXECUTABLE_STATEMENT { $1 }

LOGICAL_IF_STATEMENT :: { Statement A0 }
LOGICAL_IF_STATEMENT : if '(' LOGICAL_EXPRESSION ')' ARITHMETIC_IF_BLOCK { StIfLogical $3 $5 }

DO_STATEMENT :: { Statement A0 }
DO_STATEMENT
: do LABEL_IN_STATEMENT DO_SPECIFICATION { let (init, limit, step) = $3 in StDo $2 init limit step }

DO_SPECIFICATION :: { (Block A0, Expression A0, Maybe (Expression A0))}
DO_SPECIFICATION
: EXPRESSION_ASSIGNMENT_BLOCK ',' INT_OR_VAR ',' INT_OR_VAR { ($1, $3, Just $5)}
| EXPRESSION_ASSIGNMENT_BLOCK ',' INT_OR_VAR                { ($1, $3, Nothing)}

INT_OR_VAR :: { Expression A0 } : INTEGER_LITERAL { $1 } | VARIABLE { $1 }

OTHER_EXECUTABLE_STATEMENT :: { Statement A0 }
OTHER_EXECUTABLE_STATEMENT
: EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| assign LABEL_IN_STATEMENT to VARIABLE { StLabelAssign $2 $4 }
| goto LABEL_IN_STATEMENT { StGotoUnconditional $2 }
| goto VARIABLE LABELS_IN_STATEMENT { StGotoAssigned $2 $3 }
| goto LABELS_IN_STATEMENT VARIABLE { StGotoComputed $2 $3 }
| if '(' ARITHMETIC_EXPRESSION ')' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT { StIfArithmetic $3 $5 $7 $9 }
| call SUBROUTINE_NAME CALLABLE_EXPRESSIONS { StCall $2 $3 }
| return { StReturn }
| continue { StContinue }
| stop INTEGER_LITERAL { StStop $2 }
| pause INTEGER_LITERAL { StPause $2 }
| rewind UNIT { StRewind $2 }
| backspace UNIT { StBackspace $2 }
| endfile UNIT { StEndfile $2 }
| write READ_WRITE_ARGUMENTS { let (unit, form, list) = $2 in StWrite unit form list }
| read READ_WRITE_ARGUMENTS { let (unit, form, list) = $2 in StRead unit form list }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
EXPRESSION_ASSIGNMENT_STATEMENT : ELEMENT '=' EXPRESSION { StExpressionAssign $1 $3 }

NONEXECUTABLE_STATEMENT :: { Statement A0 }
: external PROCEDURES { let rev = reverse $2 in StExternal (AList () (getListSpan rev) $ rev) }
| dimension ARRAY_DECLARATORS { let rev = reverse $2 in StDimension (AList () (getListSpan rev) $ rev) }
| common COMMON_GROUPS { let rev = reverse $2 in StCommon (AList () (getListSpan rev) $ rev) }
| equivalence EQUIVALENCE_GROUPS { let rev = reverse $2 in StEquivalence (AList () (getListSpan rev) $ rev) }
| data  DATA_GROUPS { let rev = reverse $2 in StData (AList () (getListSpan rev) $ rev) }
| format srcloc '(' FORMAT_ITEMS ')' {% getSrcSpan $2 >>= \s -> return $ StFormat (AList () s $ reverse $4) }
| type DECLARATORS { StDeclaration (read $1) (let rev = reverse $2 in AList () (getListSpan rev) rev) }


READ_WRITE_ARGUMENTS :: { (Expression A0, Maybe (Expression A0), AList (IOElement A0) A0) }
READ_WRITE_ARGUMENTS
: '(' UNIT ')' READ_WRITE_ARGUMENTS_LEVEL1 { ($2, Nothing, $4) }
| '(' UNIT ',' FORM ')' READ_WRITE_ARGUMENTS_LEVEL1 { ($2, Just $4, $6) }

READ_WRITE_ARGUMENTS_LEVEL1 :: { AList (IOElement A0) A0 }
READ_WRITE_ARGUMENTS_LEVEL1
: IO_ELEMENTS { let rev = reverse $1 in AList () (getListSpan rev) rev }

-- Not my terminology a VAR or an INT (probably positive) is defined as UNIT.
UNIT :: { Expression A0 } : INTEGER_LITERAL { $1 } | VARIABLE { $1 }

FORM :: { Expression A0 } : ARRAY { $1 } | LABEL_IN_STATEMENT { $1 }

IO_ELEMENTS :: { [ IOElement A0 ] }
IO_ELEMENTS
: IO_ELEMENTS ',' IO_ELEMENT { $3 : $1}
| IO_ELEMENT { [ $1 ] }

IO_ELEMENT :: { IOElement A0 }
IO_ELEMENT
: VARIABLE { IOExpression $1 }
-- There should also be a caluse for variable names but not way to 
-- differentiate it at this stage from VARIABLE. Hence, it is omitted to prevent
-- reduce/reduce conflict.
| SUBSCRIPT { IOExpression $1 }
-- TODO after handling blocks | srcloc '(' IO_ELEMENTS ',' DO_SPEC ')' {% getSrcSpan $1 >>= \s -> return $ IOTuple () s $3 $5 }
| '(' ELEMENTS ')' { let rev = reverse $2 in IOExpressionList $ AList () (getListSpan rev) rev} 

ELEMENTS :: { [ Expression A0 ] }
ELEMENTS
: ELEMENTS ',' ELEMENT { $3 : $1 }
| ELEMENT { [ $1 ] }

ELEMENT :: { Expression A0 }
ELEMENT
: VARIABLE { $1 }
| SUBSCRIPT { $1 }

FORMAT_ITEMS :: { [ FormatItem A0 ] }
FORMAT_ITEMS
: FORMAT_ITEMS ',' FORMAT_ITEM { $3 : $1 }
| FORMAT_ITEMS ',' FORMAT_ITEM_DELIMETER { $3 : $1 }
| FORMAT_ITEMS FORMAT_ITEM { $2 : $1 }
| FORMAT_ITEMS FORMAT_ITEM_DELIMETER { $2 : $1 }
| FORMAT_ITEM { [ $1 ] }
| FORMAT_ITEM_DELIMETER { [ $1 ] }
| {- EMPTY -} { [ ] }

FORMAT_ITEM_DELIMETER :: { FormatItem A0 }
FORMAT_ITEM_DELIMETER: srcloc '/' {% getSrcSpan $1 >>= \s -> return $ FIDelimiter () s }

FORMAT_ITEM :: { FormatItem A0 }
FORMAT_ITEM
: srcloc int '(' FORMAT_ITEMS ')' {% getSrcSpan $1 >>= \s -> return $ FIFormatList () s (Just $2) (let rev = reverse $4 in AList () (getListSpan rev) rev) }
| srcloc '(' FORMAT_ITEMS ')' {% getSrcSpan $1 >>= \s -> return $ FIFormatList () s Nothing (let rev = reverse $3 in AList () (getListSpan rev) rev) }
| HOLLERITH { let (ExpValue _ s val) = $1 in FIHollerith () s val }
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
| DECLARATOR  { [ $1 ] }

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
PROCEDURES :: { [ Expression A0 ] }
PROCEDURES
: PROCEDURES ',' FUNCTION_NAME { $3 : $1 }
| FUNCTION_NAME { [ $1 ] }

CALLABLE_EXPRESSIONS :: { AList (Expression A0) A0 }
CALLABLE_EXPRESSIONS
: srcloc CALLABLE_EXPRESSIONS_LEVEL1 ')' {% getSrcSpan $1 >>= \s -> return $ AList () s $ reverse $2 }

CALLABLE_EXPRESSIONS_LEVEL1 :: { [ Expression A0 ] }
CALLABLE_EXPRESSIONS_LEVEL1
: CALLABLE_EXPRESSIONS_LEVEL1 ',' CALLABLE_EXPRESSION { $3 : $1 }
| '(' CALLABLE_EXPRESSION { [ $2 ] } 
| '(' { [ ] }

-- Expression all by itself subsumes all other callable expressions.
CALLABLE_EXPRESSION :: { Expression A0 }
CALLABLE_EXPRESSION
: HOLLERITH   { $1 }
| EXPRESSION  { $1 }

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
SUBSCRIPT : ARRAY INDICIES { ExpSubscript () (getTransSpan $1 $2) $1 $2 }

INDICIES :: { AList (Expression A0) A0 }
INDICIES 
: srcloc '(' INDICIES_LEVEL1 {% getSrcSpan $1 >>= \s -> return $ AList () s $3 }

INDICIES_LEVEL1 :: { [Expression A0] }
INDICIES_LEVEL1
: INDEX ',' INDICIES_LEVEL1 { $1:$3 }
| INDEX ')' { [ $1 ] }
| ')' { [ ] }

INDEX :: { Expression A0 }
INDEX
: INTEGER_LITERAL { $1 }
| INDEX_LEVEL1 '+' INTEGER_LITERAL { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 } 
| INDEX_LEVEL1 '-' INTEGER_LITERAL { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 } 
| INDEX_LEVEL1 { $1 }

INDEX_LEVEL1 :: { Expression A0 }
INDEX_LEVEL1
: INTEGER_LITERAL '*' VARIABLE { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 } 
| VARIABLE { $1 }

ARITHMETIC_SIGN :: { UnaryOp }
ARITHMETIC_SIGN
: '-' { Minus }
| '+' { Plus }

-- This may also be used to parse a function name, or an array name. Since when
-- are valid options in a production there is no way of differentiating them at
-- this stage.
-- This at least reduces reduce/reduce conflicts.
VARIABLE :: { Expression A0 }
VARIABLE : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValVariable $2) }

ARRAY :: { Expression A0 }
ARRAY : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValArray $2) }

FUNCTION_NAME :: { Expression A0 }
FUNCTION_NAME : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValFunctionName $2) }

SUBROUTINE_NAME :: { Expression A0 }
SUBROUTINE_NAME : srcloc id {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValSubroutineName $2) }

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

LABELS_IN_6COLUMN :: { AList (Expression A0) A0 }
LABELS_IN_6COLUMN
: srcloc LABELS_IN_6COLUMN_LEVEL1 ')' {% getSrcSpan $1 >>= \s -> return $ AList () s $ reverse $2 }

LABELS_IN_6COLUMN_LEVEL1 :: { [ Expression A0 ] }
LABELS_IN_6COLUMN_LEVEL1
: LABELS_IN_6COLUMN_LEVEL1 ',' LABEL_IN_6COLUMN { $3 : $1 }
| '(' LABEL_IN_6COLUMN { [ $2 ] }

LABELS_IN_STATEMENT :: { AList (Expression A0) A0 }
LABELS_IN_STATEMENT
: srcloc LABELS_IN_STATEMENT_LEVEL1 ')' {% getSrcSpan $1 >>= \s -> return $ AList () s $ reverse $2 }

LABELS_IN_STATEMENT_LEVEL1 :: { [ Expression A0 ] }
LABELS_IN_STATEMENT_LEVEL1
: LABELS_IN_STATEMENT_LEVEL1 ',' LABEL_IN_STATEMENT { $3 : $1 }
| '(' LABEL_IN_STATEMENT { [ $2 ] }

-- Labels that occur in the first 6 columns
LABEL_IN_6COLUMN :: { Expression A0 } : srcloc label {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValLabel $2) }

-- Labels that occur in statements
LABEL_IN_STATEMENT :: { Expression A0 } : srcloc int {% getSrcSpan $1 >>= \s -> return $ ExpValue () s (ValLabel $2) }

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
parseError _ = fail "Couldn't parse."

}
