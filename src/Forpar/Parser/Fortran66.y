{
module Forpar.Parser.Fortran66(expressionParser,
                               statementParser,
                               fortran66Parser) where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST

import Data.Maybe (isNothing, fromJust)

import Forpar.Util.Position
import Forpar.ParserMonad
import Forpar.Lexer.FixedForm
import Forpar.AST

}

%name programParser PROGRAM
%name statementParser STATEMENT
%name expressionParser EXPRESSION
%monad { Parse AlexInput }
%lexer { lexer } { TEOF _ }
%tokentype { Token }
%error { parseError }

%token
  '('                   { TLeftPar _ }
  ')'                   { TRightPar _ }
  ','                   { TComma _ }
  function              { TFunction _ }
  subroutine            { TSubroutine _ }
  blockData             { TBlockData _ }
  end                   { TEnd _ }
  '='                   { TOpAssign _ }
  assign                { TAssign _ }
  to                    { TTo _ }
  goto                  { TGoto _ }
  if                    { TIf _ }
  call                  { TCall _ }
  return                { TReturn _ }
  continue              { TContinue _ }
  stop                  { TStop _ }
  pause                 { TPause _ }
  do                    { TDo _ }
  read                  { TRead _ }
  write                 { TWrite _ }
  rewind                { TRewind _ }
  backspace             { TBackspace _ }
  endfile               { TEndfile _ }
  common                { TCommon _ }
  equivalence           { TEquivalence _ }
  external              { TExternal _ }
  dimension             { TDimension _ }
  type                  { TType _ _ }
  data                  { TData _ }
  format                { TFormat _ }
  fieldDescriptorDEFG   { TFieldDescriptorDEFG _ _ _ _ _ }
  fieldDescriptorAIL    { TFieldDescriptorAIL _ _ _ _ }
  blankDescriptor       { TBlankDescriptor _ _ }
  scaleFactor           { TScaleFactor _ _ }
  int                   { TInt _ _ }
  real                  { TReal _ _ }
  true                  { TTrue _ }
  false                 { TFalse _ }
  '+'                   { TOpPlus _ }
  '-'                   { TOpMinus _ }
  '**'                  { TOpExp _ }
  '*'                   { TStar _ }
  '/'                   { TSlash _ }
  or                    { TOpOr _ }
  and                   { TOpAnd _ }
  not                   { TOpNot _ }
  '<'                   { TOpLT _ }
  '<='                  { TOpLE _ }
  '>'                   { TOpGT _ }
  '>='                  { TOpGE _ }
  '=='                  { TOpEQ _ }
  '!='                  { TOpNE _ }
  id                    { TId _ _ }
  comment               { TComment _ _ }
  hollerith             { THollerith _ _ }
  label                 { TLabel _ _ }
  newline               { TNewline _ }

%left or
%left and
%right not

%nonassoc '>' '<' '>=' '<=' '==' '!='

%left '+' '-'
%left '*' '/'
%right NEGATION
%right '**'

%%

PROGRAM :: { [ ProgramUnit A0 ] }
PROGRAM
: PROGRAM_UNITS { reverse $1 }
| PROGRAM_UNITS COMMENTS { reverse $1 }

PROGRAM_UNITS :: { [ ProgramUnit A0 ] }
PROGRAM_UNITS
: PROGRAM_UNITS PROGRAM_UNIT { $2 : $1 } 
| PROGRAM_UNIT { [ $1 ] } 

PROGRAM_UNIT :: { ProgramUnit A0 }
PROGRAM_UNIT
: MAIN_PROGRAM_UNIT NEWLINE { $1 }
| OTHER_PROGRAM_UNIT NEWLINE { $1 }

MAIN_PROGRAM_UNIT :: { ProgramUnit A0 }
MAIN_PROGRAM_UNIT
: BLOCKS end { let blocks = reverse $1 in PUMain () (getTransSpan $1 $2) Nothing blocks (getComments (snd . head $ blocks)) }

OTHER_PROGRAM_UNIT :: { ProgramUnit A0 }
OTHER_PROGRAM_UNIT
: COMMENTS OTHER_PROGRAM_UNIT_LEVEL1 { setComments $2 (reverse $1) }
| OTHER_PROGRAM_UNIT_LEVEL1 { $1 }

OTHER_PROGRAM_UNIT_LEVEL1 :: { ProgramUnit A0 }
OTHER_PROGRAM_UNIT_LEVEL1
: type function NAME '(' ARGS ')' NEWLINE BLOCKS end { PUFunction () (getTransSpan $1 $9) (let (TType _ t) = $1 in Just $ read t) $3 (aReverse $5) (reverse $8) [] }
| function NAME '(' ARGS ')' NEWLINE BLOCKS end { PUFunction () (getTransSpan $1 $8) Nothing $2 (aReverse $4) (reverse $7) [] }
| subroutine NAME '(' ARGS ')' NEWLINE BLOCKS end { PUSubroutine () (getTransSpan $1 $8) $2 $4 (reverse $7) [] }
| blockData NEWLINE BLOCKS end { PUBlockData () (getTransSpan $1 $4) (reverse $3) [] }

ARGS :: { AList String A0 }
ARGS
: ARGS ',' id { let (TId s arg) = $3 in setSpan s $ arg `aCons` $1}
| id { let (TId s arg) = $1 in AList () s [ arg ] }

NAME :: { Name } : id { let (TId _ name) = $1 in name }

BLOCKS :: { [ (Maybe (Expression A0), Block A0) ] }
BLOCKS
: BLOCKS_LEVEL1 COMMENTS { $1 }
| BLOCKS_LEVEL1 { $1 }

BLOCKS_LEVEL1 :: { [ (Maybe (Expression A0), Block A0) ] }
: BLOCKS_LEVEL1 COMMENTS LABELED_BLOCK { (fst $3, setComments (snd $3) (reverse $2)) : $1 }
| BLOCKS_LEVEL1 LABELED_BLOCK { $2 : $1 }
| COMMENTS LABELED_BLOCK { [ (fst $2, setComments (snd $2) (reverse $1)) ] }
| LABELED_BLOCK { [ $1 ] }

-- TODO In this version an empty line followed by a block doesn't work.
LABELED_BLOCK :: { (Maybe (Expression A0), Block A0) }
LABELED_BLOCK
: LABEL_IN_6COLUMN BLOCK { (Just $1, $2) }
| BLOCK { (Nothing, $1) }

BLOCK :: { Block A0 }
BLOCK : STATEMENT NEWLINE { BlStatement () (getSpan $1) $1 [] }

COMMENTS :: { [ Comment A0 ] }
COMMENTS
: COMMENTS COMMENT { $2 : $1 }
| COMMENT { [ $1 ] }

COMMENT :: { Comment A0 }
COMMENT : comment NEWLINE { let (TComment s c) = $1 in Comment () s c }

NEWLINE :: { Token } 
NEWLINE
: NEWLINE newline { $1 }
| newline { $1 }

STATEMENT :: { Statement A0 }
STATEMENT
: LOGICAL_IF_STATEMENT { $1 }
| DO_STATEMENT { $1 }
| OTHER_EXECUTABLE_STATEMENT { $1 }
| NONEXECUTABLE_STATEMENT { $1 }

LOGICAL_IF_STATEMENT :: { Statement A0 }
LOGICAL_IF_STATEMENT : if '(' LOGICAL_EXPRESSION ')' OTHER_EXECUTABLE_STATEMENT { StIfLogical () (getTransSpan $1 $5) $3 $5 }

DO_STATEMENT :: { Statement A0 }
DO_STATEMENT
: do LABEL_IN_STATEMENT DO_SPECIFICATION { let (init, limit, step) = $3 in StDo () (getTransSpan $1 (if isNothing step then $2 else fromJust step)) $2 init limit step }

DO_SPECIFICATION :: { (Statement A0, Expression A0, Maybe (Expression A0))}
DO_SPECIFICATION
: EXPRESSION_ASSIGNMENT_STATEMENT ',' INT_OR_VAR ',' INT_OR_VAR { ($1, $3, Just $5)}
| EXPRESSION_ASSIGNMENT_STATEMENT ',' INT_OR_VAR                { ($1, $3, Nothing)}

INT_OR_VAR :: { Expression A0 } : INTEGER_LITERAL { $1 } | VARIABLE { $1 }

OTHER_EXECUTABLE_STATEMENT :: { Statement A0 }
OTHER_EXECUTABLE_STATEMENT
: EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| assign LABEL_IN_STATEMENT to VARIABLE { StLabelAssign () (getTransSpan $1 $4) $2 $4 }
| goto LABEL_IN_STATEMENT { StGotoUnconditional () (getTransSpan $1 $2) $2 }
| goto VARIABLE LABELS_IN_STATEMENT { StGotoAssigned () (getTransSpan $1 $3) $2 $3 }
| goto LABELS_IN_STATEMENT VARIABLE { StGotoComputed () (getTransSpan $1 $3) $2 $3 }
| if '(' ARITHMETIC_EXPRESSION ')' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT { StIfArithmetic () (getTransSpan $1 $9) $3 $5 $7 $9 }
| call SUBROUTINE_NAME CALLABLE_EXPRESSIONS { StCall () (getTransSpan $1 $3) $2 $3 }
| return { StReturn () $ getSpan $1 }
| continue { StContinue () $ getSpan $1 }
| stop INTEGER_LITERAL { StStop () (getTransSpan $1 $2) $2 }
| pause INTEGER_LITERAL { StPause () (getTransSpan $1 $2) $2 }
| rewind UNIT { StRewind () (getTransSpan $1 $2) $2 }
| backspace UNIT { StBackspace () (getTransSpan $1 $2) $2 }
| endfile UNIT { StEndfile () (getTransSpan $1 $2) $2 }
| write READ_WRITE_ARGUMENTS { let (unit, form, list) = $2 in StWrite () (getTransSpan $1 $2) unit form list }
| read READ_WRITE_ARGUMENTS { let (unit, form, list) = $2 in StRead () (getTransSpan $1 $2) unit form list }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
EXPRESSION_ASSIGNMENT_STATEMENT : ELEMENT '=' EXPRESSION { StExpressionAssign () (getTransSpan $1 $3) $1 $3 }

NONEXECUTABLE_STATEMENT :: { Statement A0 }
: external PROCEDURES { StExternal () (getTransSpan $1 $2) (aReverse $2) }
| dimension ARRAY_DECLARATORS { StDimension () (getTransSpan $1 $2) (aReverse $2) }
| common COMMON_GROUPS { StCommon () (getTransSpan $1 $2) (aReverse $2) }
| equivalence EQUIVALENCE_GROUPS { StEquivalence () (getTransSpan $1 $2) (aReverse $2) }
| data DATA_GROUPS { StData () (getTransSpan $1 $2) (aReverse $2) }
| format FORMAT_ITEMS ')' { StFormat () (getTransSpan $1 $3) (aReverse $2) }
| type DECLARATORS { StDeclaration () (getTransSpan $1 $2) (let (TType _ t) = $1 in read t) (aReverse $2) }

READ_WRITE_ARGUMENTS :: { (Expression A0, Maybe (Expression A0), Maybe (AList (IOElement A0) A0)) }
READ_WRITE_ARGUMENTS
: '(' UNIT ')' IO_ELEMENTS { ($2, Nothing, Just (aReverse $4)) }
| '(' UNIT ',' FORM ')' IO_ELEMENTS { ($2, Just $4, Just (aReverse $6)) }
| '(' UNIT ')' { ($2, Nothing, Nothing) }
| '(' UNIT ',' FORM ')' { ($2, Just $4, Nothing) }

-- Not my terminology a VAR or an INT (probably positive) is defined as UNIT.
UNIT :: { Expression A0 } : INTEGER_LITERAL { $1 } | VARIABLE { $1 }

FORM :: { Expression A0 } : ARRAY { $1 } | LABEL_IN_STATEMENT { $1 }

IO_ELEMENTS :: { AList (IOElement A0) A0 }
IO_ELEMENTS
: IO_ELEMENTS ',' IO_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| IO_ELEMENT { AList () (getSpan $1) [ $1 ] }

IO_ELEMENT :: { IOElement A0 }
IO_ELEMENT
: VARIABLE { IOExpression $1 }
-- There should also be a caluse for variable names but not way to 
-- differentiate it at this stage from VARIABLE. Hence, it is omitted to prevent
-- reduce/reduce conflict.
| SUBSCRIPT { IOExpression $1 }
-- TODO after handling blocks |  '(' IO_ELEMENTS ',' DO_SPEC ')' {% getSrcSpan $1 >>= \s -> return $ IOTuple () s $3 $5 }
| '(' ELEMENTS ')' { IOExpressionList () (getTransSpan $1 $3) $ aReverse $2 } 

ELEMENTS :: { AList (Expression A0) A0 }
ELEMENTS
: ELEMENTS ',' ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| ELEMENT { AList () (getSpan $1) [ $1 ] }

ELEMENT :: { Expression A0 }
ELEMENT
: VARIABLE { $1 }
| SUBSCRIPT { $1 }

FORMAT_ITEMS :: { AList (FormatItem A0) A0 }
FORMAT_ITEMS
: FORMAT_ITEMS ',' FORMAT_ITEM { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| FORMAT_ITEMS ',' FORMAT_ITEM_DELIMETER { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| FORMAT_ITEMS FORMAT_ITEM { setSpan (getTransSpan $1 $2) $ $2 `aCons` $1 }
| FORMAT_ITEMS FORMAT_ITEM_DELIMETER { setSpan (getTransSpan $1 $2) $ $2 `aCons` $1 }
| '(' { AList () (getSpan $1) [ ] }

FORMAT_ITEM_DELIMETER :: { FormatItem A0 }
FORMAT_ITEM_DELIMETER: '/' { FIDelimiter () (getSpan $1) }

FORMAT_ITEM :: { FormatItem A0 }
FORMAT_ITEM
: int FORMAT_ITEMS ')' { FIFormatList () (getTransSpan $1 $3) (let (TInt _ s) = $1 in Just s) (aReverse $2) }
| FORMAT_ITEMS ')' { FIFormatList () (getTransSpan $1 $2) Nothing (aReverse $1) }
| HOLLERITH { let (ExpValue _ s val) = $1 in FIHollerith () s val }
| fieldDescriptorDEFG { let (TFieldDescriptorDEFG s a b c d) = $1 in FIFieldDescriptorDEFG () s a b c d }
| fieldDescriptorAIL { let (TFieldDescriptorAIL s a b c) = $1 in FIFieldDescriptorAIL () s a b c }
| blankDescriptor { let (TBlankDescriptor s w) = $1 in FIBlankDescriptor () s w }
| scaleFactor { let (TScaleFactor s sf) = $1 in FIScaleFactor () s sf }

DATA_GROUPS :: { AList (DataGroup A0) A0 }
DATA_GROUPS
: DATA_GROUPS ',' DECLARATORS  '/' DATA_ITEMS '/' { setSpan (getTransSpan $1 $6) $ (DataGroup () (getTransSpan $3 $6) (aReverse $3) (aReverse $5)) `aCons` $1 }
| DECLARATORS  '/' DATA_ITEMS '/' { AList () (getTransSpan $1 $4) [ DataGroup () (getTransSpan $1 $4) (aReverse $1) (aReverse $3) ] }

DATA_ITEMS :: { AList (Expression A0) A0 }
DATA_ITEMS
: DATA_ITEMS ',' DATA_ITEM { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| DATA_ITEM { AList () (getSpan $1) [ $1 ] }

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

EQUIVALENCE_GROUPS :: { AList (AList (Expression A0) A0) A0 }
EQUIVALENCE_GROUPS
: EQUIVALENCE_GROUPS ','  '(' DECLARATORS ')' { setSpan (getTransSpan $1 $5) $ (setSpan (getTransSpan $3 $5) $ aReverse $4) `aCons` $1 }
| '(' DECLARATORS ')' { let s = (getTransSpan $1 $3) in AList () s [ setSpan s $ aReverse $2 ] }

DECLARATORS :: { AList (Expression A0) A0 }
DECLARATORS
: DECLARATORS ',' DECLARATOR { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DECLARATOR { AList () (getSpan $1) [ $1 ] }

COMMON_GROUPS :: { AList (CommonGroup A0) A0 }
COMMON_GROUPS
: COMMON_GROUPS COMMON_GROUP { setSpan (getTransSpan $1 $2) $ $2 `aCons` $1 }
| INIT_COMMON_GROUP { AList () (getSpan $1) [ $1 ] }

COMMON_GROUP :: { CommonGroup A0 }
COMMON_GROUP
: '/' id '/' COMMON_ELEMENTS { CommonGroup () (getTransSpan $1 $4) (let (TId _ s) = $2 in Just s) $ aReverse $4 }
| '/' '/' COMMON_ELEMENTS { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }

INIT_COMMON_GROUP :: { CommonGroup A0 }
INIT_COMMON_GROUP
: '/' id '/' COMMON_ELEMENTS { CommonGroup () (getTransSpan $1 $4) (let (TId _ s) = $2 in Just s) $ aReverse $4 }
| '/' '/' COMMON_ELEMENTS { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }
| COMMON_ELEMENTS { CommonGroup () (getSpan $1) Nothing $ aReverse $1 }

COMMON_ELEMENTS :: { AList (Expression A0) A0 }
COMMON_ELEMENTS
: COMMON_ELEMENTS ',' DECLARATOR { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DECLARATOR  { AList () (getSpan $1) [ $1 ] }

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
ARRAY_DECLARATORS :: { AList (Expression A0) A0 }
ARRAY_DECLARATORS
: ARRAY_DECLARATORS ',' SUBSCRIPT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| SUBSCRIPT { AList () (getSpan $1) [ $1 ] }

-- Here the procedure should be either a function or subroutine name, but 
-- since they are syntactically identical at this stage subroutine names
-- are also emitted as function names.
PROCEDURES :: { AList (Expression A0) A0 }
PROCEDURES
: PROCEDURES ',' FUNCTION_NAME { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| FUNCTION_NAME { AList () (getSpan $1) [ $1 ] }

CALLABLE_EXPRESSIONS :: { AList (Expression A0) A0 }
CALLABLE_EXPRESSIONS
:  CALLABLE_EXPRESSIONS_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

CALLABLE_EXPRESSIONS_LEVEL1 :: { AList (Expression A0) A0 }
CALLABLE_EXPRESSIONS_LEVEL1
: CALLABLE_EXPRESSIONS_LEVEL1 ',' CALLABLE_EXPRESSION { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' CALLABLE_EXPRESSION { AList () (getTransSpan $1 $2) [ $2 ] } 
| '(' { AList () (getSpan $1) [ ] }

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
| ARITHMETIC_SIGN ARITHMETIC_EXPRESSION %prec NEGATION { ExpUnary () (let (SrcSpan p1 _) = (fst $1); (SrcSpan _ p2) = getSpan $2 in SrcSpan p1 p2) (snd $1) $2 }
| '(' ARITHMETIC_EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
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
| not LOGICAL_EXPRESSION { ExpUnary () (getTransSpan $1 $2) Not $2 }
| '(' LOGICAL_EXPRESSION ')'  { setSpan (getTransSpan $1 $3) $2 }
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
:  '(' INDICIES_LEVEL1 { setSpan (getTransSpan $1 $2) $2 }

INDICIES_LEVEL1 :: { AList (Expression A0) A0  }
INDICIES_LEVEL1
: INDEX ',' INDICIES_LEVEL1 { setSpan (getTransSpan $1 $3) $ $1 `aCons` $3 }
| INDEX ')' { AList () (getTransSpan $1 $2) [ $1 ] }
| ')' { AList () (getSpan $1) [ ] }

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

ARITHMETIC_SIGN :: { (SrcSpan, UnaryOp) }
ARITHMETIC_SIGN
: '-' { (getSpan $1, Minus) }
| '+' { (getSpan $1, Plus) }

-- This may also be used to parse a function name, or an array name. Since when
-- are valid options in a production there is no way of differentiating them at
-- this stage.
-- This at least reduces reduce/reduce conflicts.
VARIABLE :: { Expression A0 }
VARIABLE
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValVariable s }

ARRAY :: { Expression A0 }
ARRAY : id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValArray s }

FUNCTION_NAME :: { Expression A0 }
FUNCTION_NAME
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValFunctionName s }

SUBROUTINE_NAME :: { Expression A0 }
SUBROUTINE_NAME
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValSubroutineName s }

SIGNED_INTEGER_LITERAL :: { Expression A0 }
SIGNED_INTEGER_LITERAL
: ARITHMETIC_SIGN INTEGER_LITERAL { ExpUnary () (let (SrcSpan p1 _) = fst $1; (SrcSpan _ p2) = getSpan $2 in SrcSpan p1 p2) (snd $1) $2 }
| INTEGER_LITERAL { $1 }

INTEGER_LITERAL :: { Expression A0 } : int { ExpValue () (getSpan $1) $ let (TInt _ i) = $1 in ValInteger i }

SIGNED_REAL_LITERAL :: { Expression A0 }
SIGNED_REAL_LITERAL
: ARITHMETIC_SIGN REAL_LITERAL { ExpUnary () (let (SrcSpan p1 _) = (fst $1); (SrcSpan _ p2) = (getSpan $2) in SrcSpan p1 p2) (snd $1) $2 }
| REAL_LITERAL { $1 }

REAL_LITERAL :: { Expression A0 } : real { ExpValue () (getSpan $1) $ let (TReal _ r) = $1 in (ValReal r) }

SIGNED_NUMERIC_LITERAL :: { Expression A0 }
SIGNED_NUMERIC_LIETERAL
: SIGNED_INTEGER_LITERAL { $1 }
| SIGNED_REAL_LITERAL    { $1 }

COMPLEX_LITERAL :: { Expression A0 }
COMPLEX_LITERAL
:  '(' SIGNED_NUMERIC_LITERAL ',' SIGNED_NUMERIC_LITERAL ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4)}

LOGICAL_LITERAL :: { Expression A0 }
LOGICAL_LITERAL
:  true   { ExpValue () (getSpan $1) ValTrue }
|  false  { ExpValue () (getSpan $1) ValFalse }

HOLLERITH :: { Expression A0 } : hollerith { ExpValue () (getSpan $1) $ let (THollerith _ h) = $1 in ValHollerith h }

LABELS_IN_STATEMENT :: { AList (Expression A0) A0 }
LABELS_IN_STATEMENT
: LABELS_IN_STATEMENT_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

LABELS_IN_STATEMENT_LEVEL1 :: { AList (Expression A0) A0 }
LABELS_IN_STATEMENT_LEVEL1
: LABELS_IN_STATEMENT_LEVEL1 ',' LABEL_IN_STATEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' LABEL_IN_STATEMENT { AList () (getTransSpan $1 $2) [ $2 ] }

-- Labels that occur in the first 6 columns
LABEL_IN_6COLUMN :: { Expression A0 } : label { ExpValue () (getSpan $1) (let (TLabel _ l) = $1 in ValLabel l) }

-- Labels that occur in statements
LABEL_IN_STATEMENT :: { Expression A0 } : int { ExpValue () (getSpan $1) (let (TInt _ l) = $1 in ValLabel l) }

{

type A0 = ()

fortran66Parser :: String -> String -> [ ProgramUnit A0 ]
fortran66Parser sourceCode filename = 
  evalParse programParser $ initParseState sourceCode Fortran66 filename

parseError :: Token -> Parse AlexInput a
parseError _ = fail "Couldn't parse."

}
