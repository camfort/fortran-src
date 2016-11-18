-- -*- Mode: Haskell -*-
{
module Language.Fortran.Parser.Fortran77 ( expressionParser
                               , statementParser
                               , fortran77Parser
                               , extended77Parser
                               , fortran77ParserWithModFiles
                               , extended77ParserWithModFiles
                               ) where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST

import Control.Monad.State
import Data.Maybe (isNothing, fromJust)
import qualified Data.ByteString.Char8 as B
import Language.Fortran.Util.Position
import Language.Fortran.Util.ModFile
import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FixedForm
import Language.Fortran.Transformer
import Language.Fortran.AST

import Debug.Trace

}

%name programParser PROGRAM
%name statementParser STATEMENT
%name expressionParser EXPRESSION
%monad { LexAction }
%lexer { lexer } { TEOF _ }
%tokentype { Token }
%error { parseError }

%token
  '('                   { TLeftPar _ }
  ')'                   { TRightPar _ }
  '(/'                  { TLeftArrayPar _ }
  '/)'                  { TRightArrayPar _ }
  ','                   { TComma _ }
  '.'                   { TDot _ }
  ':'                   { TColon _ }
  include               { TInclude _ }
  program               { TProgram _ }
  function              { TFunction _ }
  subroutine            { TSubroutine _ }
  blockData             { TBlockData _ }
  end                   { TEnd _ }
  '='                   { TOpAssign _ }
  assign                { TAssign _ }
  to                    { TTo _ }
  goto                  { TGoto _ }
  if                    { TIf _ }
  then                  { TThen _ }
  else                  { TElse _ }
  elsif                 { TElsif _ }
  endif                 { TEndif _ }
  call                  { TCall _ }
  return                { TReturn _ }
  save                  { TSave _ }
  continue              { TContinue _ }
  stop                  { TStop _ }
  exit                  { TExit _ }
  pause                 { TPause _ }
  do                    { TDo _ }
  doWhile               { TDoWhile _ }
  enddo                 { TEndDo _ }
  read                  { TRead _ }
  write                 { TWrite _ }
  print                 { TPrint _ }
  open                  { TOpen _ }
  close                 { TClose _ }
  inquire               { TInquire _ }
  rewind                { TRewind _ }
  backspace             { TBackspace _ }
  endfile               { TEndfile _ }
  common                { TCommon _ }
  equivalence           { TEquivalence _ }
  external              { TExternal _ }
  dimension             { TDimension _ }
  character             { TType _ "character" }
  integer               { TType _ "integer" }
  real                  { TType _ "real" }
  doublePrecision       { TType _ "doubleprecision" }
  logical               { TType _ "logical" }
  complex               { TType _ "complex" }
  doubleComplex         { TType _ "doublecomplex" }
  intrinsic             { TIntrinsic _ }
  implicit              { TImplicit _ }
  parameter             { TParameter _ }
  entry                 { TEntry _ }
  none                  { TNone _ }
  data                  { TData _ }
  format                { TFormat _ }
  blob                  { TBlob _ _ }
  int                   { TInt _ _ }
  exponent              { TExponent _ _ }
  bool                  { TBool _ _ }
  '+'                   { TOpPlus _ }
  '-'                   { TOpMinus _ }
  '**'                  { TOpExp _ }
  '*'                   { TStar _ }
  '/'                   { TSlash _ }
  eqv                   { TOpEquivalent _ }
  neqv                  { TOpNotEquivalent _ }
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
  string                { TString _ _ }
  label                 { TLabel _ _ }
  newline               { TNewline _ }

%left eqv neqv
%left or
%left and
%right not

%nonassoc '>' '<' '>=' '<=' '==' '!='
%nonassoc RELATIONAL

%left CONCAT

%left '+' '-'
%left '*' '/'
%right NEGATION
%right '**'

%%

-- This rule is to ignore leading whitespace
PROGRAM :: { ProgramFile A0 }
PROGRAM
: NEWLINE PROGRAM_INNER { $2 }
| PROGRAM_INNER { $1 }

PROGRAM_INNER :: { ProgramFile A0 }
PROGRAM_INNER
: PROGRAM_UNITS { ProgramFile (MetaInfo { miVersion = Fortran77 }) (reverse $1) [ ] }
| PROGRAM_UNITS COMMENT_BLOCKS { ProgramFile (MetaInfo { miVersion = Fortran77 }) (reverse $1) (reverse $2) }

PROGRAM_UNITS :: { [ ([ Block A0 ], ProgramUnit A0) ] }
PROGRAM_UNITS
: PROGRAM_UNITS PROGRAM_UNIT MAYBE_NEWLINE { ([ ], $2) : $1 }
| PROGRAM_UNITS COMMENT_BLOCKS PROGRAM_UNIT MAYBE_NEWLINE { (reverse $2, $3) : $1 }
| PROGRAM_UNIT MAYBE_NEWLINE { [ ([ ], $1) ] }
| COMMENT_BLOCKS PROGRAM_UNIT MAYBE_NEWLINE { [ (reverse $1, $2) ] }

PROGRAM_UNIT :: { ProgramUnit A0 }
PROGRAM_UNIT
: program NAME NEWLINE BLOCKS end { PUMain () (getTransSpan $1 $5) (Just $2) (reverse $4) Nothing }
| TYPE_SPEC function NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end
  { PUFunction () (getTransSpan $1 $7) (Just $1) False $3 $4 Nothing (reverse $6) Nothing }
| function NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end
  { PUFunction () (getTransSpan $1 $6) Nothing False $2 $3 Nothing (reverse $5) Nothing }
| subroutine NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end
  { PUSubroutine () (getTransSpan $1 $6) False $2 $3 (reverse $5) Nothing }
| blockData NEWLINE BLOCKS end { PUBlockData () (getTransSpan $1 $4) Nothing (reverse $3) }
| blockData NAME NEWLINE BLOCKS end { PUBlockData () (getTransSpan $1 $5) (Just $2) (reverse $4) }

MAYBE_ARGUMENTS :: { Maybe (AList Expression A0) }
: '(' MAYBE_VARIABLES ')' { $2 }
| {- Nothing -} { Nothing }

NAME :: { Name } : id { let (TId _ name) = $1 in name }

BLOCKS :: { [ Block A0 ] }
BLOCKS
: BLOCKS BLOCK { $2 : $1 }
| {- EMPTY -} { [ ] }

BLOCK :: { Block A0 }
BLOCK
: LABEL_IN_6COLUMN STATEMENT NEWLINE { BlStatement () (getTransSpan $1 $2) (Just $1) $2 }
| STATEMENT NEWLINE { BlStatement () (getSpan $1) Nothing $1 }
| COMMENT_BLOCK { $1 }

COMMENT_BLOCKS :: { [ Block A0 ] }
COMMENT_BLOCKS
: COMMENT_BLOCKS COMMENT_BLOCK { $2 : $1 }
| COMMENT_BLOCK { [ $1 ] }

COMMENT_BLOCK :: { Block A0 }
COMMENT_BLOCK
: comment NEWLINE { let (TComment s c) = $1 in BlComment () s c }

MAYBE_NEWLINE :: { Maybe Token } : NEWLINE { Just $1 } | {- EMPTY -} { Nothing }

NEWLINE :: { Token }
NEWLINE
: NEWLINE newline { $1 }
| newline { $1 }

STATEMENT :: { Statement A0 }
STATEMENT
: LOGICAL_IF_STATEMENT { $1 }
| DO_STATEMENT { $1 }
| EXECUTABLE_STATEMENT { $1 }
| NONEXECUTABLE_STATEMENT { $1 }

LOGICAL_IF_STATEMENT :: { Statement A0 }
LOGICAL_IF_STATEMENT : if '(' EXPRESSION ')' EXECUTABLE_STATEMENT { StIfLogical () (getTransSpan $1 $5) $3 $5 }

DO_STATEMENT :: { Statement A0 }
DO_STATEMENT
: do LABEL_IN_STATEMENT DO_SPECIFICATION { StDo () (getTransSpan $1 $3) Nothing (Just $2) (Just $3) }
| do LABEL_IN_STATEMENT ',' DO_SPECIFICATION { StDo () (getTransSpan $1 $4) Nothing (Just $2) (Just $4) }
| do DO_SPECIFICATION { StDo () (getTransSpan $1 $2) Nothing Nothing (Just $2) }

DO_SPECIFICATION :: { DoSpecification A0 }
DO_SPECIFICATION
: EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION ',' EXPRESSION { DoSpecification () (getTransSpan $1 $5) $1 $3 (Just $5) }
| EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION                { DoSpecification () (getTransSpan $1 $3) $1 $3 Nothing }

EXECUTABLE_STATEMENT :: { Statement A0 }
EXECUTABLE_STATEMENT
: EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| assign LABEL_IN_STATEMENT to VARIABLE { StLabelAssign () (getTransSpan $1 $4) $2 $4 }
| GOTO_STATEMENT { $1 }
| if '(' EXPRESSION ')' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT { StIfArithmetic () (getTransSpan $1 $9) $3 $5 $7 $9 }
| if '(' EXPRESSION ')' then { StIfThen () (getTransSpan $1 $5) Nothing $3 }
| elsif '(' EXPRESSION ')' then { StElsif () (getTransSpan $1 $5) Nothing $3 }
| else { StElse () (getSpan $1) Nothing }
| endif { StEndif () (getSpan $1) Nothing }
| doWhile '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $4) Nothing Nothing $3 }
| enddo { StEnddo () (getSpan $1) Nothing }
| call VARIABLE ARGUMENTS
  { StCall () (getTransSpan $1 $3) $2 $ Just $3 }
| call VARIABLE { StCall () (getTransSpan $1 $2) $2 Nothing }
| return { StReturn () (getSpan $1) Nothing }
| return EXPRESSION { StReturn () (getTransSpan $1 $2) $ Just $2 }
| save SAVE_ARGS { StSave () (getSpan ($1, $2)) $2 }
| continue { StContinue () $ getSpan $1 }
| stop INTEGER_OR_STRING { StStop () (getTransSpan $1 $2) $ Just $2 }
| stop { StStop () (getSpan $1) Nothing }
| exit { StExit () (getSpan $1) Nothing }
| pause INTEGER_OR_STRING { StPause () (getTransSpan $1 $2) $ Just $2 }
| pause { StPause () (getSpan $1) Nothing }
-- IO Statements
| read CILIST IN_IOLIST { StRead () (getTransSpan $1 $3) $2 (Just $ aReverse $3) }
| read CILIST { StRead () (getTransSpan $1 $2) $2 Nothing }
| read FORMAT_ID ',' IN_IOLIST { StRead2 () (getTransSpan $1 $4) $2 (Just $ aReverse $4) }
| read FORMAT_ID { StRead2 () (getTransSpan $1 $2) $2 Nothing }
| write CILIST OUT_IOLIST { StWrite () (getTransSpan $1 $3) $2 (Just $ aReverse $3) }
| write CILIST { StWrite () (getTransSpan $1 $2) $2 Nothing }
| print FORMAT_ID ',' OUT_IOLIST { StPrint () (getTransSpan $1 $4) $2 (Just $ aReverse $4) }
| print FORMAT_ID { StPrint () (getTransSpan $1 $2) $2 Nothing }
| open CILIST { StOpen () (getTransSpan $1 $2) $2 }
| close CILIST { StClose () (getTransSpan $1 $2) $2 }
| inquire CILIST { StInquire () (getTransSpan $1 $2) $2 }
| rewind CILIST { StRewind () (getTransSpan $1 $2) $2 }
| rewind UNIT { StRewind2 () (getTransSpan $1 $2) $2 }
| endfile CILIST { StEndfile () (getTransSpan $1 $2) $2 }
| endfile UNIT { StEndfile2 () (getTransSpan $1 $2) $2 }
| backspace CILIST { StBackspace () (getTransSpan $1 $2) $2 }
| backspace UNIT { StBackspace2 () (getTransSpan $1 $2) $2 }

FORMAT_ID :: { Expression A0 }
FORMAT_ID
: FORMAT_ID '/' '/' FORMAT_ID %prec CONCAT { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| INTEGER_LITERAL               { $1 }
| STRING                        { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| SUBSCRIPT                     { $1 }
| VARIABLE                      { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

UNIT :: { Expression A0 }
UNIT
: INTEGER_LITERAL { $1 }
| VARIABLE { $1 }
| SUBSCRIPT { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

-- A crude approximation that makes parsing easy. Individual key value pairs
-- should be checket later on.
CILIST :: { AList ControlPair A0 }
CILIST
: '(' UNIT ',' FORMAT_ID ',' CILIST_PAIRS ')' {
  let { cp1 = ControlPair () (getSpan $2) Nothing $2;
        cp2 = ControlPair () (getSpan $4) Nothing $4 }
  in setSpan (getTransSpan $1 $7) $ cp1 `aCons` cp2 `aCons` aReverse $6
  }
| '(' UNIT ',' FORMAT_ID ')' {
  let { cp1 = ControlPair () (getSpan $2) Nothing $2;
        cp2 = ControlPair () (getSpan $4) Nothing $4 }
        in AList () (getTransSpan $1 $5) [ cp1,  cp2 ]
        }
| '(' UNIT ',' CILIST_PAIRS ')' {
  let cp1 = ControlPair () (getSpan $2) Nothing $2
        in setSpan (getTransSpan $1 $5) $ cp1 `aCons` aReverse $4
        }
| '(' UNIT ')' {
  let cp1 = ControlPair () (getSpan $2) Nothing $2
  in AList () (getTransSpan $1 $3) [ cp1 ]
  }
| '(' CILIST_PAIRS ')' { setSpan (getTransSpan $1 $3) $ aReverse $2 }

CILIST_PAIRS :: { AList ControlPair A0 }
CILIST_PAIRS
: CILIST_PAIRS ',' CILIST_PAIR { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| CILIST_PAIR { AList () (getSpan $1) [ $1 ] }

CILIST_PAIR :: { ControlPair A0 }
CILIST_PAIR : id '=' CILIST_ELEMENT { let (TId s id) = $1 in ControlPair () (getTransSpan s $3) (Just id) $3 }

CILIST_ELEMENT :: { Expression A0 }
CILIST_ELEMENT
: CI_EXPRESSION { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

CI_EXPRESSION :: { Expression A0 }
CI_EXPRESSION
: CI_EXPRESSION '+' CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| CI_EXPRESSION '-' CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| CI_EXPRESSION '*' CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| CI_EXPRESSION '/' CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| CI_EXPRESSION '**' CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| CI_EXPRESSION '/' '/' CI_EXPRESSION %prec CONCAT { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| ARITHMETIC_SIGN CI_EXPRESSION %prec NEGATION { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| CI_EXPRESSION or CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| CI_EXPRESSION and CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not CI_EXPRESSION { ExpUnary () (getTransSpan $1 $2) Not $2 }
| CI_EXPRESSION eqv CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Equivalent $1 $3 }
| CI_EXPRESSION neqv CI_EXPRESSION { ExpBinary () (getTransSpan $1 $3) NotEquivalent $1 $3 }
| CI_EXPRESSION RELATIONAL_OPERATOR CI_EXPRESSION %prec RELATIONAL { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| '(' CI_EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| INTEGER_LITERAL               { $1 }
| LOGICAL_LITERAL               { $1 }
| STRING                        { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| SUBSCRIPT                     { $1 }
| VARIABLE                      { $1 }

-- Input IOList used in read like statements is much more restrictive as it
-- doesn't make sense to read into an integer.
-- While the output list can be an arbitrary expression. Hence, the grammar
-- rule separation.

IN_IOLIST :: { AList Expression A0 }
IN_IOLIST
: IN_IOLIST ',' IN_IO_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| IN_IO_ELEMENT { AList () (getSpan $1) [ $1 ] }

IN_IO_ELEMENT :: { Expression A0 }
IN_IO_ELEMENT
: VARIABLE { $1 }
| SUBSCRIPT { $1 }
| '(' IN_IOLIST ',' DO_SPECIFICATION ')' { ExpImpliedDo () (getTransSpan $1 $5) (aReverse $2) $4 }

OUT_IOLIST :: { AList Expression A0 }
OUT_IOLIST
: OUT_IOLIST ',' EXPRESSION { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| EXPRESSION { AList () (getSpan $1) [ $1 ] }

SAVE_ARGS :: { Maybe (AList Expression A0) }
SAVE_ARGS
: SAVE_ARGS_LEVEL1 { Just $ fromReverseList $1 }
| {-EMPTY-} { Nothing }

SAVE_ARGS_LEVEL1 :: { [ Expression A0 ] }
SAVE_ARGS_LEVEL1
: SAVE_ARGS_LEVEL1 ',' SAVE_ARG { $3 : $1 }
| SAVE_ARG { [ $1 ] }

SAVE_ARG :: { Expression A0 }
SAVE_ARG : COMMON_NAME { $1 } | VARIABLE { $1 }

INTEGER_OR_STRING :: { Expression A0 } : STRING { $1 } | INTEGER_LITERAL { $1 }

GOTO_STATEMENT :: { Statement A0 }
GOTO_STATEMENT
: goto LABEL_IN_STATEMENT { StGotoUnconditional () (getTransSpan $1 $2) $2 }
| goto VARIABLE LABELS_IN_STATEMENT { StGotoAssigned () (getTransSpan $1 $3) $2 $3 }
| goto VARIABLE ',' LABELS_IN_STATEMENT { StGotoAssigned () (getTransSpan $1 $4) $2 $4 }
| goto LABELS_IN_STATEMENT EXPRESSION { StGotoComputed () (getTransSpan $1 $3) $2 $3 }
| goto LABELS_IN_STATEMENT ',' EXPRESSION { StGotoComputed () (getTransSpan $1 $4) $2 $4 }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
EXPRESSION_ASSIGNMENT_STATEMENT : ELEMENT '=' EXPRESSION { StExpressionAssign () (getTransSpan $1 $3) $1 $3 }

NONEXECUTABLE_STATEMENT :: { Statement A0 }
NONEXECUTABLE_STATEMENT
: external FUNCTION_NAMES { StExternal () (getTransSpan $1 $2) (aReverse $2) }
| intrinsic FUNCTION_NAMES { StIntrinsic () (getTransSpan $1 $2) (aReverse $2) }
| dimension ARRAY_DECLARATORS { StDimension () (getTransSpan $1 $2) (aReverse $2) }
| common COMMON_GROUPS { StCommon () (getTransSpan $1 $2) (aReverse $2) }
| equivalence EQUIVALENCE_GROUPS { StEquivalence () (getTransSpan $1 $2) (aReverse $2) }
| data DATA_GROUPS { StData () (getTransSpan $1 $2) (aReverse $2) }
-- Following is a fake node to make arbitrary FORMAT statements parsable.
-- Must be fixed in the future. TODO
| format blob
  { let TBlob s blob = $2 in StFormatBogus () (getTransSpan $1 s) blob }
| DECLARATION_STATEMENT { $1 }
| implicit none { StImplicit () (getTransSpan $1 $2) Nothing }
| implicit IMP_LISTS { StImplicit () (getTransSpan $1 $2) $ Just $ aReverse $2 }
| parameter '(' PARAMETER_ASSIGNMENTS ')'
  { StParameter () (getTransSpan $1 $4) $ fromReverseList $3 }
| entry VARIABLE { StEntry () (getTransSpan $1 $2) $2 Nothing Nothing }
| entry VARIABLE ENTRY_ARGS { StEntry () (getTransSpan $1 $3) $2 (Just $3) Nothing }
| include STRING { StInclude () (getTransSpan $1 $2) $2 }

ENTRY_ARGS :: { AList Expression A0 }
ENTRY_ARGS
: ENTRY_ARGS_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

ENTRY_ARGS_LEVEL1 :: { AList Expression A0 }
ENTRY_ARGS_LEVEL1
: ENTRY_ARGS_LEVEL1 ',' ENTRY_ARG { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' ENTRY_ARG { AList () (getTransSpan $1 $2) [ $2 ] }
| '(' { AList () (getSpan $1) [ ] }

ENTRY_ARG :: { Expression A0 }
ENTRY_ARG
: VARIABLE { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

PARAMETER_ASSIGNMENTS :: { [ Declarator A0 ] }
PARAMETER_ASSIGNMENTS
: PARAMETER_ASSIGNMENTS ',' PARAMETER_ASSIGNMENT { $3 : $1 }
| PARAMETER_ASSIGNMENT { [ $1 ] }

PARAMETER_ASSIGNMENT :: { Declarator A0 }
PARAMETER_ASSIGNMENT
: VARIABLE '=' CONSTANT_EXPRESSION
  { DeclVariable () (getTransSpan $1 $3) $1 Nothing (Just $3) }

DECLARATION_STATEMENT :: { Statement A0 }
DECLARATION_STATEMENT
: TYPE_SPEC DECLARATORS { StDeclaration () (getTransSpan $1 $2) $1 Nothing $2 }

IMP_LISTS :: { AList ImpList A0 }
IMP_LISTS
: IMP_LISTS ',' IMP_LIST { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| IMP_LIST { AList () (getSpan $1) [ $1 ] }

IMP_LIST :: { ImpList A0 }
IMP_LIST
: IMP_TYPE_SPEC '(' IMP_ELEMENTS ')'
  { ImpList () (getTransSpan $1 $4) $1 $ aReverse $3 }

IMP_ELEMENTS :: { AList ImpElement A0 }
IMP_ELEMENTS
: IMP_ELEMENTS ',' IMP_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| IMP_ELEMENT { AList () (getSpan $1) [ $1 ] }

IMP_ELEMENT :: { ImpElement A0 }
IMP_ELEMENT
: id {% do
      let (TId s id) = $1
      if length id /= 1
      then fail "Implicit argument must be a character."
      else return $ ImpCharacter () s id
     }
| id '-' id {% do
             let (TId _ id1) = $1
             let (TId _ id2) = $3
             if length id1 /= 1 || length id2 /= 1
             then fail "Implicit argument must be a character."
             else return $ ImpRange () (getTransSpan $1 $3) id1 id2
             }

ELEMENT :: { Expression A0 }
ELEMENT
: VARIABLE { $1 }
| SUBSCRIPT { $1 }

DATA_GROUPS :: { AList DataGroup A0 }
DATA_GROUPS
: DATA_GROUPS ',' NAME_LIST  '/' DATA_ITEMS '/' { setSpan (getTransSpan $1 $6) $ (DataGroup () (getTransSpan $3 $6) (aReverse $3) (aReverse $5)) `aCons` $1 }
| NAME_LIST  '/' DATA_ITEMS '/' { AList () (getTransSpan $1 $4) [ DataGroup () (getTransSpan $1 $4) (aReverse $1) (aReverse $3) ] }

DATA_ITEMS :: { AList Expression A0 }
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
--| COMPLEX_LITERAL         { $1 }
| VARIABLE                { $1 }
| '(' SIGNED_NUMERIC_LITERAL ',' SIGNED_NUMERIC_LITERAL ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4)}
| LOGICAL_LITERAL         { $1 }
| STRING                  { $1 }

EQUIVALENCE_GROUPS :: { AList (AList Expression) A0 }
EQUIVALENCE_GROUPS
: EQUIVALENCE_GROUPS ','  '(' NAME_LIST ')' { setSpan (getTransSpan $1 $5) $ (setSpan (getTransSpan $3 $5) $ aReverse $4) `aCons` $1 }
| '(' NAME_LIST ')' { let s = (getTransSpan $1 $3) in AList () s [ setSpan s $ aReverse $2 ] }

COMMON_GROUPS :: { AList CommonGroup A0 }
COMMON_GROUPS
: COMMON_GROUPS COMMON_GROUP { setSpan (getTransSpan $1 $2) $ $2 `aCons` $1 }
| INIT_COMMON_GROUP { AList () (getSpan $1) [ $1 ] }

COMMON_GROUP :: { CommonGroup A0 }
COMMON_GROUP
: COMMON_NAME NAME_LIST { CommonGroup () (getTransSpan $1 $2) (Just $1) $ aReverse $2 }
| '/' '/' NAME_LIST { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }

INIT_COMMON_GROUP :: { CommonGroup A0 }
INIT_COMMON_GROUP
: COMMON_NAME NAME_LIST { CommonGroup () (getTransSpan $1 $2) (Just $1) $ aReverse $2 }
| '/' '/' NAME_LIST { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }
| NAME_LIST { CommonGroup () (getSpan $1) Nothing $ aReverse $1 }

COMMON_NAME :: { Expression A0 }
COMMON_NAME : '/' VARIABLE '/' { setSpan (getTransSpan $1 $3) $2 }

NAME_LIST :: { AList Expression A0 }
NAME_LIST
: NAME_LIST ',' ELEMENT
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| ELEMENT { AList () (getSpan $1) [ $1 ] }

DECLARATORS :: { AList Declarator A0 }
DECLARATORS
: DECLARATORS ',' DECLARATOR { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DECLARATOR { AList () (getSpan $1) [ $1 ] }

DECLARATOR :: { Declarator A0 }
DECLARATOR
: ARRAY_DECLARATOR { $1 }
| VARIABLE_DECLARATOR { $1 }

ARRAY_DECLARATORS :: { AList Declarator A0 }
ARRAY_DECLARATORS
: ARRAY_DECLARATORS ',' ARRAY_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| ARRAY_DECLARATOR { AList () (getSpan $1) [ $1 ] }

ARRAY_DECLARATOR :: { Declarator A0 }
ARRAY_DECLARATOR
: VARIABLE '(' DIMENSION_DECLARATORS ')'
  { DeclArray () (getTransSpan $1 $4) $1 (aReverse $3) Nothing Nothing }

VARIABLE_DECLARATOR :: { Declarator A0 }
VARIABLE_DECLARATOR
: VARIABLE { DeclVariable () (getSpan $1) $1 Nothing Nothing }
| VARIABLE '*' EXPRESSION
  { DeclVariable () (getTransSpan $1 $3) $1 (Just $3) Nothing }

DIMENSION_DECLARATORS :: { AList DimensionDeclarator A0 }
DIMENSION_DECLARATORS
: DIMENSION_DECLARATORS ',' DIMENSION_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DIMENSION_DECLARATOR { AList () (getSpan $1) [ $1 ] }

DIMENSION_DECLARATOR :: { DimensionDeclarator A0 }
DIMENSION_DECLARATOR
: EXPRESSION ':' EXPRESSION { DimensionDeclarator () (getTransSpan $1 $3) (Just $1) (Just $3) }
| EXPRESSION { DimensionDeclarator () (getSpan $1) Nothing (Just $1) }
| EXPRESSION ':' '*' { DimensionDeclarator () (getTransSpan $1 $3) (Just $1) (Just $ ExpValue () (getSpan $3) ValStar) }
| '*' { DimensionDeclarator () (getSpan $1) Nothing (Just $ ExpValue () (getSpan $1) ValStar) }

-- Here the procedure should be either a function or subroutine name, but
-- since they are syntactically identical at this stage subroutine names
-- are also emitted as function names.
FUNCTION_NAMES :: { AList Expression A0 }
FUNCTION_NAMES
: FUNCTION_NAMES ',' VARIABLE { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| VARIABLE { AList () (getSpan $1) [ $1 ] }

ARGUMENTS :: { AList Argument A0 }
ARGUMENTS
: ARGUMENTS_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

ARGUMENTS_LEVEL1 :: { AList Argument A0 }
ARGUMENTS_LEVEL1
: ARGUMENTS_LEVEL1 ',' CALLABLE_EXPRESSION { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' CALLABLE_EXPRESSION { AList () (getTransSpan $1 $2) [ $2 ] }
| '(' { AList () (getSpan $1) [ ] }

-- Expression all by itself subsumes all other callable expressions.
CALLABLE_EXPRESSION :: { Argument A0 }
CALLABLE_EXPRESSION
: HOLLERITH   { Argument () (getSpan $1) Nothing $1 }
| EXPRESSION  { Argument () (getSpan $1) Nothing $1 }

EXPRESSION :: { Expression A0 }
EXPRESSION
: EXPRESSION '+' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| EXPRESSION '-' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| EXPRESSION '*' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| EXPRESSION '/' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| EXPRESSION '**' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| EXPRESSION '/' '/' EXPRESSION %prec CONCAT { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| ARITHMETIC_SIGN EXPRESSION %prec NEGATION { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| EXPRESSION or EXPRESSION { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| EXPRESSION and EXPRESSION { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not EXPRESSION { ExpUnary () (getTransSpan $1 $2) Not $2 }
| EXPRESSION eqv EXPRESSION { ExpBinary () (getTransSpan $1 $3) Equivalent $1 $3 }
| EXPRESSION neqv EXPRESSION { ExpBinary () (getTransSpan $1 $3) NotEquivalent $1 $3 }
| EXPRESSION RELATIONAL_OPERATOR EXPRESSION %prec RELATIONAL { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| '(' EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| NUMERIC_LITERAL                   { $1 }
| '(' EXPRESSION ',' EXPRESSION ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4) }
| LOGICAL_LITERAL                   { $1 }
| STRING                            { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| SUBSCRIPT                         { $1 }
| VARIABLE                          { $1 }
| IMPLIED_DO                        { $1 }
| '(/' EXPRESSION_LIST '/)' {
    let { exps = reverse $2;
          expList = AList () (getSpan exps) exps }
    in ExpInitialisation () (getTransSpan $1 $3) expList
          }

IMPLIED_DO :: { Expression A0 }
IMPLIED_DO
: '(' EXPRESSION ',' DO_SPECIFICATION ')' {
    let expList = AList () (getSpan $2) [ $2 ]
          in ExpImpliedDo () (getTransSpan $1 $5) expList $4
         }
| '(' EXPRESSION ',' EXPRESSION ',' DO_SPECIFICATION ')' {
    let expList = AList () (getTransSpan $2 $4) [ $2, $4 ]
          in ExpImpliedDo () (getTransSpan $1 $5) expList $6
         }
| '(' EXPRESSION ',' EXPRESSION ',' EXPRESSION_LIST ',' DO_SPECIFICATION ')' {
    let { exps =  reverse $6;
          expList = AList () (getTransSpan $2 exps) ($2 : $4 : reverse $6) }
    in ExpImpliedDo () (getTransSpan $1 $9) expList $8
         }

EXPRESSION_LIST :: { [ Expression A0 ] }
EXPRESSION_LIST
: EXPRESSION_LIST ',' EXPRESSION { $3 : $1 }
| EXPRESSION { [ $1 ] }

STRING :: { Expression A0 } : string { let (TString s cs) = $1 in ExpValue () s (ValString cs) }

CONSTANT_EXPRESSION :: { Expression A0 }
CONSTANT_EXPRESSION
: CONSTANT_EXPRESSION '+' CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| CONSTANT_EXPRESSION '-' CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| CONSTANT_EXPRESSION '*' CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| CONSTANT_EXPRESSION '/' CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| CONSTANT_EXPRESSION '**' CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| CONSTANT_EXPRESSION '/' '/' CONSTANT_EXPRESSION %prec CONCAT { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| ARITHMETIC_SIGN CONSTANT_EXPRESSION %prec NEGATION { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| CONSTANT_EXPRESSION or CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| CONSTANT_EXPRESSION and CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not CONSTANT_EXPRESSION { ExpUnary () (getTransSpan $1 $2) Not $2 }
| CONSTANT_EXPRESSION RELATIONAL_OPERATOR CONSTANT_EXPRESSION %prec RELATIONAL { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| '(' CONSTANT_EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| NUMERIC_LITERAL               { $1 }
| '(' CONSTANT_EXPRESSION ',' CONSTANT_EXPRESSION ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4)}
| LOGICAL_LITERAL               { $1 }
| string                        { let (TString s cs) = $1 in ExpValue () s (ValString cs) }
| VARIABLE                     { $1 }

ARITHMETIC_CONSTANT_EXPRESSION :: { Expression A0 }
ARITHMETIC_CONSTANT_EXPRESSION
: ARITHMETIC_CONSTANT_EXPRESSION '+' ARITHMETIC_CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| ARITHMETIC_CONSTANT_EXPRESSION '-' ARITHMETIC_CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| ARITHMETIC_CONSTANT_EXPRESSION '*' ARITHMETIC_CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| ARITHMETIC_CONSTANT_EXPRESSION '/' ARITHMETIC_CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| ARITHMETIC_CONSTANT_EXPRESSION '**' ARITHMETIC_CONSTANT_EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| ARITHMETIC_SIGN ARITHMETIC_CONSTANT_EXPRESSION %prec NEGATION { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| '(' ARITHMETIC_CONSTANT_EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| NUMERIC_LITERAL               { $1 }
| '(' ARITHMETIC_CONSTANT_EXPRESSION ',' ARITHMETIC_CONSTANT_EXPRESSION ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4)}
| VARIABLE                     { $1 }

RELATIONAL_OPERATOR :: { BinaryOp }
RELATIONAL_OPERATOR
: '=='  { EQ }
| '!='  { NE }
| '>'   { GT }
| '>='  { GTE }
| '<'   { LT }
| '<='  { LTE }

SUBSCRIPT :: { Expression A0 }
SUBSCRIPT
: VARIABLE '(' ')'
  { ExpFunctionCall () (getTransSpan $1 $3) $1 Nothing }
| VARIABLE '(' INDICIES ')'
  { ExpSubscript () (getTransSpan $1 $4) $1 (fromReverseList $3) }
| VARIABLE '(' INDICIES ')' '(' INDICIES ')'
  { let innerSub = ExpSubscript () (getTransSpan $1 $4) $1 (fromReverseList $3)
    in ExpSubscript () (getTransSpan $1 $7) innerSub (fromReverseList $6) }

INDICIES :: { [ Index A0 ] }
: INDICIES ',' INDEX { $3 : $1 }
| INDEX { [ $1 ] }

INDEX :: { Index A0 }
: RANGE { $1 }
| EXPRESSION { IxSingle () (getSpan $1) Nothing $1 }

RANGE :: { Index A0 }
: ':' { IxRange () (getSpan $1) Nothing Nothing Nothing }
| ':' EXPRESSION { IxRange () (getTransSpan $1 $2) Nothing (Just $2) Nothing }
| EXPRESSION ':' { IxRange () (getTransSpan $1 $2) (Just $1) Nothing Nothing }
| EXPRESSION ':' EXPRESSION
  { IxRange () (getTransSpan $1 $3) (Just $1) (Just $3) Nothing }

ARITHMETIC_SIGN :: { (SrcSpan, UnaryOp) }
ARITHMETIC_SIGN
: '-' { (getSpan $1, Minus) }
| '+' { (getSpan $1, Plus) }

MAYBE_VARIABLES :: { Maybe (AList Expression A0) }
: VARIABLES { Just $ fromReverseList $1 } | {- EMPTY -} { Nothing }

VARIABLES :: { [ Expression A0 ] }
VARIABLES : VARIABLES ',' VARIABLE { $3 : $1 } | VARIABLE { [ $1 ] }

-- This may also be used to parse a function name, or an array name. Since when
-- are valid options in a production there is no way of differentiating them at
-- this stage.
-- This at least reduces reduce/reduce conflicts.
VARIABLE :: { Expression A0 }
VARIABLE
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValVariable s }

INTEGER_LITERAL :: { Expression A0 } : int { ExpValue () (getSpan $1) $ let (TInt _ i) = $1 in ValInteger i }

REAL_LITERAL :: { Expression A0 }
REAL_LITERAL
: int EXPONENT { makeReal (Just $1) Nothing Nothing (Just $2) }
| int '.' MAYBE_EXPONENT { makeReal (Just $1) (Just $2) Nothing $3 }
| '.' int MAYBE_EXPONENT { makeReal Nothing (Just $1) (Just $2) $3 }
| int '.' int MAYBE_EXPONENT { makeReal (Just $1) (Just $2) (Just $3) $4 }

MAYBE_EXPONENT :: { Maybe (SrcSpan, String) }
MAYBE_EXPONENT
: EXPONENT { Just $1 }
| {-EMPTY-} { Nothing }

EXPONENT :: { (SrcSpan, String) }
EXPONENT
: exponent { let (TExponent s exp) = $1 in (s, exp) }

SIGNED_NUMERIC_LITERAL :: { Expression A0 }
SIGNED_NUMERIC_LITERAL
: ARITHMETIC_SIGN NUMERIC_LITERAL { ExpUnary () (getTransSpan (fst $1) $2) Minus $2 }
| NUMERIC_LITERAL { $1 }

NUMERIC_LITERAL :: { Expression A0 }
NUMERIC_LITERAL
: INTEGER_LITERAL { $1 }
| REAL_LITERAL { $1 }

LOGICAL_LITERAL :: { Expression A0 }
LOGICAL_LITERAL : bool { let TBool s b = $1 in ExpValue () s $ ValLogical b }

HOLLERITH :: { Expression A0 } : hollerith { ExpValue () (getSpan $1) $ let (THollerith _ h) = $1 in ValHollerith h }

LABELS_IN_STATEMENT :: { AList Expression A0 }
LABELS_IN_STATEMENT
: LABELS_IN_STATEMENT_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

LABELS_IN_STATEMENT_LEVEL1 :: { AList Expression A0 }
LABELS_IN_STATEMENT_LEVEL1
: LABELS_IN_STATEMENT_LEVEL1 ',' LABEL_IN_STATEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' LABEL_IN_STATEMENT { AList () (getTransSpan $1 $2) [ $2 ] }

-- Labels that occur in the first 6 columns
LABEL_IN_6COLUMN :: { Expression A0 } : label { ExpValue () (getSpan $1) (let (TLabel _ l) = $1 in ValInteger l) }

-- Labels that occur in statements
LABEL_IN_STATEMENT :: { Expression A0 } : int { ExpValue () (getSpan $1) (let (TInt _ l) = $1 in ValInteger l) }

TYPE_SPEC :: { TypeSpec A0 }
TYPE_SPEC
: integer KIND_SELECTOR { TypeSpec () (getSpan $1) TypeInteger Nothing }
| real KIND_SELECTOR { TypeSpec () (getSpan $1) TypeReal Nothing }
| doublePrecision KIND_SELECTOR
  { TypeSpec () (getSpan $1) TypeDoublePrecision Nothing }
| logical KIND_SELECTOR { TypeSpec () (getSpan $1) TypeLogical Nothing }
| complex KIND_SELECTOR { TypeSpec () (getSpan $1) TypeComplex Nothing }
| doubleComplex KIND_SELECTOR
  { TypeSpec () (getSpan $1) TypeDoubleComplex Nothing }
| character CHAR_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeCharacter $2 }

KIND_SELECTOR :: { Maybe (Selector A0) }
KIND_SELECTOR
: '*' ARITHMETIC_CONSTANT_EXPRESSION
  { Just $ Selector () (getTransSpan $1 $2) Nothing (Just $2) }
| '*' '(' STAR ')' { Just $ Selector () (getTransSpan $1 $4) Nothing (Just $3) }
| {- EMPTY -} { Nothing }

CHAR_SELECTOR :: { Maybe (Selector A0) }
CHAR_SELECTOR
: '(' ARITHMETIC_CONSTANT_EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $3) (Just $2) Nothing }
| BASIC_CHAR_SELECTOR { $1 }

BASIC_CHAR_SELECTOR :: { Maybe (Selector A0) }
BASIC_CHAR_SELECTOR
: '*' ARITHMETIC_CONSTANT_EXPRESSION
  { Just $ Selector () (getTransSpan $1 $2) (Just $2) Nothing }
| '*' '(' STAR ')' { Just $ Selector () (getTransSpan $1 $4) (Just $3) Nothing }
| {- EMPTY -} { Nothing }

IMP_TYPE_SPEC :: { TypeSpec A0 }
IMP_TYPE_SPEC
: integer          { TypeSpec () (getSpan $1) TypeInteger Nothing }
| real             { TypeSpec () (getSpan $1) TypeReal Nothing }
| doublePrecision  { TypeSpec () (getSpan $1) TypeDoublePrecision Nothing }
| logical          { TypeSpec () (getSpan $1) TypeLogical Nothing }
| complex          { TypeSpec () (getSpan $1) TypeComplex Nothing }
| doubleComplex    { TypeSpec () (getSpan $1) TypeDoubleComplex Nothing }
| character BASIC_CHAR_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeCharacter $2 }

STAR :: { Expression A0 }
STAR : '*' { ExpValue () (getSpan $1) ValStar }

{

makeReal :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe (SrcSpan, String) -> Expression A0
makeReal i1 dot i2 exp =
  let span1   = getSpan (i1, dot, i2)
      span2   = case exp of
                  Just e -> getTransSpan span1 (fst e)
                  Nothing -> span1
      i1Str   = case i1 of { Just (TInt _ s) -> s ; _ -> "" }
      dotStr  = case dot of { Just (TDot _) -> "." ; _ -> "" }
      i2Str   = case i2 of { Just (TInt _ s) -> s ; _ -> "" }
      expStr  = case exp of { Just (_, s) -> s ; _ -> "" } in
    ExpValue () span2 (ValReal $ i1Str ++ dotStr ++ i2Str ++ expStr)

parse = evalParse programParser

transformations77 =
  [ GroupLabeledDo
  , GroupIf
  , DisambiguateFunction
  ]
fortran77Parser :: B.ByteString -> String -> ProgramFile A0
fortran77Parser = fortran77ParserWithModFiles emptyModFiles

fortran77ParserWithModFiles :: ModFiles -> B.ByteString -> String -> ProgramFile A0
fortran77ParserWithModFiles mods sourceCode filename =
    transformWithModFiles mods transformations77 $ parse parseState
  where
    parseState = initParseState sourceCode Fortran77Extended filename

transformations77Extended =
  [ GroupLabeledDo
  , GroupDo
  , GroupIf
  , DisambiguateFunction
  ]
extended77Parser :: B.ByteString -> String -> ProgramFile A0
extended77Parser = extended77ParserWithModFiles emptyModFiles

extended77ParserWithModFiles :: ModFiles -> B.ByteString -> String -> ProgramFile A0
extended77ParserWithModFiles mods sourceCode filename =
    transformWithModFiles mods transformations77Extended $ parse parseState
  where
    parseState = initParseState sourceCode Fortran77Extended filename

parseError :: Token -> LexAction a
parseError _ = do
    parseState <- get
#ifdef DEBUG
    tokens <- reverse <$> aiPreviousTokensInLine <$> getAlex
#endif
    fail $ psFilename parseState ++ ": parsing failed. "
#ifdef DEBUG
      ++ '\n' : show tokens
#endif


}
