-- -*- Mode: Haskell -*-
{
module Forpar.Parser.Fortran90 ( statementParser
                               ) where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST
import Control.Monad.State (get)

#ifdef DEBUG
import Data.Data (toConstr)
#endif

import Forpar.Util.Position
import Forpar.ParserMonad
import Forpar.Lexer.FreeForm
import Forpar.AST

import Debug.Trace

}

%name statementParser STATEMENT
%monad { LexAction }
%lexer { lexer } { TEOF _ }
%tokentype { Token }
%error { parseError }

%token
  id                          { TId _ _ }
  comment                     { TComment _ _ }
  string                      { TString _ _ }
  label                       { TLabel _ _ }
  int                         { TIntegerLiteral _ _ }
  float                       { TRealLiteral _ _ }
  boz                         { TBozLiteral _ _ }
  ','                         { TComma _ }
  ',2'                        { TComma2 _ }
  ';'                         { TSemiColon _ }
  ':'                         { TColon _ }
  '::'                        { TDoubleColon _ }
  '='                         { TOpAssign _ }
  '=>'                        { TArrow _ }
  '%'                         { TPercent _ }
  '('                         { TLeftPar _ }
  '(2'                        { TLeftPar2 _ }
  ')'                         { TRightPar _ }
  '(/'                        { TLeftInitPar _ }
  '/)'                        { TRightInitPar _ }
  opCustom                    { TOpCustom _ _ }
  '**'                        { TOpExp _ }
  '+'                         { TOpPlus _ }
  '-'                         { TOpMinus _ }
  '*'                         { TStar _ }
  '/'                         { TOpDivision _ }
  slash                       { TSlash _ }
  or                          { TOpOr _ }
  and                         { TOpAnd _ }
  not                         { TOpNot _ }
  eqv                         { TOpEquivalent _ }
  neqv                        { TOpNotEquivalent _ }
  '<'                         { TOpLT _ }
  '<='                        { TOpLE _ }
  '=='                        { TOpEQ _ }
  '!='                        { TOpNE _ }
  '>'                         { TOpGT _ }
  '>='                        { TOpGE _ }
  bool                        { TLogicalLiteral _ _ }
  program                     { TProgram _ }
  endProgram                  { TEndProgram _ }
  function                    { TFunction _ }
  endFunction                 { TEndFunction _ }
  result                      { TResult _ }
  recursive                   { TRecursive _ }
  subroutine                  { TSubroutine _ }
  endSubroutine               { TEndSubroutine _ }
  blockData                   { TBlockData _ }
  endBlockData                { TEndBlockData _ }
  module                      { TModule _ }
  endModule                   { TEndModule _ }
  contains                    { TContains _ }
  use                         { TUse _ }
  only                        { TOnly _ }
  interface                   { TInterface _ }
  endInterface                { TEndInterface _ }
  procedure                   { TProcedure _ }
  assignment                  { TAssignment _ }
  operator                    { TOperator _ }
  call                        { TCall _ }
  return                      { TReturn _ }
  public                      { TPublic _ }
  private                     { TPrivate _ }
  parameter                   { TParameter _ }
  allocatable                 { TAllocatable _ }
  dimension                   { TDimension _ }
  external                    { TExternal _ }
  intent                      { TIntent _ }
  intrinsic                   { TIntrinsic _ }
  optional                    { TOptional _ }
  pointer                     { TPointer _ }
  save                        { TSave _ }
  target                      { TTarget _ }
  in                          { TIn _ }
  out                         { TOut _ }
  inout                       { TInOut _ }
  data                        { TData _ }
  namelist                    { TNamelist _ }
  implicit                    { TImplicit _ }
  equivalence                 { TEquivalence _ }
  common                      { TCommon _ }
  allocate                    { TAllocate _ }
  deallocate                  { TDeallocate _ }
  nullify                     { TNullify _ }
  none                        { TNone _ }
  goto                        { TGoto _ }
  assign                      { TAssign _ }
  to                          { TTo _ }
  continue                    { TContinue _ }
  stop                        { TStop _ }
  pause                       { TPause _ }
  do                          { TDo _ }
  enddo                       { TEndDo _ }
  while                       { TWhile _ }
  if                          { TIf _ }
  then                        { TThen _ }
  else                        { TElse _ }
  elsif                       { TElsif _ }
  endif                       { TEndIf _ }
  case                        { TCase _ }
  selectcase                  { TSelectCase _ }
  endselect                   { TEndSelect _ }
  default                     { TDefault _ }
  cycle                       { TCycle _ }
  exit                        { TExit _ }
  where                       { TWhere _ }
  elsewhere                   { TElsewhere _ }
  endwhere                    { TEndWhere _ }
  type                        { TType _ }
  endType                     { TEndType _ }
  sequence                    { TSequence _ }
  kind                        { TKind _ }
  len                         { TLen _ }
  integer                     { TInteger _ }
  real                        { TReal _ }
  doublePrecision             { TDoublePrecision _ }
  logical                     { TLogical _ }
  character                   { TCharacter _ }
  complex                     { TComplex _ }
  open                        { TOpen _ }
  close                       { TClose _ }
  read                        { TRead _ }
  write                       { TWrite _ }
  print                       { TPrint _ }
  backspace                   { TBackspace _ }
  rewind                      { TRewind _ }
  inquire                     { TInquire _ }
  endfile                     { TEndfile _ }
  end                         { TEnd _ }
  newline                     { TNewline _ }

-- Precedence of operators

-- Level 6
%left opCustom

-- Level 5
%left eqv neqv
%left or
%left and
%right not

-- Level 4
%nonassoc '==' '!=' '>' '<' '>=' '<='
%nonassoc RELATIONAL

-- Level 3
%left CONCAT

-- Level 2
%left '+' '-'
%left '*' '/'
%right SIGN
%right '**'

-- Level 1
%right DEFINED_UNARY

-- Level 0
%left '%'

%%

STATEMENT :: { Statement A0 }
: LOGICAL_IF_STATEMENT { $1 }
| NONEXECUTABLE_STATEMENT { $1 }
| EXECUTABLE_STATEMENT { $1 }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
: DATA_REF '=' EXPRESSION { StExpressionAssign () (getTransSpan $1 $3) $1 $3 }

NONEXECUTABLE_STATEMENT :: { Statement A0 }
: DECLARATION_STATEMENT { $1 }
| intent '(' INTENT_CHOICE ')' MAYBE_DCOLON EXPRESSION_LIST
  { let expAList = fromReverseList $6
    in StIntent () (getTransSpan $1 expAList) $3 expAList }
| optional MAYBE_DCOLON EXPRESSION_LIST
  { let expAList = fromReverseList $3
    in StOptional () (getTransSpan $1 expAList) expAList }
| public MAYBE_DCOLON EXPRESSION_LIST
  { let expAList = fromReverseList $3
    in StPublic () (getTransSpan $1 expAList) (Just expAList) }
| public { StPublic () (getSpan $1) Nothing }
| private MAYBE_DCOLON EXPRESSION_LIST
  { let expAList = fromReverseList $3
    in StPrivate () (getTransSpan $1 expAList) (Just expAList) }
| private { StPrivate () (getSpan $1) Nothing }
| save MAYBE_DCOLON SAVE_ARGS
  { let saveAList = (fromReverseList $3)
    in StSave () (getTransSpan $1 saveAList) (Just saveAList) }
| save { StSave () (getSpan $1) Nothing }
| dimension MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StDimension () (getTransSpan $1 declAList) declAList }
| allocatable MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StAllocatable () (getTransSpan $1 declAList) declAList }
| pointer MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StPointer () (getTransSpan $1 declAList) declAList }
| target MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StTarget () (getTransSpan $1 declAList) declAList }
| data cDATA DATA_GROUPS cPOP
  { let dataAList = fromReverseList $3
    in StData () (getTransSpan $1 dataAList) dataAList }
| parameter '(' PARAMETER_ASSIGNMENTS ')'
  { let declAList = fromReverseList $3
    in StParameter () (getTransSpan $1 $4) declAList }
| implicit none { StImplicit () (getTransSpan $1 $2) Nothing }
| implicit cIMPLICIT IMP_LISTS cPOP
  { let impAList = fromReverseList $3
    in StImplicit () (getTransSpan $1 impAList) $ Just $ impAList }
| namelist cNAMELIST NAMELISTS cPOP
  { let nameALists = fromReverseList $3
    in StNamelist () (getTransSpan $1 nameALists) nameALists }
| equivalence EQUIVALENCE_GROUPS
  { let eqALists = fromReverseList $2
    in StEquivalence () (getTransSpan $1 eqALists) eqALists }
| common cCOMMON COMMON_GROUPS cPOP
  { let commonAList = fromReverseList $3
    in StCommon () (getTransSpan $1 commonAList) commonAList }

EXECUTABLE_STATEMENT :: { Statement A0 }
: allocate '(' DATA_REFS ')'
  { StAllocate () (getTransSpan $1 $4) (fromReverseList $3) Nothing }
| allocate '(' DATA_REFS ',' CILIST_PAIR ')'
  { StAllocate () (getTransSpan $1 $6) (fromReverseList $3) (Just $5) }
| nullify '(' DATA_REFS ')'
  { StNullify () (getTransSpan $1 $4) (fromReverseList $3) }
| deallocate '(' DATA_REFS ')'
  { StDeallocate () (getTransSpan $1 $4) (fromReverseList $3) Nothing }
| deallocate '(' DATA_REFS ',' CILIST_PAIR ')'
  { StDeallocate () (getTransSpan $1 $6) (fromReverseList $3) (Just $5) }
| EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| DATA_REF '=>' EXPRESSION { StPointerAssign () (getTransSpan $1 $3) $1 $3 }
| where '(' EXPRESSION ')' EXPRESSION_ASSIGNMENT_STATEMENT
  { StWhere () (getTransSpan $1 $5) $3 $5 }
| where '(' EXPRESSION ')' { StWhereConstruct () (getTransSpan $1 $4) $3 }
| elsewhere { StElsewhere () (getSpan $1) }
| endwhere { StEndwhere () (getSpan $1) }
| if '(' EXPRESSION ')' INTEGER_LITERAL ',' INTEGER_LITERAL ',' INTEGER_LITERAL
  { StIfArithmetic () (getTransSpan $1 $9) $3 $5 $7 $9 }
| if '(' EXPRESSION ')' then { StIfThen () (getTransSpan $1 $5) Nothing $3 }
| VARIABLE ':' if '(' EXPRESSION ')' then
  { StIfThen () (getTransSpan $1 $7) (Just $1) $5 }
| elsif '(' EXPRESSION ')' then { StElsif () (getTransSpan $1 $5) Nothing $3 }
| elsif '(' EXPRESSION ')' then VARIABLE
  { StElsif () (getTransSpan $1 $6) (Just $6) $3 }
| else { StElse () (getSpan $1) Nothing }
| else VARIABLE { StElse () (getSpan $1) (Just $2) }
| endif { StEndif () (getSpan $1) Nothing }
| endif VARIABLE { StEndif () (getSpan $1) (Just $2) }
| do INTEGER_LITERAL MAYBE_COMMA DO_SPECIFICATION
  { StDo () (getTransSpan $1 $4) Nothing (Just $2) $4 }
| do DO_SPECIFICATION { StDo () (getTransSpan $1 $2) Nothing Nothing $2 }
| VARIABLE ':' do DO_SPECIFICATION
  { StDo () (getTransSpan $1 $4) (Just $1) Nothing $4 }
| do INTEGER_LITERAL MAYBE_COMMA while '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $7) Nothing (Just $2) $6 }
| do while '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $5) Nothing Nothing $4 }
| VARIABLE ':' do while '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $7) (Just $1) Nothing $6 }
| enddo { StEnddo () (getSpan $1) Nothing }
| enddo VARIABLE { StEnddo () (getSpan $1) (Just $2) }
| selectcase '(' EXPRESSION ')'
  { StSelectCase () (getTransSpan $1 $4) Nothing $3 }
| VARIABLE ':' selectcase '(' EXPRESSION ')'
  { StSelectCase () (getTransSpan $1 $6) (Just $1) $5 }
| case default { StCase () (getTransSpan $1 $2) Nothing Nothing }
| case default VARIABLE { StCase () (getTransSpan $1 $3) (Just $3) Nothing }
| case '(' RANGE ')' { StCase () (getTransSpan $1 $4) Nothing (Just $3) }
| case '(' RANGE ')' VARIABLE
  { StCase () (getTransSpan $1 $5) (Just $5) (Just $3) }
| endselect { StEndcase () (getSpan $1) Nothing }
| endselect VARIABLE { StEndcase () (getTransSpan $1 $2) (Just $2) }

LOGICAL_IF_STATEMENT :: { Statement A0 }
: if '(' EXPRESSION ')' EXECUTABLE_STATEMENT
  { StIfLogical () (getTransSpan $1 $5) $3 $5 }

CILIST_PAIR :: { ControlPair A0 }
: id '=' CILIST_ELEMENT
  { let (TId s id) = $1 in ControlPair () (getTransSpan s $3) (Just id) $3 }

CILIST_ELEMENT :: { Expression A0 }
: EXPRESSION { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

MAYBE_DCOLON :: { () } : '::' { () } | {- EMPTY -} { () }

COMMON_GROUPS :: { [ CommonGroup A0 ] }
: COMMON_GROUPS COMMON_GROUP { $2 : $1 }
| COMMON_GROUPS ',2' COMMON_GROUP { $3 : $1 }
| INIT_COMMON_GROUP { [ $1 ] }

COMMON_GROUP :: { CommonGroup A0 }
: COMMON_NAME PART_REFS
  { let alist = fromReverseList $2
    in CommonGroup () (getTransSpan $1 alist) (Just $1) alist }
| '/' '/' PART_REFS
  { let alist = fromReverseList $3
    in CommonGroup () (getTransSpan $1 alist) Nothing alist }

INIT_COMMON_GROUP :: { CommonGroup A0 }
: COMMON_NAME PART_REFS
  { let alist = fromReverseList $2
    in CommonGroup () (getTransSpan $1 alist) (Just $1) alist }
| '/' '/' PART_REFS
  { let alist = fromReverseList $3
    in CommonGroup () (getTransSpan $1 alist) Nothing alist }
| PART_REFS
  { let alist = fromReverseList $1
    in CommonGroup () (getSpan alist) Nothing alist }

EQUIVALENCE_GROUPS :: { [ AList Expression A0 ] }
: EQUIVALENCE_GROUPS ',' '(' PART_REFS ')'
  { setSpan (getTransSpan $3 $5) (fromReverseList $4) : $1 }
| '(' PART_REFS ')'
  { [ setSpan (getTransSpan $1 $3) (fromReverseList $2) ] }

NAMELISTS :: { [ Namelist A0 ] }
: NAMELISTS NAMELIST { $2 : $1 }
| NAMELISTS ',2' NAMELIST { $3 : $1 }
| NAMELIST { [ $1 ] }

NAMELIST :: { Namelist A0 }
: '/' VARIABLE '/' VARIABLES
  { Namelist () (getTransSpan $1 $4) $2 $ fromReverseList $4 }

VARIABLES :: { [ Expression A0 ] }
: VARIABLES ',' VARIABLE { $3 : $1 }
| VARIABLE { [ $1 ] }

IMP_LISTS :: { [ ImpList A0 ] }
: IMP_LISTS ',' IMP_LIST { $3 : $1 }
| IMP_LIST { [ $1 ] }

IMP_LIST :: { ImpList A0 }
: TYPE_SPEC '(2' IMP_ELEMENTS ')'
  { ImpList () (getTransSpan $1 $4) $1 (aReverse $3) }

IMP_ELEMENTS :: { AList ImpElement A0 }
: IMP_ELEMENTS ',' IMP_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| IMP_ELEMENT { AList () (getSpan $1) [ $1 ] }

IMP_ELEMENT :: { ImpElement A0 }
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

PARAMETER_ASSIGNMENTS :: { [ Declarator A0 ] }
: PARAMETER_ASSIGNMENTS ',' PARAMETER_ASSIGNMENT { $3 : $1 }
| PARAMETER_ASSIGNMENT { [ $1 ] }

PARAMETER_ASSIGNMENT :: { Declarator A0 }
: VARIABLE '=' EXPRESSION
  { DeclVariable () (getTransSpan $1 $3) $1 Nothing (Just $3) }

DECLARATION_STATEMENT :: { Statement A0 }
: TYPE_SPEC ATTRIBUTE_LIST '::' DECLARATOR_LIST
  { let { mAttrAList = if null $2 then Nothing else Just $ fromReverseList $2;
          declAList = fromReverseList $4 }
    in StDeclaration () (getTransSpan $1 declAList) $1 mAttrAList declAList }
| TYPE_SPEC DECLARATOR_LIST
  { let { declAList = fromReverseList $2 }
    in StDeclaration () (getTransSpan $1 declAList) $1 Nothing declAList }

ATTRIBUTE_LIST :: { [ Attribute A0 ] }
: ATTRIBUTE_LIST ',' ATTRIBUTE_SPEC { $3 : $1 }
| {- EMPTY -} { [ ] }

ATTRIBUTE_SPEC :: { Attribute A0 }
: public { AttrPublic () (getSpan $1) }
| private { AttrPrivate () (getSpan $1) }
| allocatable { AttrAllocatable () (getSpan $1) }
| dimension '(' DIMENSION_DECLARATORS ')'
  { AttrDimension () (getTransSpan $1 $4) $3 }
| external { AttrExternal () (getSpan $1) }
| intent '(' INTENT_CHOICE ')' { AttrIntent () (getTransSpan $1 $4) $3 }
| intrinsic { AttrIntrinsic () (getSpan $1) }
| optional { AttrOptional () (getSpan $1) }
| pointer { AttrPointer () (getSpan $1) }
| save { AttrSave () (getSpan $1) }
| target { AttrTarget () (getSpan $1) }

INTENT_CHOICE :: { Intent } : in { In } | out { Out } | inout { InOut }

DATA_GROUPS :: { [ DataGroup A0 ] }
: DATA_GROUPS MAYBE_COMMA DATA_LIST slash EXPRESSION_LIST slash
  { let { nameAList = fromReverseList $3;
          dataAList = fromReverseList $5 }
    in DataGroup () (getTransSpan nameAList $6) nameAList dataAList : $1 }
| DATA_LIST slash EXPRESSION_LIST slash
  { let { nameAList = fromReverseList $1;
          dataAList = fromReverseList $3 }
    in [ DataGroup () (getTransSpan nameAList $4) nameAList dataAList ] }

MAYBE_COMMA :: { () } : ',' { () } | {- EMPTY -} { () }

DATA_LIST :: { [ Expression A0 ] }
: DATA_LIST ',' DATA_ELEMENT { $3 : $1 }
| DATA_ELEMENT { [ $1 ] }

DATA_ELEMENT :: { Expression A0 }
: DATA_REF { $1 } | IMPLIED_DO { $1 }

SAVE_ARGS :: { [ Expression A0 ] }
: SAVE_ARGS ',' SAVE_ARG { $3 : $1 } | SAVE_ARG { [ $1 ] }

SAVE_ARG :: { Expression A0 } : COMMON_NAME { $1 } | VARIABLE { $1 }

COMMON_NAME :: { Expression A0 }
: '/' VARIABLE '/' { setSpan (getTransSpan $1 $3) $2 }

DECLARATOR_LIST :: { [ Declarator A0 ] }
: DECLARATOR_LIST ',' INITIALISED_DECLARATOR { $3 : $1 }
| INITIALISED_DECLARATOR { [ $1 ] }

INITIALISED_DECLARATOR :: { Declarator A0 }
: DECLARATOR '=' EXPRESSION { setInitialisation $1 $3 }
| DECLARATOR { $1 }

DECLARATOR :: { Declarator A0 }
: VARIABLE { DeclVariable () (getSpan $1) $1 Nothing Nothing }
| VARIABLE '*' EXPRESSION
  { DeclVariable () (getTransSpan $1 $3) $1 (Just $3) Nothing }
| VARIABLE '*' '(' '*' ')'
  { let star = ExpValue () (getSpan $4) ValStar
    in DeclVariable () (getTransSpan $1 $5) $1 (Just star) Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')'
  { DeclArray () (getTransSpan $1 $4) $1 $3 Nothing Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')' '*' EXPRESSION
  { DeclArray () (getTransSpan $1 $6) $1 $3 (Just $6) Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')' '*' '(' '*' ')'
  { let star = ExpValue () (getSpan $7) ValStar
    in DeclArray () (getTransSpan $1 $8) $1 $3 (Just star) Nothing }

DIMENSION_DECLARATORS :: { AList DimensionDeclarator A0 }
: DIMENSION_DECLARATORS ',' DIMENSION_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DIMENSION_DECLARATOR
  { AList () (getSpan $1) [ $1 ] }

DIMENSION_DECLARATOR :: { DimensionDeclarator A0 }
: EXPRESSION ':' EXPRESSION
  { DimensionDeclarator () (getTransSpan $1 $3) (Just $1) $3 }
| EXPRESSION { DimensionDeclarator () (getSpan $1) Nothing $1 }
| EXPRESSION ':' '*'
  { let { span = getSpan $3;
          star = ExpValue () span ValStar }
    in DimensionDeclarator () (getTransSpan $1 span) (Just $1) star }
| '*'
  { let { span = getSpan $1;
          star = ExpValue () span ValStar }
    in DimensionDeclarator () span Nothing star }

TYPE_SPEC :: { TypeSpec A0 }
: integer KIND_SELECTOR   { TypeSpec () (getSpan ($1, $2)) TypeInteger $2 }
| real    KIND_SELECTOR   { TypeSpec () (getSpan ($1, $2)) TypeReal $2 }
| doublePrecision { TypeSpec () (getSpan $1) TypeDoublePrecision Nothing }
| complex KIND_SELECTOR   { TypeSpec () (getSpan ($1, $2)) TypeComplex $2 }
| character CHAR_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeCharacter $2 }
| logical KIND_SELECTOR   { TypeSpec () (getSpan ($1, $2)) TypeLogical $2 }
| type '(' id ')'
  { let TId _ id = $3
    in TypeSpec () (getTransSpan $1 $4) (TypeCustom id) Nothing }

KIND_SELECTOR :: { Maybe (Selector A0) }
: '(' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $3) Nothing (Just $2) }
| '(' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) Nothing (Just $4) }
| {- EMPTY -} { Nothing }

CHAR_SELECTOR :: { Maybe (Selector A0) }
: '*' EXPRESSION
  { Just $ Selector () (getTransSpan $1 $2) (Just $2) Nothing }
-- The following rule is a bug in the spec.
-- | '*' EXPRESSION ','
--   { Just $ Selector () (getTransSpan $1 $2) (Just $2) Nothing }
| '*' '(' '*' ')'
  { let star = ExpValue () (getSpan $3) ValStar
    in Just $ Selector () (getTransSpan $1 $4) (Just star) Nothing }
| '(' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $3) (Just $2) Nothing }
| '(' len '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) (Just $4) Nothing }
| '(' EXPRESSION ',' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) (Just $2) (Just $4) }
| '(' EXPRESSION ',' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $7) (Just $2) (Just $6) }
| '(' len '=' EXPRESSION ',' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $9) (Just $4) (Just $8) }
| '(' kind '=' EXPRESSION ',' len '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $9) (Just $8) (Just $4) }
| {- EMPTY -} { Nothing }

EXPRESSION :: { Expression A0 }
: EXPRESSION '+' EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| EXPRESSION '-' EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| EXPRESSION '*' EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| EXPRESSION '/' EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| EXPRESSION '**' EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| EXPRESSION '/' '/' EXPRESSION %prec CONCAT
  { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| ARITHMETIC_SIGN EXPRESSION %prec SIGN
  { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| EXPRESSION or EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| EXPRESSION and EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not EXPRESSION
  { ExpUnary () (getTransSpan $1 $2) Not $2 }
| EXPRESSION eqv EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Equivalent $1 $3 }
| EXPRESSION neqv EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) NotEquivalent $1 $3 }
| EXPRESSION RELATIONAL_OPERATOR EXPRESSION %prec RELATIONAL
  { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| opCustom EXPRESSION %prec DEFINED_UNARY {
    let TOpCustom span str = $1
    in ExpUnary () (getTransSpan span $2) (UnCustom str) $2 }
| EXPRESSION opCustom EXPRESSION {
    let TOpCustom _ str = $2
    in ExpBinary () (getTransSpan $1 $3) (BinCustom str) $1 $3 }
| '(' EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| NUMERIC_LITERAL                   { $1 }
| '(' EXPRESSION ',' EXPRESSION ')'
  { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4) }
| LOGICAL_LITERAL                   { $1 }
| STRING                            { $1 }
| DATA_REF                          { $1 }
| IMPLIED_DO                        { $1 }
| '(/' EXPRESSION_LIST '/)'
  { ExpInitialisation () (getTransSpan $1 $3) (fromReverseList $2) }
| operator '(' opCustom ')'
  { let TOpCustom _ op = $3
    in ExpValue () (getTransSpan $1 $4) (ValOperator op) }
| assignment { ExpValue () (getSpan $1) ValAssignment }

DATA_REFS :: { [ Expression A0 ] }
: DATA_REFS ',' DATA_REF { $3 : $1 }
| DATA_REF { [ $1 ] }

DATA_REF :: { Expression A0 }
: DATA_REF '%' PART_REF { ExpDataRef () (getTransSpan $1 $3) $1 $3 }
| PART_REF { $1 }

PART_REFS :: { [ Expression A0 ] }
: PART_REFS ',' PART_REF { $3 : $1 }
| PART_REF { [ $1 ] }

PART_REF :: { Expression A0 }
: VARIABLE { $1 }
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
| RANGE ':' EXPRESSION
  { let IxRange () s lower upper _ = $1
    in IxRange () (getTransSpan s $3) lower upper (Just $3) }
| EXPRESSION { IxSingle () (getSpan $1) $1 }

RANGE :: { Index A0 }
: ':' { IxRange () (getSpan $1) Nothing Nothing Nothing }
| ':' EXPRESSION { IxRange () (getTransSpan $1 $2) Nothing (Just $2) Nothing }
| EXPRESSION ':' { IxRange () (getTransSpan $1 $2) (Just $1) Nothing Nothing }
| EXPRESSION ':' EXPRESSION
  { IxRange () (getTransSpan $1 $3) (Just $1) (Just $3) Nothing }

DO_SPECIFICATION :: { DoSpecification A0 }
: EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION ',' EXPRESSION
  { DoSpecification () (getTransSpan $1 $5) $1 $3 (Just $5) }
| EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION
  { DoSpecification () (getTransSpan $1 $3) $1 $3 Nothing }

IMPLIED_DO :: { Expression A0 }
: '(' EXPRESSION ',' DO_SPECIFICATION ')'
  { let expList = AList () (getSpan $2) [ $2 ]
    in ExpImpliedDo () (getTransSpan $1 $5) expList $4 }
| '(' EXPRESSION ',' EXPRESSION ',' DO_SPECIFICATION ')'
  { let expList = AList () (getTransSpan $2 $4) [ $2, $4 ]
    in ExpImpliedDo () (getTransSpan $1 $5) expList $6 }
| '(' EXPRESSION ',' EXPRESSION ',' EXPRESSION_LIST ',' DO_SPECIFICATION ')'
  { let { exps =  reverse $6;
          expList = AList () (getTransSpan $2 exps) ($2 : $4 : reverse $6) }
    in ExpImpliedDo () (getTransSpan $1 $9) expList $8 }

EXPRESSION_LIST :: { [ Expression A0 ] }
: EXPRESSION_LIST ',' EXPRESSION { $3 : $1 }
| EXPRESSION { [ $1 ] }

ARITHMETIC_SIGN :: { (SrcSpan, UnaryOp) }
: '-' { (getSpan $1, Minus) }
| '+' { (getSpan $1, Plus) }

RELATIONAL_OPERATOR :: { BinaryOp }
: '=='  { EQ }
| '!='  { NE }
| '>'   { GT }
| '>='  { GTE }
| '<'   { LT }
| '<='  { LTE }

VARIABLE :: { Expression A0 }
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValVariable () s }

NUMERIC_LITERAL :: { Expression A0 }
: INTEGER_LITERAL { $1 } | REAL_LITERAL { $1 }

INTEGER_LITERAL :: { Expression A0 }
: int { let TIntegerLiteral s i = $1 in ExpValue () s $ ValInteger i }
| boz { let TBozLiteral s i = $1 in ExpValue () s $ ValInteger i }

REAL_LITERAL :: { Expression A0 }
: float { let TRealLiteral s r = $1 in ExpValue () s $ ValReal r }

LOGICAL_LITERAL :: { Expression A0 }
: bool { let TLogicalLiteral s b = $1 in ExpValue () s $ ValLogical b }

STRING :: { Expression A0 }
: string { let TString s c = $1 in ExpValue () s $ ValString c }

cDATA :: { () } : {% pushContext ConData }
cIMPLICIT :: { () } : {% pushContext ConImplicit }
cNAMELIST :: { () } : {% pushContext ConNamelist }
cCOMMON :: { () } : {% pushContext ConCommon }
cPOP :: { () } : {% popContext }

{

parseError :: Token -> LexAction a
parseError _ = do
#ifdef DEBUG
    tokens <- reverse <$> aiPreviousTokensInLine <$> getAlex
#endif
    fail $ "Parsing failed."
#ifdef DEBUG
      ++ '\n' : show tokens
#endif

}
