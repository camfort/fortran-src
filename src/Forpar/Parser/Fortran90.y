-- -*- Mode: Haskell -*-
{
module Forpar.Parser.Fortran90 ( statementParser
                               ) where

import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST

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
  ';'                         { TSemiColon _ }
  ':'                         { TColon _ }
  '::'                        { TDoubleColon _ }
  '='                         { TOpAssign _ }
  '=>'                        { TArrow _ }
  '%'                         { TPercent _ }
  '('                         { TLeftPar _ }
  ')'                         { TRightPar _ }
  '(/'                        { TLeftInitPar _ }
  '/)'                        { TRightInitPar _ }
  opCustom                    { TOpCustom _ _ }
  '**'                        { TOpExp _ }
  '+'                         { TOpPlus _ }
  '-'                         { TOpMinus _ }
  '*'                         { TStar _ }
  '/'                         { TSlash _ }
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
  endDo                       { TEndDo _ }
  while                       { TWhile _ }
  if                          { TIf _ }
  then                        { TThen _ }
  else                        { TElse _ }
  elsif                       { TElsif _ }
  endif                       { TEndIf _ }
  case                        { TCase _ }
  selectCase                  { TSelectCase _ }
  endSelect                   { TEndSelect _ }
  default                     { TDefault _ }
  cycle                       { TCycle _ }
  exit                        { TExit _ }
  where                       { TWhere _ }
  elsewhere                   { TElsewhere _ }
  endWhere                    { TEndWhere _ }
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

%%

STATEMENT :: { Statement A0 }
: EXPRESSION_ASSIGNMENT_STATEMENT { $1 }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
: ELEMENT '=' EXPRESSION { StExpressionAssign () (getTransSpan $1 $3) $1 $3 }

ELEMENT :: { Expression A0 } : VARIABLE { $1 } | SUBSCRIPT { $1 }

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
    in ExpUnary () (getTransSpan span $2) (UnCustom str) $2
      }
| EXPRESSION opCustom EXPRESSION {
    let TOpCustom _ str = $2
    in ExpBinary () (getTransSpan $1 $3) (BinCustom str) $1 $3
      }
| '(' EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| NUMERIC_LITERAL                   { $1 }
| '(' EXPRESSION ',' EXPRESSION ')'
  { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4) }
| LOGICAL_LITERAL                   { $1 }
| STRING                            { $1 }
| SUBSCRIPT                         { $1 }
| SUBSTRING                         { $1 }
| VARIABLE                          { $1 }
| IMPLIED_DO                        { $1 }
| '(/' EXPRESSION_LIST '/)' {
    let { exps = reverse $2;
          expList = AList () (getSpan exps) exps }
    in ExpInitialisation () (getTransSpan $1 $3) expList
          }

DO_SPECIFICATION :: { DoSpecification A0 }
: EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION ',' EXPRESSION 
  { DoSpecification () (getTransSpan $1 $5) $1 $3 (Just $5) }
| EXPRESSION_ASSIGNMENT_STATEMENT ',' EXPRESSION                
  { DoSpecification () (getTransSpan $1 $3) $1 $3 Nothing }

SUBSTRING :: { Expression A0 }
: SUBSCRIPT '(' EXPRESSION ':' EXPRESSION ')'
  { ExpSubstring () (getTransSpan $1 $6) $1 (Just $3) (Just $5) }
| SUBSCRIPT '(' ':' EXPRESSION ')'
  { ExpSubstring () (getTransSpan $1 $5) $1 Nothing (Just $4) }
| SUBSCRIPT '(' EXPRESSION ':' ')'
  { ExpSubstring () (getTransSpan $1 $5) $1 (Just $3) Nothing }
| SUBSCRIPT '(' ':' ')'
  { ExpSubstring () (getTransSpan $1 $4) $1 Nothing Nothing }
| ARRAY '(' EXPRESSION ':' EXPRESSION ')'
  { ExpSubstring () (getTransSpan $1 $6) $1 (Just $3) (Just $5) }
| ARRAY '(' ':' EXPRESSION ')'
  { ExpSubstring () (getTransSpan $1 $5) $1 Nothing (Just $4) }
| ARRAY '(' EXPRESSION ':' ')'
  { ExpSubstring () (getTransSpan $1 $5) $1 (Just $3) Nothing }
| ARRAY '(' ':' ')'
  { ExpSubstring () (getTransSpan $1 $4) $1 Nothing Nothing }

SUBSCRIPT :: { Expression A0 }
: ARRAY INDICIES { ExpSubscript () (getTransSpan $1 $2) $1 $2 }

INDICIES :: { AList Expression A0 }
: INDICIES_L1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

INDICIES_L1 :: { AList Expression A0  }
: INDICIES_L1 ',' EXPRESSION { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' EXPRESSION { AList () (getTransSpan $1 $2) [ $2 ] }
| '(' { AList () (getSpan $1) [ ] }

IMPLIED_DO :: { Expression A0 }
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

ARRAY :: { Expression A0 }
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValArray () s }

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

{

type A0 = ()

parseError :: Token -> LexAction a
parseError _ = fail "Parsing failed."

}
