-- -*- Mode: Haskell -*-
{
module Language.Fortran.Parser.Fortran95 ( functionParser
                                         , blockParser
                                         , statementParser
                                         , fortran95Parser
                                         , fortran95ParserWithTransforms
                                         , fortran95ParserWithModFiles
                                         , fortran95ParserWithModFilesWithTransforms
                                         ) where


import Prelude hiding (EQ,LT,GT) -- Same constructors exist in the AST
import Control.Monad.State
import Data.Maybe (fromMaybe, isJust)
import Data.List (nub)
import Data.Either (either, lefts, rights, partitionEithers)
import Control.Applicative
import qualified Data.ByteString.Char8 as B

#ifdef DEBUG
import Data.Data (toConstr)
#endif

import Language.Fortran.Util.Position
import Language.Fortran.Util.ModFile
import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FreeForm
import Language.Fortran.AST
import Language.Fortran.Transformer

import Debug.Trace

}

%name programParser PROGRAM
%name blockParser BLOCK
%name statementParser STATEMENT
%name functionParser SUBPROGRAM_UNIT
%monad { LexAction }
%lexer { lexer } { TEOF _ }
%tokentype { Token }
%error { parseError }

%token
  id                          { TId _ _ }
  comment                     { TComment _ _ }
  string                      { TString _ _ }
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
  pure                        { TPure _ }
  elemental                   { TElemental _ }
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
  moduleProcedure             { TModuleProcedure _ }
  assignment                  { TAssignment _ }
  operator                    { TOperator _ }
  call                        { TCall _ }
  return                      { TReturn _ }
  entry                       { TEntry _ }
  include                     { TInclude _ }
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
  value                       { TValue _ }
  volatile                    { TVolatile _ }
  in                          { TIn _ }
  out                         { TOut _ }
  inout                       { TInOut _ }
  data                        { TData _ }
  namelist                    { TNamelist _ }
  implicit                    { TImplicit _ }
  equivalence                 { TEquivalence _ }
  common                      { TCommon _ }
  allocate                    { TAllocate _ }
  stat                        { TStat _ }
  deallocate                  { TDeallocate _ }
  nullify                     { TNullify _ }
  none                        { TNone _ }
  goto                        { TGoto _ }
  to                          { TTo _ }
  continue                    { TContinue _ }
  stop                        { TStop _ }
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
  format                      { TFormat _ }
  blob                        { TBlob _ _ }
  end                         { TEnd _ }
  newline                     { TNewline _ }
  forall                      { TForall _ }
  endforall                   { TEndForall _ }
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

maybe(p)
: p           { Just $1 }
| {- empty -} { Nothing }

-- This rule is to ignore leading whitespace
PROGRAM :: { ProgramFile A0 }
: NEWLINE PROGRAM_INNER { $2 }
| PROGRAM_INNER { $1 }

PROGRAM_INNER :: { ProgramFile A0 }
: PROGRAM_UNITS { ProgramFile (MetaInfo { miVersion = Fortran95, miFilename = "" }) (reverse $1) }
| {- empty -}   { ProgramFile (MetaInfo { miVersion = Fortran95, miFilename = "" }) [] }

PROGRAM_UNITS :: { [ ProgramUnit A0 ] }
: PROGRAM_UNITS PROGRAM_UNIT MAYBE_NEWLINE { $2 : $1 }
| PROGRAM_UNIT MAYBE_NEWLINE { [ $1 ] }

PROGRAM_UNIT :: { ProgramUnit A0 }
: program NAME NEWLINE BLOCKS MAYBE_SUBPROGRAM_UNITS PROGRAM_END
  {% do { unitNameCheck $6 $2;
          return $ PUMain () (getTransSpan $1 $6) (Just $2) (reverse $4) $5 } }
| module NAME NEWLINE BLOCKS MAYBE_SUBPROGRAM_UNITS MODULE_END
  {% do { unitNameCheck $6 $2;
          return $ PUModule () (getTransSpan $1 $6) $2 (reverse $4) $5 } }
| blockData NEWLINE BLOCKS BLOCK_DATA_END
  { PUBlockData () (getTransSpan $1 $4) Nothing (reverse $3) }
| blockData NAME NEWLINE BLOCKS BLOCK_DATA_END
  {% do { unitNameCheck $5 $2;
          return $ PUBlockData () (getTransSpan $1 $5) (Just $2) (reverse $4) } }
| SUBPROGRAM_UNIT { $1 }

MAYBE_SUBPROGRAM_UNITS :: { Maybe [ ProgramUnit A0 ] }
: contains NEWLINE SUBPROGRAM_UNITS { Just $ reverse $3 }
| {- Empty -} { Nothing }

SUBPROGRAM_UNITS :: { [ ProgramUnit A0 ] }
: SUBPROGRAM_UNITS SUBPROGRAM_UNIT NEWLINE { $2 : $1 }
| {- EMPTY -} { [ ] }

SUBPROGRAM_UNIT :: { ProgramUnit A0 }
: PREFIXES function NAME MAYBE_ARGUMENTS MAYBE_RESULT MAYBE_COMMENT NEWLINE BLOCKS MAYBE_SUBPROGRAM_UNITS FUNCTION_END
  {% do { unitNameCheck $10 $3;
          let (pfxs, typeSpec) = case partitionEithers $1 of
                                   { (ps, t:_) -> (fromReverseList' ps, Just t)
                                   ; (ps, [])  -> (fromReverseList' ps, Nothing) } in
          let sfx = emptySuffixes in
          let ss = if null $1 then getTransSpan $2 $10 else getTransSpan (reverse $1) $10 in
          if validPrefixSuffix (pfxs, sfx) then
            return $ PUFunction () ss typeSpec (pfxs, sfx) $3 $4 $5 (reverse $8) $9
          else fail "Cannot specify elemental along with recursive." } }
| PREFIXES subroutine NAME MAYBE_ARGUMENTS MAYBE_COMMENT NEWLINE BLOCKS MAYBE_SUBPROGRAM_UNITS SUBROUTINE_END
  {% do { unitNameCheck $9 $3;
          (pfxs, typeSpec) <- case partitionEithers $1 of
                                { (ps, t:_) -> fail "Subroutines cannot have return types."
                                ; (ps, [])  -> return (fromReverseList' ps, Nothing) };
          let sfx = emptySuffixes in
          let ss = if null $1 then getTransSpan $2 $9 else getTransSpan (reverse $1) $9 in
          if validPrefixSuffix (pfxs, sfx) then
            return $ PUSubroutine () ss (pfxs, sfx) $3 $4 (reverse $7) $8
          else fail "Cannot specify elemental along with recursive." } }
| comment { let (TComment s c) = $1 in PUComment () s (Comment c) }

-- (Fortran2003) R1227, Fortran95 (...)
PREFIXES :: { [Either (Prefix A0) (TypeSpec A0)] }
: PREFIXES PREFIX { $2:$1 }
| {- EMPTY -}     { [] }

-- (Fortran2003) R1228, Fortran95 (...)
PREFIX :: { Either (Prefix A0) (TypeSpec A0) }
: recursive { Left $ PfxRecursive () (getSpan $1) }
| elemental { Left $ PfxElemental () (getSpan $1) }
| pure      { Left $ PfxPure      () (getSpan $1) }
| TYPE_SPEC { Right $1 }

RESULT :: { Expression A0 }
: result '(' VARIABLE ')' { $3 }

MAYBE_RESULT :: { Maybe (Expression A0) }
: RESULT      { Just $1 }
| {- empty -} { Nothing}

MAYBE_ARGUMENTS :: { Maybe (AList Expression A0) }
: '(' MAYBE_VARIABLES ')' { $2 }
| {- Nothing -} { Nothing }

PROGRAM_END :: { Token }
: end { $1 } | endProgram { $1 } | endProgram id { $2 }
MODULE_END :: { Token }
: end { $1 } | endModule { $1 } | endModule id { $2 }
FUNCTION_END :: { Token }
: end { $1 } | endFunction { $1 } | endFunction id { $2 }
SUBROUTINE_END :: { Token }
: end { $1 } | endSubroutine { $1 } | endSubroutine id { $2 }
BLOCK_DATA_END :: { Token }
: end { $1 } | endBlockData { $1 } | endBlockData id { $2 }
INTERFACE_END :: { Token }
: end { $1 } | endInterface { $1 } | endInterface id { $2 }

NAME :: { Name } : id { let (TId _ name) = $1 in name }

BLOCKS :: { [ Block A0 ] } : BLOCKS BLOCK { $2 : $1 } | {- EMPTY -} { [ ] }

BLOCK :: { Block A0 }
: IF_BLOCK NEWLINE { $1 }
| INTEGER_LITERAL STATEMENT MAYBE_COMMENT NEWLINE
  { BlStatement () (getTransSpan $1 $2) (Just $1) $2 }
| STATEMENT MAYBE_COMMENT NEWLINE { BlStatement () (getSpan $1) Nothing $1 }
| interface MAYBE_EXPRESSION MAYBE_COMMENT NEWLINE SUBPROGRAM_UNITS2 MODULE_PROCEDURES INTERFACE_END MAYBE_COMMENT NEWLINE
  { BlInterface () (getTransSpan $1 $9) $2 False (reverse $5) (reverse $6) }
| interface MAYBE_EXPRESSION MAYBE_COMMENT NEWLINE MODULE_PROCEDURES INTERFACE_END MAYBE_COMMENT NEWLINE
  { BlInterface () (getTransSpan $1 $8) $2 False [ ] (reverse $5) }
| COMMENT_BLOCK { $1 }

IF_BLOCK :: { Block A0 }
IF_BLOCK
: if '(' EXPRESSION ')' then NEWLINE BLOCKS ELSE_BLOCKS
  {% let { startSpan = getSpan $1;
           (endSpan, conds, blocks, endLabel, endName) = $8;
           span = getTransSpan startSpan endSpan }
      in if Nothing == endName
         then pure $ BlIf () span Nothing Nothing ((Just $3):conds) ((reverse $7):blocks) endLabel
         else fail $ "Start and end construct names don't match for if block at " <> show span }
| id ':' if '(' EXPRESSION ')' then NEWLINE BLOCKS ELSE_BLOCKS
  {% let { TId startSpan startName = $1;
           (endSpan, conds, blocks, endLabel, endName) = $10;
           span = getTransSpan startSpan endSpan }
      in if Just startName == endName
         then pure $ BlIf () span Nothing (Just startName) ((Just $5):conds) ((reverse $9):blocks) endLabel
         else fail $ "Start and end construct names don't match for if block at " <> show span }
| INTEGER_LITERAL if '(' EXPRESSION ')' then NEWLINE BLOCKS ELSE_BLOCKS
  {% let { startSpan = getSpan $1;
           startLabel = Just $1;
           (endSpan, conds, blocks, endLabel, endName) = $9;
           span = getTransSpan startSpan endSpan }
      in if Nothing == endName
         then pure $ BlIf () span startLabel Nothing ((Just $4):conds) ((reverse $8):blocks) endLabel
         else fail $ "Start and end construct names don't match for if block at " <> show span }
| INTEGER_LITERAL id ':' if '(' EXPRESSION ')' then NEWLINE BLOCKS ELSE_BLOCKS
  {% let { startSpan = getSpan $1;
           startLabel = Just $1;
           TId _ startName = $2;
           (endSpan, conds, blocks, endLabel, endName) = $11;
           span = getTransSpan startSpan endSpan }
      in if Just startName == endName
         then pure $ BlIf () span startLabel (Just startName) ((Just $6):conds) ((reverse $10):blocks) endLabel
         else fail $ "Start and end construct names don't match for if block at " <> show span }

ELSE_BLOCKS :: { (SrcSpan, [Maybe (Expression A0)], [[Block A0]], Maybe (Expression A0), Maybe String) }
ELSE_BLOCKS
: maybe(INTEGER_LITERAL) elsif '(' EXPRESSION ')' then NEWLINE BLOCKS ELSE_BLOCKS
  { let (endSpan, conds, blocks, endLabel, endString) = $9
    in (endSpan, Just $4 : conds, reverse $8 : blocks, endLabel, endString) }
| maybe(INTEGER_LITERAL) else NEWLINE BLOCKS END_IF 
  { let (endSpan, endLabel, endId) = $5
    in (endSpan, [Nothing], [reverse $4], endLabel, endId) }
| END_IF { let (endSpan, endLabel, endId) = $1 in (endSpan, [], [], endLabel, endId) }

END_IF :: { (SrcSpan, Maybe (Expression A0), Maybe String) }
END_IF
: endif { (getSpan $1, Nothing, Nothing) }
| endif id { let TId s id = $2 in (s, Nothing, Just id) }
| INTEGER_LITERAL endif { (getSpan $2, Just $1, Nothing) }
| INTEGER_LITERAL endif id { let TId s id = $3 in (s, Just $1, Just id) }

MAYBE_EXPRESSION :: { Maybe (Expression A0) }
: EXPRESSION { Just $1 }
| {- EMPTY -} { Nothing }

MAYBE_COMMENT :: { Maybe Token }
: comment { Just $1 }
| {- EMPTY -} { Nothing }

SUBPROGRAM_UNITS2 :: { [ ProgramUnit A0 ] }
: SUBPROGRAM_UNITS SUBPROGRAM_UNIT NEWLINE { $2 : $1 }

MODULE_PROCEDURES :: { [ Block A0 ] }
: MODULE_PROCEDURES MODULE_PROCEDURE { $2 : $1 }
| MODULE_PROCEDURES MODULE_PROCEDURE COMMENT_BLOCK { $3 : $2 : $1 }
| { [ ] }

MODULE_PROCEDURE :: { Block A0 }
: moduleProcedure VARIABLES MAYBE_COMMENT NEWLINE
  { let { al = fromReverseList $2;
          st = StModuleProcedure () (getTransSpan $1 al) (fromReverseList $2) }
    in BlStatement () (getTransSpan $1 $4) Nothing st }

COMMENT_BLOCK :: { Block A0 }
: comment NEWLINE { let (TComment s c) = $1 in BlComment () s (Comment c) }

MAYBE_NEWLINE :: { Maybe Token } : NEWLINE { Just $1 } | {- EMPTY -} { Nothing }

NEWLINE :: { Token }
: NEWLINE newline { $1 }
| NEWLINE ';' { $1 }
| newline { $1 }
| ';' { $1 }

STATEMENT :: { Statement A0 }
: NONEXECUTABLE_STATEMENT { $1 }
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
| value MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StValue () (getTransSpan $1 declAList) declAList }
| volatile MAYBE_DCOLON DECLARATOR_LIST
  { let declAList = fromReverseList $3
    in StVolatile () (getTransSpan $1 declAList) declAList }
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
| external MAYBE_DCOLON VARIABLES
  { let alist = fromReverseList $3
    in StExternal () (getTransSpan $1 alist) alist }
| intrinsic MAYBE_DCOLON VARIABLES
  { let alist = fromReverseList $3
    in StIntrinsic () (getTransSpan $1 alist) alist }
| use VARIABLE { StUse () (getTransSpan $1 $2) $2 Nothing Permissive Nothing }
| use VARIABLE ',' RENAME_LIST
  { let alist = fromReverseList $4
    in StUse () (getTransSpan $1 alist) $2 Nothing Permissive (Just alist) }
| use VARIABLE ',' only ':' MAYBE_RENAME_LIST
  { StUse () (getTransSpan $1 ($5, $6)) $2 Nothing Exclusive $6 }
| entry VARIABLE MAYBE_RESULT
  { StEntry () (getTransSpan $1 $ maybe (getSpan $2) getSpan $3) $2 Nothing $3 }
| entry VARIABLE '(' ')' MAYBE_RESULT
  { StEntry () (getTransSpan $1 $ maybe (getSpan $4) getSpan $5) $2 Nothing $5 }
| entry VARIABLE '(' VARIABLES ')' MAYBE_RESULT
  { StEntry () (getTransSpan $1 $ maybe (getSpan $5) getSpan $6) $2 (Just $ fromReverseList $4) $6 }
| sequence { StSequence () (getSpan $1) }
| type ATTRIBUTE_LIST '::' id
  { let { TId span id = $4;
          alist = if null $2 then Nothing else (Just . fromReverseList) $2 }
    in StType () (getTransSpan $1 span) alist id }
| type id
  { let TId span id = $2 in StType () (getTransSpan $1 span) Nothing id }
| endType { StEndType () (getSpan $1) Nothing }
| endType id
  { let TId span id = $2 in StEndType () (getTransSpan $1 span) (Just id) }
| include STRING { StInclude () (getTransSpan $1 $2) $2 Nothing }
-- Following is a fake node to make arbitrary FORMAT statements parsable.
-- Must be fixed in the future. TODO
| format blob
  { let TBlob s blob = $2 in StFormatBogus () (getTransSpan $1 s) blob }

EXECUTABLE_STATEMENT :: { Statement A0 }
: allocate '(' DATA_REFS MAYBE_ALLOC_OPT_LIST ')'
  { StAllocate () (getTransSpan $1 $5) Nothing (fromReverseList $3) $4 }
| nullify '(' DATA_REFS ')'
  { StNullify () (getTransSpan $1 $4) (fromReverseList $3) }
| deallocate '(' DATA_REFS MAYBE_ALLOC_OPT_LIST ')'
  { StDeallocate () (getTransSpan $1 $5) (fromReverseList $3) $4 }
| EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| POINTER_ASSIGNMENT_STMT { $1 }
| where '(' EXPRESSION ')' EXPRESSION_ASSIGNMENT_STATEMENT
  { StWhere () (getTransSpan $1 $5) $3 $5 }
| where '(' EXPRESSION ')' { StWhereConstruct () (getTransSpan $1 $4) Nothing $3 }
| elsewhere '(' EXPRESSION ')' { StElsewhere () (getTransSpan $1 $4) Nothing (Just $3) }
| elsewhere { StElsewhere () (getSpan $1) Nothing Nothing }
| endwhere { StEndWhere () (getSpan $1) Nothing }
| if '(' EXPRESSION ')' INTEGER_LITERAL ',' INTEGER_LITERAL ',' INTEGER_LITERAL
  { StIfArithmetic () (getTransSpan $1 $9) $3 $5 $7 $9 }
| do { StDo () (getSpan $1) Nothing Nothing Nothing }
| id ':' do
  { let TId s id = $1
    in StDo () (getTransSpan s $3) (Just id) Nothing Nothing }
| do INTEGER_LITERAL MAYBE_COMMA DO_SPECIFICATION
  { StDo () (getTransSpan $1 $4) Nothing (Just $2) (Just $4) }
| do DO_SPECIFICATION { StDo () (getTransSpan $1 $2) Nothing Nothing (Just $2) }
| id ':' do DO_SPECIFICATION
  { let TId s id = $1
    in StDo () (getTransSpan s $4) (Just id) Nothing (Just $4) }
| do INTEGER_LITERAL MAYBE_COMMA while '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $7) Nothing (Just $2) $6 }
| do while '(' EXPRESSION ')'
  { StDoWhile () (getTransSpan $1 $5) Nothing Nothing $4 }
| id ':' do while '(' EXPRESSION ')'
  { let TId s id = $1
    in StDoWhile () (getTransSpan s $7) (Just id) Nothing $6 }
| enddo { StEnddo () (getSpan $1) Nothing }
| enddo id
  { let TId s id = $2 in StEnddo () (getTransSpan $1 s) (Just id) }
| cycle { StCycle () (getSpan $1) Nothing }
| cycle VARIABLE { StCycle () (getTransSpan $1 $2) (Just $2) }
| exit { StExit () (getSpan $1) Nothing }
| exit VARIABLE { StExit () (getTransSpan $1 $2) (Just $2) }
-- GO TO label
| goto INTEGER_LITERAL { StGotoUnconditional () (getTransSpan $1 $2) $2 }
-- GO TO label-list [,] scalar-int-expression
| goto '(' INTEGERS ')' MAYBE_COMMA EXPRESSION
  { StGotoComputed () (getTransSpan $1 $6) (fromReverseList $3) $6 }
| continue { StContinue () (getSpan $1) }
| stop { StStop () (getSpan $1) Nothing }
| stop EXPRESSION { StStop () (getTransSpan $1 $2) (Just $2) }
| selectcase '(' EXPRESSION ')'
  { StSelectCase () (getTransSpan $1 $4) Nothing $3 }
| id ':' selectcase '(' EXPRESSION ')'
  { let TId s id = $1 in StSelectCase () (getTransSpan s $6) (Just id) $5 }
| case default { StCase () (getTransSpan $1 $2) Nothing Nothing }
| case default id
  { let TId s id = $3 in StCase () (getTransSpan $1 s) (Just id) Nothing }
| case '(' INDICIES ')'
  { StCase () (getTransSpan $1 $4) Nothing (Just $ fromReverseList $3) }
| case '(' INDICIES ')' id
  { let TId s id = $5
    in StCase () (getTransSpan $1 s) (Just id) (Just $ fromReverseList $3) }
| endselect { StEndcase () (getSpan $1) Nothing }
| endselect id
  { let TId s id = $2 in StEndcase () (getTransSpan $1 s) (Just id) }
| if '(' EXPRESSION ')' EXECUTABLE_STATEMENT
  { StIfLogical () (getTransSpan $1 $5) $3 $5 }
| read CILIST IN_IOLIST
  { let alist = fromReverseList $3
    in StRead () (getTransSpan $1 alist) $2 (Just alist) }
| read CILIST { StRead () (getTransSpan $1 $2) $2 Nothing }
| read FORMAT_ID ',' IN_IOLIST
  { let alist = fromReverseList $4
    in StRead2 () (getTransSpan $1 alist) $2 (Just alist) }
| read FORMAT_ID { StRead2 () (getTransSpan $1 $2) $2 Nothing }
| write CILIST OUT_IOLIST
  { let alist = fromReverseList $3
    in StWrite () (getTransSpan $1 alist) $2 (Just alist) }
| write CILIST { StWrite () (getTransSpan $1 $2) $2 Nothing }
| print FORMAT_ID ',' OUT_IOLIST
  { let alist = fromReverseList $4
    in StPrint () (getTransSpan $1 alist) $2 (Just alist) }
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
| call VARIABLE { StCall () (getTransSpan $1 $2) $2 Nothing }
| call VARIABLE '(' ')' { StCall () (getTransSpan $1 $4) $2 Nothing }
| call VARIABLE '(' ARGUMENTS ')'
  { let alist = fromReverseList $4
    in StCall () (getTransSpan $1 $5) $2 (Just alist) }
| return { StReturn () (getSpan $1) Nothing }
| return EXPRESSION { StReturn () (getTransSpan $1 $2) (Just $2) }
| FORALL { $1 }
| END_FORALL { $1 }

ARGUMENTS :: { [ Argument A0 ] }
: ARGUMENTS ',' ARGUMENT { $3 : $1 }
| ARGUMENT { [ $1 ] }

ARGUMENT :: { Argument A0 }
: id '=' EXPRESSION
  { let TId span keyword = $1
    in Argument () (getTransSpan span $3) (Just keyword) $3 }
| EXPRESSION
  { Argument () (getSpan $1) Nothing $1 }

MAYBE_RENAME_LIST :: { Maybe (AList Use A0) }
: RENAME_LIST { Just $ fromReverseList $1 }
| {- empty -} { Nothing }

RENAME_LIST :: { [ Use A0 ] }
: RENAME_LIST ',' RENAME { $3 : $1 }
| RENAME { [ $1 ] }

RENAME :: { Use A0  }
: VARIABLE '=>' VARIABLE { UseRename () (getTransSpan $1 $3) $1 $3 }
| VARIABLE { UseID () (getSpan $1) $1 }
| operator '(' opCustom ')'
  { let TOpCustom ss op = $3
    in UseID () (getTransSpan $1 $4) (ExpValue () ss (ValOperator op)) }
| assignment { UseID () (getSpan $1) (ExpValue () (getSpan $1) ValAssignment) }

MAYBE_DCOLON :: { () } : '::' { () } | {- EMPTY -} { () }

FORMAT_ID :: { Expression A0 }
: FORMAT_ID '/' '/' FORMAT_ID %prec CONCAT
  { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| INTEGER_LITERAL { $1 }
| STRING { $1 }
| DATA_REF { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

UNIT :: { Expression A0 }
: INTEGER_LITERAL { $1 }
| DATA_REF { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

CILIST :: { AList ControlPair A0 }
: '(' CILIST_ELEMENT ',' FORMAT_ID ',' CILIST_PAIRS ')'
  { let { cp1 = ControlPair () (getSpan $2) Nothing $2;
          cp2 = ControlPair () (getSpan $4) Nothing $4;
          tail = fromReverseList $6 }
    in setSpan (getTransSpan $1 $7) $ cp1 `aCons` cp2 `aCons` tail }
| '(' CILIST_ELEMENT ',' FORMAT_ID ')'
  { let { cp1 = ControlPair () (getSpan $2) Nothing $2;
          cp2 = ControlPair () (getSpan $4) Nothing $4 }
    in AList () (getTransSpan $1 $5) [ cp1,  cp2 ] }
| '(' CILIST_ELEMENT ',' CILIST_PAIRS ')'
  { let { cp1 = ControlPair () (getSpan $2) Nothing $2;
          tail = fromReverseList $4 }
    in setSpan (getTransSpan $1 $5) $ cp1 `aCons` tail }
| '(' CILIST_ELEMENT ')'
  { let cp1 = ControlPair () (getSpan $2) Nothing $2
    in AList () (getTransSpan $1 $3) [ cp1 ] }
| '(' CILIST_PAIRS ')' { fromReverseList $2 }

CILIST_PAIRS :: { [ ControlPair A0 ] }
: CILIST_PAIRS ',' CILIST_PAIR { $3 : $1 }
| CILIST_PAIR { [ $1 ] }

CILIST_PAIR :: { ControlPair A0 }
: id '=' CILIST_ELEMENT
  { let (TId s id) = $1 in ControlPair () (getTransSpan s $3) (Just id) $3 }

CILIST_ELEMENT :: { Expression A0 }
: CI_EXPRESSION { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

CI_EXPRESSION :: { Expression A0 }
: CI_EXPRESSION '+' CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| CI_EXPRESSION '-' CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| CI_EXPRESSION '*' CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| CI_EXPRESSION '/' CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| CI_EXPRESSION '**' CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| CI_EXPRESSION '/' '/' CI_EXPRESSION %prec CONCAT
  { ExpBinary () (getTransSpan $1 $4) Concatenation $1 $4 }
| ARITHMETIC_SIGN CI_EXPRESSION %prec SIGN
  { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| CI_EXPRESSION or CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| CI_EXPRESSION and CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not CI_EXPRESSION
  { ExpUnary () (getTransSpan $1 $2) Not $2 }
| CI_EXPRESSION eqv CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) Equivalent $1 $3 }
| CI_EXPRESSION neqv CI_EXPRESSION
  { ExpBinary () (getTransSpan $1 $3) NotEquivalent $1 $3 }
| CI_EXPRESSION RELATIONAL_OPERATOR CI_EXPRESSION %prec RELATIONAL
  { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| opCustom CI_EXPRESSION %prec DEFINED_UNARY
  { let TOpCustom span str = $1
    in ExpUnary () (getTransSpan span $2) (UnCustom str) $2 }
| CI_EXPRESSION opCustom CI_EXPRESSION
  { let TOpCustom _ str = $2
    in ExpBinary () (getTransSpan $1 $3) (BinCustom str) $1 $3 }
| '(' CI_EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| INTEGER_LITERAL { $1 }
| LOGICAL_LITERAL { $1 }
| STRING { $1 }
| DATA_REF { $1 }

{- p67 ALLOCATE statement -}
MAYBE_ALLOC_OPT_LIST :: { Maybe (AList AllocOpt A0) }
: ',' stat '=' EXPRESSION { Just (fromReverseList [AOStat () (getTransSpan $2 $4) $4]) }
| {- empty -}             { Nothing }

IN_IOLIST :: { [ Expression A0 ] }
: IN_IOLIST ',' IN_IO_ELEMENT { $3 : $1}
| IN_IO_ELEMENT { [ $1 ] }

IN_IO_ELEMENT :: { Expression A0 }
: DATA_REF { $1 }
| '(' IN_IOLIST ',' DO_SPECIFICATION ')'
  { ExpImpliedDo () (getTransSpan $1 $5) (fromReverseList $2) $4 }

OUT_IOLIST :: { [ Expression A0 ] }
: OUT_IOLIST ',' EXPRESSION { $3 : $1}
| EXPRESSION { [ $1 ] }

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

MAYBE_VARIABLES :: { Maybe (AList Expression A0) }
: VARIABLES { Just $ fromReverseList $1 } | {- EMPTY -} { Nothing }

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
  { AttrDimension () (getTransSpan $1 $4) (aReverse $3) }
| external { AttrExternal () (getSpan $1) }
| intent '(' INTENT_CHOICE ')' { AttrIntent () (getTransSpan $1 $4) $3 }
| intrinsic { AttrIntrinsic () (getSpan $1) }
| optional { AttrOptional () (getSpan $1) }
| pointer { AttrPointer () (getSpan $1) }
| parameter { AttrParameter () (getSpan $1) }
| save { AttrSave () (getSpan $1) }
| target { AttrTarget () (getSpan $1) }
| value { AttrValue () (getSpan $1) }
| volatile { AttrVolatile () (getSpan $1) }

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
| DECLARATOR '=>' EXPRESSION { setInitialisation $1 $3 }
| DECLARATOR { $1 }

DECLARATOR :: { Declarator A0 }
: VARIABLE { DeclVariable () (getSpan $1) $1 Nothing Nothing }
| VARIABLE '*' EXPRESSION
  { DeclVariable () (getTransSpan $1 $3) $1 (Just $3) Nothing }
| VARIABLE '*' '(' '*' ')'
  { let star = ExpValue () (getSpan $4) ValStar
    in DeclVariable () (getTransSpan $1 $5) $1 (Just star) Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')'
  { DeclArray () (getTransSpan $1 $4) $1 (aReverse $3) Nothing Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')' '*' EXPRESSION
  { DeclArray () (getTransSpan $1 $6) $1 (aReverse $3) (Just $6) Nothing }
| VARIABLE '(' DIMENSION_DECLARATORS ')' '*' '(' '*' ')'
  { let star = ExpValue () (getSpan $7) ValStar
    in DeclArray () (getTransSpan $1 $8) $1 (aReverse $3) (Just star) Nothing }

DIMENSION_DECLARATORS :: { AList DimensionDeclarator A0 }
: DIMENSION_DECLARATORS ',' DIMENSION_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DIMENSION_DECLARATOR
  { AList () (getSpan $1) [ $1 ] }

DIMENSION_DECLARATOR :: { DimensionDeclarator A0 }
: EXPRESSION ':' EXPRESSION
  { DimensionDeclarator () (getTransSpan $1 $3) (Just $1) (Just $3) }
| EXPRESSION { DimensionDeclarator () (getSpan $1) Nothing (Just $1) }
-- Lower bound only
| EXPRESSION ':'
  { DimensionDeclarator () (getTransSpan $1 $2) (Just $1) Nothing }
| EXPRESSION ':' '*'
  { let { span = getSpan $3;
          star = ExpValue () span ValStar }
    in DimensionDeclarator () (getTransSpan $1 span) (Just $1) (Just star) }
| '*'
  { let { span = getSpan $1;
          star = ExpValue () span ValStar }
    in DimensionDeclarator () span Nothing (Just star) }
| ':'
  { let span = getSpan $1
    in DimensionDeclarator () span Nothing Nothing }

TYPE_SPEC :: { TypeSpec A0 }
: integer   KIND_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeInteger $2 }
| real      KIND_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeReal $2 }
| doublePrecision         { TypeSpec () (getSpan $1)       TypeDoublePrecision Nothing }
| complex   KIND_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeComplex $2 }
| character CHAR_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeCharacter $2 }
| logical   KIND_SELECTOR { TypeSpec () (getSpan ($1, $2)) TypeLogical $2 }
| type      '(' id ')'
  { let TId _ id = $3
    in TypeSpec () (getTransSpan $1 $4) (TypeCustom id) Nothing }

KIND_SELECTOR :: { Maybe (Selector A0) }
: '(' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $3) Nothing (Just $2) }
| '(' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) Nothing (Just $4) }
| '*' EXPRESSION -- non-standard but commonly used extension
  { Just $ Selector () (getTransSpan $1 $2) Nothing (Just $2) }
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
| '(' LEN_EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $3) (Just $2) Nothing }
| '(' len '=' LEN_EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) (Just $4) Nothing }
| '(' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) Nothing (Just $4) }
| '(' LEN_EXPRESSION ',' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $5) (Just $2) (Just $4) }
| '(' LEN_EXPRESSION ',' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $7) (Just $2) (Just $6) }
| '(' len '=' LEN_EXPRESSION ',' kind '=' EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $9) (Just $4) (Just $8) }
| '(' kind '=' EXPRESSION ',' len '=' LEN_EXPRESSION ')'
  { Just $ Selector () (getTransSpan $1 $9) (Just $8) (Just $4) }
| {- EMPTY -} { Nothing }

LEN_EXPRESSION :: { Expression A0 }
: EXPRESSION { $1 }
| '*' { ExpValue () (getSpan $1) ValStar }

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
| opCustom EXPRESSION %prec DEFINED_UNARY
  { let TOpCustom span str = $1
    in ExpUnary () (getTransSpan span $2) (UnCustom str) $2 }
| EXPRESSION opCustom EXPRESSION
  { let TOpCustom _ str = $2
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
| '*' INTEGER_LITERAL { ExpReturnSpec () (getTransSpan $1 $2) $2 }

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
| VARIABLE '(' ')'
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
| RANGE ':' EXPRESSION
  { let IxRange () s lower upper _ = $1
    in IxRange () (getTransSpan s $3) lower upper (Just $3) }
| EXPRESSION { IxSingle () (getSpan $1) Nothing $1 }
-- Following is only as an intermediate stage before having been turned into
-- an argument by later transformation.
| id '=' EXPRESSION
  { let TId s id = $1 in IxSingle () (getTransSpan $1 s) (Just id) $3 }

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

FORALL :: { Statement A0 }
: id ':' forall FORALL_HEADER {
  let (TId s1 id) = $1 in
  let (h,s2) = $4 in
  StForall () (getTransSpan s1 s2) (Just id) h
}
| forall FORALL_HEADER {
  let (h,s) = $2 in
  StForall () (getTransSpan $1 s) Nothing h
}
| forall FORALL_HEADER FORALL_ASSIGNMENT_STMT {
  let (h,_) = $2 in
  StForallStatement () (getTransSpan $1 $3) h $3
}

FORALL_HEADER
  :: { (ForallHeader A0, SrcSpan) }
FORALL_HEADER :
  -- Standard simple forall header
    '(' FORALL_TRIPLET_SPEC ')'   { (ForallHeader [$2] Nothing, getTransSpan $1 $3) }
  -- forall header with scale expression
  | '(' '(' FORALL_TRIPLET_SPEC ')' ',' EXPRESSION ')'
                                  { (ForallHeader [$3] (Just $6), getTransSpan $1 $7) }
  -- multi forall header
  | '(' FORALL_TRIPLET_SPEC_LIST_PLUS_STRIDE ')'
                                  { (ForallHeader $2 Nothing, getTransSpan $1 $3) }
  -- multi forall header with scale
  | '(' FORALL_TRIPLET_SPEC_LIST_PLUS_STRIDE ',' EXPRESSION ')'
                                  { (ForallHeader $2 (Just $4), getTransSpan $1 $5) }

FORALL_TRIPLET_SPEC_LIST_PLUS_STRIDE
  :: { [(Name, Expression A0, Expression A0, Maybe (Expression A0))] }
FORALL_TRIPLET_SPEC_LIST_PLUS_STRIDE
: '(' FORALL_TRIPLET_SPEC ')' ',' FORALL_TRIPLET_SPEC_LIST_PLUS_STRIDE { $2 : $5 }
| {- empty -}                                                          { [] }

FORALL_TRIPLET_SPEC :: { (Name, Expression A0, Expression A0, Maybe (Expression A0)) }
FORALL_TRIPLET_SPEC
: NAME '=' EXPRESSION ':' EXPRESSION { ($1, $3, $5, Nothing) }
| NAME '=' EXPRESSION ':' EXPRESSION ',' EXPRESSION { ($1, $3, $5, Just $7) }

FORALL_ASSIGNMENT_STMT :: { Statement A0 }
FORALL_ASSIGNMENT_STMT :
    EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
  | POINTER_ASSIGNMENT_STMT { $1 }

POINTER_ASSIGNMENT_STMT :: { Statement A0 }
POINTER_ASSIGNMENT_STMT :
 DATA_REF '=>' EXPRESSION { StPointerAssign () (getTransSpan $1 $3) $1 $3 }

END_FORALL :: { Statement A0 }
END_FORALL :
   endforall    { StEndForall () (getSpan $1) Nothing }
 | endforall id { let (TId s id) = $2 in StEndForall () (getTransSpan $1 s) (Just id)}

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
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValVariable s }

NUMERIC_LITERAL :: { Expression A0 }
: INTEGER_LITERAL { $1 } | REAL_LITERAL { $1 }

INTEGERS :: { [ Expression A0 ] }
: INTEGERS ',' INTEGER_LITERAL { $3 : $1 }
| INTEGER_LITERAL { [ $1 ] }

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

unitNameCheck :: Token -> String -> Parse AlexInput Token ()
unitNameCheck (TId _ name1) name2
  | name1 == name2 = return ()
  | otherwise = fail "Unit name does not match the corresponding END statement."
unitNameCheck _ _ = return ()

parse = runParse programParser
defTransforms = defaultTransformations Fortran95

fortran95Parser
    :: B.ByteString -> String -> ParseResult AlexInput Token (ProgramFile A0)
fortran95Parser = fortran95ParserWithTransforms defTransforms

fortran95ParserWithTransforms
    :: [Transformation]
    -> B.ByteString -> String -> ParseResult AlexInput Token (ProgramFile A0)
fortran95ParserWithTransforms =
    flip fortran95ParserWithModFilesWithTransforms emptyModFiles

fortran95ParserWithModFiles
    :: ModFiles
    -> B.ByteString -> String -> ParseResult AlexInput Token (ProgramFile A0)
fortran95ParserWithModFiles = fortran95ParserWithModFilesWithTransforms defTransforms

fortran95ParserWithModFilesWithTransforms
    :: [Transformation] -> ModFiles
    -> B.ByteString -> String -> ParseResult AlexInput Token (ProgramFile A0)
fortran95ParserWithModFilesWithTransforms transforms mods sourceCode filename =
    fmap (pfSetFilename filename . transformWithModFiles mods transforms) $ parse parseState
  where
    parseState = initParseState sourceCode Fortran95 filename

parseError :: Token -> LexAction a
parseError token = do
    parseState <- get
#ifdef DEBUG
    tokens <- reverse <$> aiPreviousTokensInLine <$> getAlex
#endif
    fail $ psFilename parseState ++ ": parsing failed. "
      ++ specifics token
#ifdef DEBUG
      ++ '\n' : show tokens
#endif
  where specifics (TPause _) = "\nPAUSE statements are not supported in Fortran 95 or later. "
        specifics (TAssign _) = "\nASSIGN statements are not supported in Fortran 95 or later. "
        specifics _ = ""

}
