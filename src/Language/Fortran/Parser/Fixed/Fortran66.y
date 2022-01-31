-- -*- Mode: Haskell -*-
-- vim: ft=haskell
{
module Language.Fortran.Parser.Fixed.Fortran66
  ( programParser
  , blockParser
  , statementParser
  , expressionParser
  ) where

import Language.Fortran.Version
import Language.Fortran.Util.Position
import Language.Fortran.Parser.Monad
import Language.Fortran.Parser.Fixed.Lexer
import Language.Fortran.Parser.Fixed.Utils
import Language.Fortran.AST
import Language.Fortran.AST.RealLit

import Prelude hiding ( EQ, LT, GT ) -- Same constructors exist in the AST

}

%name programParser    PROGRAM
%name blockParser      BLOCK
%name statementParser  STATEMENT
%name expressionParser EXPRESSION
%monad { LexAction }
%lexer { lexer } { TEOF _ }
%tokentype { Token }
%error { parseError }

%token
  '('                   { TLeftPar _ }
  ')'                   { TRightPar _ }
  ','                   { TComma _ }
  '.'                   { TDot _ }
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
  integer               { TType _ "integer" }
  real                  { TType _ "real" }
  doublePrecision       { TType _ "doubleprecision" }
  logical               { TType _ "logical" }
  complex               { TType _ "complex" }
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
%nonassoc RELATIONAL

%left '+' '-'
%left '*' '/'
%right NEGATION
%right '**'

%%

-- This rule is to ignore leading whitespace
PROGRAM :: { ProgramFile A0 }
: NEWLINE PROGRAM_INNER { $2 }
| PROGRAM_INNER { $1 }

PROGRAM_INNER :: { ProgramFile A0 }
: PROGRAM_UNITS BLOCKS { ProgramFile (MetaInfo { miVersion = Fortran66, miFilename = "" })  (reverse $1 ++ convCmts (reverse $2)) }
| {- empty -}   { ProgramFile (MetaInfo { miVersion = Fortran66, miFilename = "" }) [] }

PROGRAM_UNITS :: { [ ProgramUnit A0 ] }
: PROGRAM_UNITS MAIN_PROGRAM_UNIT { $2 : $1 }
| PROGRAM_UNITS BLOCKS OTHER_PROGRAM_UNIT { convCmts (reverse $2) ++ ($3 : $1) }
| MAIN_PROGRAM_UNIT { [ $1 ] }
| BLOCKS OTHER_PROGRAM_UNIT { convCmts (reverse $1) ++ [ $2 ] }

MAIN_PROGRAM_UNIT :: { ProgramUnit A0 }
: BLOCKS end MAYBE_NEWLINE
  { let blocks = reverse $1
    in PUMain () (getTransSpan $1 $2) Nothing blocks Nothing }

OTHER_PROGRAM_UNIT :: { ProgramUnit A0 }
: TYPE_SPEC function NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end MAYBE_NEWLINE
  { PUFunction () (getTransSpan $1 $7) (Just $1) emptyPrefixSuffix $3 $4 Nothing (reverse $6) Nothing }
| function NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end MAYBE_NEWLINE
  { PUFunction () (getTransSpan $1 $6) Nothing emptyPrefixSuffix $2 $3 Nothing (reverse $5) Nothing  }
| subroutine NAME MAYBE_ARGUMENTS NEWLINE BLOCKS end MAYBE_NEWLINE
  { PUSubroutine () (getTransSpan $1 $6) emptyPrefixSuffix $2 $3 (reverse $5) Nothing }
| blockData NEWLINE BLOCKS end MAYBE_NEWLINE { PUBlockData () (getTransSpan $1 $4) Nothing (reverse $3) }

MAYBE_ARGUMENTS :: { Maybe (AList Expression A0) }
: '(' MAYBE_VARIABLES ')' { $2 }
| {- Nothing -} { Nothing }

NAME :: { Name } : id { let (TId _ name) = $1 in name }

BLOCKS :: { [ Block A0 ] }
: BLOCKS BLOCK { $2 : $1 }
| {- EMPTY -}  { [ ] }

BLOCK :: { Block A0 }
: LABEL_IN_6COLUMN STATEMENT NEWLINE { BlStatement () (getTransSpan $1 $2) (Just $1) $2 }
| STATEMENT NEWLINE { BlStatement () (getSpan $1) Nothing $1 }
| comment NEWLINE { let (TComment s c) = $1 in BlComment () s (Comment c) }

MAYBE_NEWLINE :: { Maybe Token }
: NEWLINE     { Just $1 }
| {- EMPTY -} { Nothing }

NEWLINE :: { Token }
: NEWLINE newline { $1 }
| newline { $1 }

STATEMENT :: { Statement A0 }
: LOGICAL_IF_STATEMENT { $1 }
| DO_STATEMENT { $1 }
| OTHER_EXECUTABLE_STATEMENT { $1 }
| NONEXECUTABLE_STATEMENT { $1 }

LOGICAL_IF_STATEMENT :: { Statement A0 }
: if '(' EXPRESSION ')' OTHER_EXECUTABLE_STATEMENT
  { StIfLogical () (getTransSpan $1 $5) $3 $5 }

DO_STATEMENT :: { Statement A0 }
: do LABEL_IN_STATEMENT DO_SPECIFICATION
  { StDo () (getTransSpan $1 $3) Nothing (Just $2) (Just $3) }

DO_SPECIFICATION :: { DoSpecification A0 }
: EXPRESSION_ASSIGNMENT_STATEMENT ',' INT_OR_VAR ',' INT_OR_VAR
  { DoSpecification () (getTransSpan $1 $5) $1 $3 (Just $5) }
| EXPRESSION_ASSIGNMENT_STATEMENT ',' INT_OR_VAR
  { DoSpecification () (getTransSpan $1 $3) $1 $3 Nothing }

INT_OR_VAR :: { Expression A0 }
: INTEGER_LITERAL { $1 }
| VARIABLE { $1 }

OTHER_EXECUTABLE_STATEMENT :: { Statement A0 }
: EXPRESSION_ASSIGNMENT_STATEMENT { $1 }
| assign LABEL_IN_STATEMENT to VARIABLE { StLabelAssign () (getTransSpan $1 $4) $2 $4 }
| goto LABEL_IN_STATEMENT { StGotoUnconditional () (getTransSpan $1 $2) $2 }
| goto VARIABLE LABELS_IN_STATEMENT { StGotoAssigned () (getTransSpan $1 $3) $2 (Just $3) }
| goto LABELS_IN_STATEMENT VARIABLE { StGotoComputed () (getTransSpan $1 $3) $2 $3 }
| if '(' EXPRESSION ')' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT ',' LABEL_IN_STATEMENT { StIfArithmetic () (getTransSpan $1 $9) $3 $5 $7 $9 }
| call VARIABLE ARGUMENTS
  { StCall () (getTransSpan $1 $3) $2 (Just $3) }
| call VARIABLE { StCall () (getTransSpan $1 $2) $2 Nothing }
| return { StReturn () (getSpan $1) Nothing }
| continue { StContinue () $ getSpan $1 }
| stop INTEGER_LITERAL { StStop () (getTransSpan $1 $2) $ Just $2 }
| stop { StStop () (getSpan $1) Nothing }
| pause INTEGER_LITERAL { StPause () (getTransSpan $1 $2) $ Just $2 }
| pause { StPause () (getSpan $1) Nothing }
| rewind UNIT { StRewind2 () (getTransSpan $1 $2) $2 }
| backspace UNIT { StBackspace2 () (getTransSpan $1 $2) $2 }
| endfile UNIT { StEndfile2 () (getTransSpan $1 $2) $2 }
| write READ_WRITE_ARGUMENTS { let (cilist, iolist) = $2 in StWrite () (getTransSpan $1 $2) cilist iolist }
| read READ_WRITE_ARGUMENTS { let (cilist, iolist) = $2 in StRead () (getTransSpan $1 $2) cilist iolist }

EXPRESSION_ASSIGNMENT_STATEMENT :: { Statement A0 }
: ELEMENT '=' EXPRESSION { StExpressionAssign () (getTransSpan $1 $3) $1 $3 }

NONEXECUTABLE_STATEMENT :: { Statement A0 }
: external FUNCTION_NAMES { StExternal () (getTransSpan $1 $2) (aReverse $2) }
| dimension ARRAY_DECLARATORS { StDimension () (getTransSpan $1 $2) (aReverse $2) }
| common COMMON_GROUPS { StCommon () (getTransSpan $1 $2) (aReverse $2) }
| equivalence EQUIVALENCE_GROUPS { StEquivalence () (getTransSpan $1 $2) (aReverse $2) }
| data DATA_GROUPS { StData () (getTransSpan $1 $2) (aReverse $2) }
-- Following is a fake node to make arbitrary FORMAT statements parsable.
-- Must be fixed in the future. TODO
| format blob
  { let TBlob s blob = $2 in StFormatBogus () (getTransSpan $1 s) blob }
| TYPE_SPEC DECLARATORS { StDeclaration () (getTransSpan $1 $2) $1 Nothing (aReverse $2) }

READ_WRITE_ARGUMENTS :: { (AList ControlPair A0, Maybe (AList Expression A0)) }
: '(' UNIT ')' IO_ELEMENTS { (AList () (getSpan $2) [ ControlPair () (getSpan $2) Nothing $2 ], Just (aReverse $4)) }
| '(' UNIT ',' FORM ')' IO_ELEMENTS { (AList () (getTransSpan $2 $4) [ ControlPair () (getSpan $2) Nothing $2, ControlPair () (getSpan $4) Nothing $4 ], Just (aReverse $6)) }
| '(' UNIT ')' { (AList () (getSpan $2) [ ControlPair () (getSpan $2) Nothing $2 ], Nothing) }
| '(' UNIT ',' FORM ')' { (AList () (getTransSpan $2 $4) [ ControlPair () (getSpan $2) Nothing $2, ControlPair () (getSpan $4) Nothing $4 ], Nothing) }

-- Not my terminology a VAR or an INT (probably positive) is defined as UNIT.
UNIT :: { Expression A0 }
: INTEGER_LITERAL { $1 }
| VARIABLE { $1 }

FORM :: { Expression A0 }
: VARIABLE { $1 }
| LABEL_IN_STATEMENT { $1 }

IO_ELEMENTS :: { AList Expression A0 }
: IO_ELEMENTS ',' IO_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| IO_ELEMENT { AList () (getSpan $1) [ $1 ] }

IO_ELEMENT :: { Expression A0 }
: VARIABLE { $1 }
-- There should also be a caluse for variable names but not way to
-- differentiate it at this stage from VARIABLE. Hence, it is omitted to prevent
-- reduce/reduce conflict.
| SUBSCRIPT { $1 }
| '(' IO_ELEMENTS ',' DO_SPECIFICATION ')' { ExpImpliedDo () (getTransSpan $1 $5) $2 $4 }

ELEMENT :: { Expression A0 }
: VARIABLE { $1 }
| SUBSCRIPT { $1 }

DATA_GROUPS :: { AList DataGroup A0 }
: DATA_GROUPS ',' NAME_LIST  '/' DATA_ITEMS '/' { setSpan (getTransSpan $1 $6) $ (DataGroup () (getTransSpan $3 $6) (aReverse $3) (aReverse $5)) `aCons` $1 }
| NAME_LIST  '/' DATA_ITEMS '/' { AList () (getTransSpan $1 $4) [ DataGroup () (getTransSpan $1 $4) (aReverse $1) (aReverse $3) ] }

DATA_ITEMS :: { AList Expression A0 }
: DATA_ITEMS ',' DATA_ITEM { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1}
| DATA_ITEM { AList () (getSpan $1) [ $1 ] }

DATA_ITEM :: { Expression A0 }
: INTEGER_LITERAL '*' DATA_ITEM_LEVEL1 { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| DATA_ITEM_LEVEL1 { $1 }

DATA_ITEM_LEVEL1 :: { Expression A0 }
: SIGNED_NUMERIC_LITERAL  { $1 }
| COMPLEX_LITERAL         { $1 }
| LOGICAL_LITERAL         { $1 }
| HOLLERITH               { $1 }

EQUIVALENCE_GROUPS :: { AList (AList Expression) A0 }
: EQUIVALENCE_GROUPS ','  '(' NAME_LIST ')' { setSpan (getTransSpan $1 $5) $ (setSpan (getTransSpan $3 $5) $ aReverse $4) `aCons` $1 }
| '(' NAME_LIST ')' { let s = (getTransSpan $1 $3) in AList () s [ setSpan s $ aReverse $2 ] }

COMMON_GROUPS :: { AList CommonGroup A0 }
: COMMON_GROUPS COMMON_GROUP { setSpan (getTransSpan $1 $2) $ $2 `aCons` $1 }
| INIT_COMMON_GROUP { AList () (getSpan $1) [ $1 ] }

COMMON_GROUP :: { CommonGroup A0 }
: COMMON_NAME DECLARATORS
  { CommonGroup () (getTransSpan $1 $2) (Just $1) $ aReverse $2 }
| '/' '/' DECLARATORS { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }

INIT_COMMON_GROUP :: { CommonGroup A0 }
: COMMON_NAME DECLARATORS
  { CommonGroup () (getTransSpan $1 $2) (Just $1) $ aReverse $2 }
| '/' '/' DECLARATORS { CommonGroup () (getTransSpan $1 $3) Nothing $ aReverse $3 }
| DECLARATORS { CommonGroup () (getSpan $1) Nothing $ aReverse $1 }

COMMON_NAME :: { Expression A0 }
: '/' VARIABLE '/' { setSpan (getTransSpan $1 $3) $2 }

NAME_LIST :: { AList Expression A0 }
: NAME_LIST ',' NAME_LIST_ELEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| NAME_LIST_ELEMENT { AList () (getSpan $1) [ $1 ] }

NAME_LIST_ELEMENT :: { Expression A0 }
: VARIABLE { $1 }
| SUBSCRIPT { $1 }

-- Note that declarator lists in the F66 parser don't have initializers.
DECLARATORS :: { AList Declarator A0 }
: DECLARATORS ',' DECLARATOR { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DECLARATOR { AList () (getSpan $1) [ $1 ] }

DECLARATOR :: { Declarator A0 }
: ARRAY_DECLARATOR    { $1 }
| VARIABLE_DECLARATOR { $1 }

ARRAY_DECLARATORS :: { AList Declarator A0 }
: ARRAY_DECLARATORS ',' ARRAY_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| ARRAY_DECLARATOR
  { AList () (getSpan $1) [ $1 ] }

ARRAY_DECLARATOR :: { Declarator A0 }
: VARIABLE '(' DIMENSION_DECLARATORS ')'
  { Declarator () (getTransSpan $1 $4) $1 (ArrayDecl (aReverse $3)) Nothing Nothing }

DIMENSION_DECLARATORS :: { AList DimensionDeclarator A0 }
: DIMENSION_DECLARATORS ',' DIMENSION_DECLARATOR
  { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| DIMENSION_DECLARATOR
  { AList () (getSpan $1) [ $1 ] }

DIMENSION_DECLARATOR :: { DimensionDeclarator A0 }
: EXPRESSION { DimensionDeclarator () (getSpan $1) Nothing (Just $1) }

VARIABLE_DECLARATOR :: { Declarator A0 }
: VARIABLE { Declarator () (getSpan $1) $1 ScalarDecl Nothing Nothing }

-- Here the procedure should be either a function or subroutine name, but
-- since they are syntactically identical at this stage subroutine names
-- are also emitted as function names.
FUNCTION_NAMES :: { AList Expression A0 }
: FUNCTION_NAMES ',' VARIABLE { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| VARIABLE { AList () (getSpan $1) [ $1 ] }

ARGUMENTS :: { AList Argument A0 }
:  ARGUMENTS_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

ARGUMENTS_LEVEL1 :: { AList Argument A0 }
: ARGUMENTS_LEVEL1 ',' CALLABLE_EXPRESSION { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' CALLABLE_EXPRESSION { AList () (getTransSpan $1 $2) [ $2 ] }
| '(' { AList () (getSpan $1) [ ] }

-- Expression all by itself subsumes all other callable expressions.
CALLABLE_EXPRESSION :: { Argument A0 }
: HOLLERITH   { Argument () (getSpan $1) Nothing (ArgExpr $1) }
| '(' VARIABLE ')'
  { let ExpValue _ _ (ValVariable v) = $2
     in Argument () (getTransSpan $1 $3) Nothing (ArgExprVar () (getSpan $2) v) }
| EXPRESSION  { Argument () (getSpan $1) Nothing (ArgExpr $1) }

EXPRESSION :: { Expression A0 }
: EXPRESSION '+' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Addition $1 $3 }
| EXPRESSION '-' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Subtraction $1 $3 }
| EXPRESSION '*' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Multiplication $1 $3 }
| EXPRESSION '/' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Division $1 $3 }
| EXPRESSION '**' EXPRESSION { ExpBinary () (getTransSpan $1 $3) Exponentiation $1 $3 }
| ARITHMETIC_SIGN EXPRESSION %prec NEGATION { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| EXPRESSION or EXPRESSION { ExpBinary () (getTransSpan $1 $3) Or $1 $3 }
| EXPRESSION and EXPRESSION { ExpBinary () (getTransSpan $1 $3) And $1 $3 }
| not EXPRESSION { ExpUnary () (getTransSpan $1 $2) Not $2 }
| EXPRESSION RELATIONAL_OPERATOR EXPRESSION %prec RELATIONAL { ExpBinary () (getTransSpan $1 $3) $2 $1 $3 }
| '(' EXPRESSION ')' { setSpan (getTransSpan $1 $3) $2 }
| INTEGER_LITERAL               { $1 }
| REAL_LITERAL                  { $1 }
| COMPLEX_LITERAL               { $1 }
| LOGICAL_LITERAL               { $1 }
| SUBSCRIPT                     { $1 }
-- There should be FUNCTION_CALL here but as far as the parser is concerned it is same as SUBSCRIPT,
-- hence putting it here would cause a reduce/reduce conflict.
| VARIABLE                      { $1 }

RELATIONAL_OPERATOR :: { BinaryOp }
: '=='  { EQ }
| '!='  { NE }
| '>'   { GT }
| '>='  { GTE }
| '<'   { LT }
| '<='  { LTE }

SUBSCRIPT :: { Expression A0 }
: VARIABLE '(' ')'
  { ExpFunctionCall () (getTransSpan $1 $3) $1 Nothing }
| VARIABLE '(' INDICIES ')'
  { ExpSubscript () (getTransSpan $1 $4) $1 (fromReverseList $3) }

INDICIES :: { [ Index A0 ] }
: INDICIES ',' EXPRESSION { IxSingle () (getSpan $3) Nothing $3 : $1 }
| EXPRESSION { [ IxSingle () (getSpan $1) Nothing $1 ] }

ARITHMETIC_SIGN :: { (SrcSpan, UnaryOp) }
: '-' { (getSpan $1, Minus) }
| '+' { (getSpan $1, Plus) }

MAYBE_VARIABLES :: { Maybe (AList Expression A0) }
: VARIABLES { Just $ fromReverseList $1 } | {- EMPTY -} { Nothing }

VARIABLES :: { [ Expression A0 ] }
: VARIABLES ',' VARIABLE { $3 : $1 } | VARIABLE { [ $1 ] }

-- This may also be used to parse a function name, or an array name. Since when
-- are valid options in a production there is no way of differentiating them at
-- this stage.
-- This at least reduces reduce/reduce conflicts.
VARIABLE :: { Expression A0 }
: id { ExpValue () (getSpan $1) $ let (TId _ s) = $1 in ValVariable s }

SIGNED_INTEGER_LITERAL :: { Expression A0 }
: ARITHMETIC_SIGN INTEGER_LITERAL { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| INTEGER_LITERAL { $1 }

INTEGER_LITERAL :: { Expression A0 }
: int { ExpValue () (getSpan $1) $ let (TInt _ i) = $1 in ValInteger i Nothing }

SIGNED_REAL_LITERAL :: { Expression A0 }
: ARITHMETIC_SIGN REAL_LITERAL { ExpUnary () (getTransSpan (fst $1) $2) (snd $1) $2 }
| REAL_LITERAL { $1 }

REAL_LITERAL :: { Expression A0 }
: int EXPONENT { makeRealLit (Just $1) Nothing Nothing (Just $2) }
| int '.' MAYBE_EXPONENT { makeRealLit (Just $1) (Just $2) Nothing $3 }
| '.' int MAYBE_EXPONENT { makeRealLit Nothing (Just $1) (Just $2) $3 }
| int '.' int MAYBE_EXPONENT { makeRealLit (Just $1) (Just $2) (Just $3) $4 }

MAYBE_EXPONENT :: { Maybe (SrcSpan, String) }
: EXPONENT { Just $1 }
| {-EMPTY-} { Nothing }

EXPONENT :: { (SrcSpan, String) }
: exponent { let (TExponent s exp) = $1 in (s, exp) }

SIGNED_NUMERIC_LITERAL :: { Expression A0 }
: SIGNED_INTEGER_LITERAL { $1 }
| SIGNED_REAL_LITERAL    { $1 }

COMPLEX_LITERAL :: { Expression A0 }
:  '(' SIGNED_NUMERIC_LITERAL ',' SIGNED_NUMERIC_LITERAL ')' { ExpValue () (getTransSpan $1 $5) (ValComplex $2 $4)}

LOGICAL_LITERAL :: { Expression A0 }
: bool { let TBool s b = $1 in ExpValue () s $ ValLogical b Nothing }

HOLLERITH :: { Expression A0 }
: hollerith { ExpValue () (getSpan $1) $ let (THollerith _ h) = $1 in ValHollerith h }

LABELS_IN_STATEMENT :: { AList Expression A0 }
: LABELS_IN_STATEMENT_LEVEL1 ')' { setSpan (getTransSpan $1 $2) $ aReverse $1 }

LABELS_IN_STATEMENT_LEVEL1 :: { AList Expression A0 }
: LABELS_IN_STATEMENT_LEVEL1 ',' LABEL_IN_STATEMENT { setSpan (getTransSpan $1 $3) $ $3 `aCons` $1 }
| '(' LABEL_IN_STATEMENT { AList () (getTransSpan $1 $2) [ $2 ] }

-- Labels that occur in the first 6 columns
LABEL_IN_6COLUMN :: { Expression A0 }
: label { ExpValue () (getSpan $1) (let (TLabel _ l) = $1 in ValInteger l Nothing) }

-- Labels that occur in statements
LABEL_IN_STATEMENT :: { Expression A0 }
: int { ExpValue () (getSpan $1) (let (TInt _ l) = $1 in ValInteger l Nothing) }

TYPE_SPEC :: { TypeSpec A0 }
: integer           { TypeSpec () (getSpan $1) TypeInteger Nothing }
| real              { TypeSpec () (getSpan $1) TypeReal Nothing }
| doublePrecision   { TypeSpec () (getSpan $1) TypeDoublePrecision Nothing }
| logical           { TypeSpec () (getSpan $1) TypeLogical Nothing }
| complex           { TypeSpec () (getSpan $1) TypeComplex Nothing }
