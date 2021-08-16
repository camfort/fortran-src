{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- This module holds the data types used to represent Fortran code of various
-- versions.
--
-- fortran-src supports Fortran 66 through to Fortran 2003, and uses the same
-- types to represent them. The Fortran standard was largely refined as it grew,
-- often assimilating popular compiler extensions for the previous standard. We
-- try to be as permissible as reasonable when parsing; similarly, this AST
-- keeps close to the syntax, and includes statements, expressions, types etc.
-- only applicable to certain (newer) versions of Fortran.
--
-- Useful Fortran standard references:
--
--   * Fortran 77 ANSI standard: ANSI X3.9-1978
--   * Fortran 90 ANSI standard: ANSI X3.198-1992 (also ISO/IEC 1539:1991)
--   * Fortran 90 Handbook (J. Adams)
--
-- (The Fortran 66 ANSI standard lacks detail, and isn't as useful as the later
-- standards for implementing the language.)
--
-- /Note:/ some comments aren't reflected in the Haddock documentation, so you
-- may also wish to view this file's source.

module Language.Fortran.AST
  (
  -- * AST nodes and types
  -- ** Statements and expressions
    ProgramFile(..)
  , ProgramUnit(..)
  , Block(..)
  , Statement(..)
  , Expression(..)
  , Index(..)
  , Value(..)
  , UnaryOp(..)
  , BinaryOp(..)

  -- ** Types and declarations
  , Name
  , BaseType(..)
  , TypeSpec(..)
  , Declarator(..)
  , Selector(..)
  , DimensionDeclarator(..)

  -- ** Annotated node list (re-export)
  , module Language.Fortran.AST.AList

  -- ** Other
  , Attribute(..)
  , Prefix(..)
  , Suffix(..)
  , ProcDecl(..)
  , ProcInterface(..)
  , Comment(..)
  , ForallHeader(..)
  , Only(..)
  , MetaInfo(..)
  , Prefixes
  , Suffixes
  , PrefixSuffix
  , ModuleNature(..)
  , Use(..)
  , Argument(..)
  , Intent(..)
  , ControlPair(..)
  , AllocOpt(..)
  , ImpList(..)
  , ImpElement(..)
  , CommonGroup(..)
  , Namelist(..)
  , DataGroup(..)
  , StructureItem(..)
  , UnionMap(..)
  , FormatItem(..)
  , FlushSpec(..)
  , DoSpecification(..)
  , ProgramUnitName(..)
  , Kind

  -- * Node annotations & related typeclasses
  , A0
  , Annotated(..)
  , Labeled(..)
  , Named(..)

  -- * Helpers
  , validPrefixSuffix
  , emptyPrefixes
  , emptySuffixes
  , emptyPrefixSuffix
  , nonExecutableStatement
  , nonExecutableStatementBlock
  , executableStatement
  , executableStatementBlock
  , setInitialisation

  -- ** Assorted getters & setters
  , pfSetFilename
  , pfGetFilename
  , programUnitBody
  , updateProgramUnitBody
  , programUnitSubprograms

  ) where

import Prelude hiding (init)
import Data.Data
import Data.Generics.Uniplate.Data ()
import Data.Typeable ()
import Data.Binary
import Control.DeepSeq
import Text.PrettyPrint.GenericPretty
import Language.Fortran.Version (FortranVersion(..))

import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter
import Language.Fortran.AST.AList

import Data.Generics.Uniplate.Direct
import Debug.Trace

-- | The empty annotation.
type A0 = ()

type Name = String

--------------------------------------------------------------------------------
-- Basic AST nodes
--------------------------------------------------------------------------------

-- | Type name referenced in syntax.
--
-- In many Fortran specs and compilers, certain types are actually "synonyms"
-- for other types with specified kinds. The primary example is DOUBLE PRECISION
-- being equivalent to REAL(8). Type kinds were introduced in Fortran 90, and it
-- should be safe to replace all instances of DOUBLE PRECISION with REAL(8) in
-- Fortran 90 code. However, type kinds weren't present in (standard) Fortran
-- 77, so this equivalence was detached from the user.
--
-- In any case, it's unclear how strong the equivalence is and whether it can
-- be retroactively applied to previous standards. We choose to parse types
-- directly, and handle those transformations during type analysis, where we
-- assign most scalars a kind (see 'Analysis.SemType').
data BaseType =
    TypeInteger
  | TypeReal
  | TypeDoublePrecision
  | TypeComplex
  | TypeDoubleComplex
  | TypeLogical
  | TypeCharacter
  | TypeCustom String
  | ClassStar
  | ClassCustom String
  | TypeByte
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary BaseType

-- | The type specification of a declaration statement, containing the syntactic
--   type name and kind selector.
--
-- See HP's F90 spec pg.24.
data TypeSpec a = TypeSpec a SrcSpan BaseType (Maybe (Selector a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- | The "kind selector" of a declaration statement.
--
-- HP's F90 spec (pg.24) actually differentiates between "kind selectors" and
-- "char selectors", where char selectors can specify a length (alongside kind),
-- and the default meaning of an unlabelled kind parameter (the 8 in INTEGER(8))
-- is length instead of kind. We handle this correctly in the parsers, but place
-- both into this 'Selector' type.
--
-- The upshot is, length is invalid for non-CHARACTER types, and the parser
-- guarantees that it will be Nothing. For CHARACTER types, both maybe or may
-- not be present.
data Selector a =
  Selector a SrcSpan
    (Maybe (Expression a)) -- ^ length (if present)
    (Maybe (Expression a)) -- ^ kind (if present)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

type Kind = Int

data MetaInfo = MetaInfo { miVersion :: FortranVersion, miFilename :: String }
  deriving (Eq, Show, Data, Typeable, Generic)

-- Program structure definition
data ProgramFile a = ProgramFile MetaInfo [ ProgramUnit a ]
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

pfSetFilename :: String -> ProgramFile a -> ProgramFile a
pfSetFilename fn (ProgramFile mi pus) = ProgramFile (mi { miFilename = fn }) pus
pfGetFilename :: ProgramFile a -> String
pfGetFilename (ProgramFile mi _) = miFilename mi

data ProgramUnit a =
    PUMain
      a SrcSpan
      (Maybe Name) -- Program name
      [Block a] -- Body
      (Maybe [ProgramUnit a]) -- Subprograms
  | PUModule
      a SrcSpan
      Name -- Program name
      [Block a] -- Body
      (Maybe [ProgramUnit a]) -- Subprograms
  | PUSubroutine
      a SrcSpan
      (PrefixSuffix a) -- Subroutine options
      Name
      (Maybe (AList Expression a)) -- Arguments
      [Block a] -- Body
      (Maybe [ProgramUnit a]) -- Subprograms
  | PUFunction
      a SrcSpan
      (Maybe (TypeSpec a)) -- Return type
      (PrefixSuffix a) -- Function Options
      Name
      (Maybe (AList Expression a)) -- Arguments
      (Maybe (Expression a)) -- Result
      [Block a] -- Body
      (Maybe [ProgramUnit a]) -- Subprograms
  | PUBlockData
      a SrcSpan
      (Maybe Name)
      [Block a] -- Body
  | PUComment a SrcSpan (Comment a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

type Prefixes a = Maybe (AList Prefix a)
type Suffixes a = Maybe (AList Suffix a)
type PrefixSuffix a = (Prefixes a, Suffixes a)

emptyPrefixes :: Prefixes a
emptyPrefixes = Nothing

emptySuffixes :: Suffixes a
emptySuffixes = Nothing

emptyPrefixSuffix :: PrefixSuffix a
emptyPrefixSuffix = (emptyPrefixes, emptySuffixes)

data Prefix a = PfxRecursive a SrcSpan
              | PfxElemental a SrcSpan
              | PfxPure a SrcSpan
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- see C1241 & C1242 (Fortran2003)
validPrefixSuffix :: PrefixSuffix a -> Bool
validPrefixSuffix (mpfxs, msfxs) =
  not (any isElem pfxs) || (not (any isRec pfxs) && not (any isBind sfxs))
  where
    isElem (PfxElemental {}) = True; isElem _ = False
    isRec  (PfxRecursive {}) = True; isRec _  = False
    isBind (SfxBind {})      = True
    pfxs = aStrip' mpfxs
    sfxs = aStrip' msfxs

data Suffix a = SfxBind a SrcSpan (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

programUnitBody :: ProgramUnit a -> [Block a]
programUnitBody (PUMain _ _ _ bs _)              = bs
programUnitBody (PUModule _ _ _ bs _)            = bs
programUnitBody (PUSubroutine _ _ _ _ _ bs _)    = bs
programUnitBody (PUFunction _ _ _ _ _ _ _ bs _)  = bs
programUnitBody (PUBlockData _ _ _ bs)           = bs
programUnitBody PUComment{}                   = []

updateProgramUnitBody :: ProgramUnit a -> [Block a] -> ProgramUnit a
updateProgramUnitBody (PUMain a s n _ pu)   bs' =
    PUMain a s n bs' pu
updateProgramUnitBody (PUModule a s n _ pu) bs' =
    PUModule a s n bs' pu
updateProgramUnitBody (PUSubroutine a s f n args _ pu) bs' =
    PUSubroutine a s f n args bs' pu
updateProgramUnitBody (PUFunction a s t f n args res _ pu) bs' =
    PUFunction a s t f n args res bs' pu
updateProgramUnitBody (PUBlockData a s n _) bs' =
    PUBlockData a s n bs'
updateProgramUnitBody p@PUComment{} _ = p

programUnitSubprograms :: ProgramUnit a -> Maybe [ProgramUnit a]
programUnitSubprograms (PUMain _ _ _ _ s)             = s
programUnitSubprograms (PUModule _ _ _ _ s)           = s
programUnitSubprograms (PUSubroutine _ _ _ _ _ _ s)   = s
programUnitSubprograms (PUFunction _ _ _ _ _ _ _ _ s) = s
programUnitSubprograms PUBlockData{}               = Nothing
programUnitSubprograms PUComment{}                 = Nothing

newtype Comment a = Comment String
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Block a =
    BlStatement a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Statement a)                -- ^ Statement

  | BlForall    a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (ForallHeader a)             -- ^ Header information
                [ Block a ]                  -- ^ Body
                (Maybe (Expression a))       -- ^ Label to END DO

  | BlIf        a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                [ Maybe (Expression a) ]     -- ^ Conditions
                [ [ Block a ] ]              -- ^ Bodies
                (Maybe (Expression a))       -- ^ Label to END IF

  | BlCase      a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (Expression a)               -- ^ Scrutinee
                [ Maybe (AList Index a) ]    -- ^ Case ranges
                [ [ Block a ] ]              -- ^ Bodies
                (Maybe (Expression a))       -- ^ Label to END SELECT

  | BlDo        a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (Maybe (Expression a))       -- ^ Target label
                (Maybe (DoSpecification a))  -- ^ Do Specification
                [ Block a ]                  -- ^ Body
                (Maybe (Expression a))       -- ^ Label to END DO

  | BlDoWhile   a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (Maybe (Expression a))       -- ^ Target label
                (Expression a)               -- ^ Condition
                [ Block a ]                  -- ^ Body
                (Maybe (Expression a))       -- ^ Label to END DO

  | BlAssociate a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                (AList (ATuple Expression Expression) a) -- Expression abbreviations
                [ Block a ]                  -- Body
                (Maybe (Expression a))       -- Label to END IF
  -- ^ The first 'Expression' in the abbreviation tuple is always an
  --   @ExpValue _ _ (ValVariable id)@. Also guaranteed nonempty.

  | BlInterface a SrcSpan
                (Maybe (Expression a))       -- ^ label
                Bool                         -- ^ abstract?
                [ ProgramUnit a ]            -- ^ Routine decls. in the interface
                [ Block a ]                  -- ^ Module procedures

  | BlComment a SrcSpan (Comment a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Statement a  =
    StDeclaration         a SrcSpan (TypeSpec a) (Maybe (AList Attribute a)) (AList Declarator a)
  | StStructure           a SrcSpan (Maybe String) (AList StructureItem a)
  | StIntent              a SrcSpan Intent (AList Expression a)
  | StOptional            a SrcSpan (AList Expression a)
  | StPublic              a SrcSpan (Maybe (AList Expression a))
  | StPrivate             a SrcSpan (Maybe (AList Expression a))
  | StProtected           a SrcSpan (Maybe (AList Expression a))
  | StSave                a SrcSpan (Maybe (AList Expression a))
  | StDimension           a SrcSpan (AList Declarator a)
  | StAllocatable         a SrcSpan (AList Declarator a)
  | StAsynchronous        a SrcSpan (AList Declarator a)
  | StPointer             a SrcSpan (AList Declarator a)
  | StTarget              a SrcSpan (AList Declarator a)
  | StValue               a SrcSpan (AList Declarator a)
  | StVolatile            a SrcSpan (AList Declarator a)
  | StData                a SrcSpan (AList DataGroup a)
  | StAutomatic           a SrcSpan (AList Declarator a)
  | StStatic              a SrcSpan (AList Declarator a)
  | StNamelist            a SrcSpan (AList Namelist a)
  | StParameter           a SrcSpan (AList Declarator a)
  | StExternal            a SrcSpan (AList Expression a)
  | StIntrinsic           a SrcSpan (AList Expression a)
  | StCommon              a SrcSpan (AList CommonGroup a)
  -- ^ A COMMON statement, defining a list of common blocks.
  | StEquivalence         a SrcSpan (AList (AList Expression) a)
  | StFormat              a SrcSpan (AList FormatItem a)
  | StImplicit            a SrcSpan (Maybe (AList ImpList a))
  | StEntry               a SrcSpan (Expression a) (Maybe (AList Expression a)) (Maybe (Expression a))

  | StInclude             a SrcSpan (Expression a) (Maybe [Block a])
  -- ^ Nothing indicates an yet-to-be processed include. (The F77 parser parses
  -- Nothing, then fills out each include statement in a post-parse step.)

  | StDo                  a SrcSpan (Maybe String) (Maybe (Expression a)) (Maybe (DoSpecification a))
  | StDoWhile             a SrcSpan (Maybe String) (Maybe (Expression a)) (Expression a)
  | StEnddo               a SrcSpan (Maybe String)
  | StCycle               a SrcSpan (Maybe (Expression a))
  | StExit                a SrcSpan (Maybe (Expression a))
  | StIfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)
  | StSelectCase          a SrcSpan (Maybe String) (Expression a)
  | StCase                a SrcSpan (Maybe String) (Maybe (AList Index a))
  | StEndcase             a SrcSpan (Maybe String)
  | StFunction            a SrcSpan (Expression a) (AList Expression a) (Expression a)
  | StExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | StPointerAssign       a SrcSpan (Expression a) (Expression a)
  | StLabelAssign         a SrcSpan (Expression a) (Expression a)
  | StGotoUnconditional   a SrcSpan (Expression a)
  | StGotoAssigned        a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StGotoComputed        a SrcSpan (AList Expression a) (Expression a)
  | StCall                a SrcSpan (Expression a) (Maybe (AList Argument a))
  | StReturn              a SrcSpan (Maybe (Expression a))
  | StContinue            a SrcSpan
  | StStop                a SrcSpan (Maybe (Expression a))
  | StPause               a SrcSpan (Maybe (Expression a))
  | StRead                a SrcSpan (AList ControlPair a) (Maybe (AList Expression a))
  | StRead2               a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StWrite               a SrcSpan (AList ControlPair a) (Maybe (AList Expression a))
  | StPrint               a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StTypePrint           a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StOpen                a SrcSpan (AList ControlPair a)
  | StClose               a SrcSpan (AList ControlPair a)
  | StFlush               a SrcSpan (AList FlushSpec a)
  | StInquire             a SrcSpan (AList ControlPair a)
  | StRewind              a SrcSpan (AList ControlPair a)
  | StRewind2             a SrcSpan (Expression a)
  | StBackspace           a SrcSpan (AList ControlPair a)
  | StBackspace2          a SrcSpan (Expression a)
  | StEndfile             a SrcSpan (AList ControlPair a)
  | StEndfile2            a SrcSpan (Expression a)
  | StAllocate            a SrcSpan (Maybe (TypeSpec a)) (AList Expression a) (Maybe (AList AllocOpt a))
  | StNullify             a SrcSpan (AList Expression a)
  | StDeallocate          a SrcSpan (AList Expression a) (Maybe (AList AllocOpt a))
  | StWhere               a SrcSpan (Expression a) (Statement a)
  | StWhereConstruct      a SrcSpan (Maybe String) (Expression a)
  | StElsewhere           a SrcSpan (Maybe String) (Maybe (Expression a))
  | StEndWhere            a SrcSpan (Maybe String)
  | StUse                 a SrcSpan (Expression a) (Maybe ModuleNature) Only (Maybe (AList Use a))
  | StModuleProcedure     a SrcSpan (AList Expression a)
  | StProcedure           a SrcSpan (Maybe (ProcInterface a)) (Maybe (Attribute a)) (AList ProcDecl a)
  | StType                a SrcSpan (Maybe (AList Attribute a)) String
  | StEndType             a SrcSpan (Maybe String)
  | StSequence            a SrcSpan
  | StForall              a SrcSpan (Maybe String) (ForallHeader a)
  | StForallStatement     a SrcSpan (ForallHeader a) (Statement a)
  | StEndForall           a SrcSpan (Maybe String)
  | StImport              a SrcSpan (AList Expression a)
  | StEnum                a SrcSpan
  | StEnumerator          a SrcSpan (AList Declarator a)
  | StEndEnum             a SrcSpan
  -- Following is a temporary solution to a complicated FORMAT statement
  -- parsing problem.
  | StFormatBogus         a SrcSpan String
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- R1214 proc-decl is procedure-entity-name [=> null-init]
data ProcDecl a = ProcDecl a SrcSpan (Expression a) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- R1212 proc-interface is interface-name or declaration-type-spec
data ProcInterface a = ProcInterfaceName a SrcSpan (Expression a)
                     | ProcInterfaceType a SrcSpan (TypeSpec a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data ForallHeader a = ForallHeader
    -- List of tuples: index-name, start subscript, end subscript, optional stride
    [(Name, Expression a, Expression a, Maybe (Expression a))]
    -- An optional expression for scaling
    (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Only = Exclusive | Permissive
  deriving (Eq, Show, Data, Typeable, Generic)

data ModuleNature = ModIntrinsic | ModNonIntrinsic
  deriving (Eq, Show, Data, Typeable, Generic)

data Use a =
    UseRename a SrcSpan (Expression a) (Expression a)
  | UseID a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Argument a = Argument a SrcSpan (Maybe String) (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Attribute a =
    AttrAllocatable a SrcSpan
  | AttrAsynchronous a SrcSpan
  | AttrDimension a SrcSpan (AList DimensionDeclarator a)
  | AttrExternal a SrcSpan
  | AttrIntent a SrcSpan Intent
  | AttrIntrinsic a SrcSpan
  | AttrOptional a SrcSpan
  | AttrParameter a SrcSpan
  | AttrPointer a SrcSpan
  | AttrPrivate a SrcSpan
  | AttrProtected a SrcSpan
  | AttrPublic a SrcSpan
  | AttrSave a SrcSpan
  | AttrSuffix a SrcSpan (Suffix a)  -- for language-binding-spec
  | AttrTarget a SrcSpan
  | AttrValue a SrcSpan
  | AttrVolatile a SrcSpan
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Intent = In | Out | InOut
  deriving (Eq, Show, Data, Typeable, Generic)

data ControlPair a = ControlPair a SrcSpan (Maybe String) (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data AllocOpt a =
    AOStat a SrcSpan (Expression a)
  | AOErrMsg a SrcSpan (Expression a)
  | AOSource a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data ImpList a = ImpList a SrcSpan (TypeSpec a) (AList ImpElement a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data ImpElement a =
    ImpCharacter    a SrcSpan String
  | ImpRange        a SrcSpan String String
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- | Note that the 'Declarator's in common group definitions do not contain
--   initializing expressions.
data CommonGroup a =
  CommonGroup a SrcSpan (Maybe (Expression a)) (AList Declarator a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Namelist a =
  Namelist a SrcSpan (Expression a) (AList Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data DataGroup a =
  DataGroup a SrcSpan (AList Expression a) (AList Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data StructureItem a =
    StructFields a SrcSpan (TypeSpec a) (Maybe (AList Attribute a)) (AList Declarator a)
  | StructUnion a SrcSpan (AList UnionMap a)
  | StructStructure a SrcSpan (Maybe String) String (AList StructureItem a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data UnionMap a =
  UnionMap a SrcSpan (AList StructureItem a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data FormatItem a =
    FIFormatList            a             SrcSpan   (Maybe String) (AList FormatItem a)
  | FIHollerith             a             SrcSpan   (Value a)
  | FIDelimiter             a             SrcSpan
--  descriptor type       | annotation  | span    | repeat          | descriptor  | width   | integer
  | FIFieldDescriptorDEFG   a             SrcSpan   (Maybe Integer)   Char          Integer   Integer
  | FIFieldDescriptorAIL    a             SrcSpan   (Maybe Integer)   Char          Integer
  | FIBlankDescriptor       a             SrcSpan   Integer
  | FIScaleFactor           a             SrcSpan   Integer
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data FlushSpec a =
    FSUnit a SrcSpan (Expression a)
  | FSIOStat a SrcSpan (Expression a)
  | FSIOMsg a SrcSpan (Expression a)
  | FSErr a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data DoSpecification a =
  DoSpecification a SrcSpan (Statement a) (Expression a) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Expression a =
    ExpValue         a SrcSpan (Value a)
  -- ^ Use a value as an expression.
  | ExpBinary        a SrcSpan BinaryOp (Expression a) (Expression a)
  -- ^ A binary operator applied to two expressions.
  | ExpUnary         a SrcSpan UnaryOp (Expression a)
  -- ^ A unary operator applied to one expression.
  | ExpSubscript     a SrcSpan (Expression a) (AList Index a)
  -- ^ Array indexing
  | ExpDataRef       a SrcSpan (Expression a) (Expression a)
  -- ^ @%@ notation for variables inside data types
  | ExpFunctionCall  a SrcSpan (Expression a) (Maybe (AList Argument a))
  -- ^ A function expression applied to a list of arguments.
  | ExpImpliedDo     a SrcSpan (AList Expression a) (DoSpecification a)
  -- ^ Implied do (i.e. one-liner do loops)
  | ExpInitialisation  a SrcSpan (AList Expression a)
  -- ^ Array initialisation
  | ExpReturnSpec    a SrcSpan (Expression a)
  -- ^ Function return value specification
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Index a =
    IxSingle a SrcSpan (Maybe String) (Expression a)
  | IxRange a SrcSpan
            (Maybe (Expression a)) -- ^ Lower index
            (Maybe (Expression a)) -- ^ Upper index
            (Maybe (Expression a)) -- ^ Stride
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- All recursive Values
data Value a =
    ValInteger           String
  -- ^ The string representation of an integer literal
  | ValReal              String
  -- ^ The string representation of a real literal
  | ValComplex           (Expression a) (Expression a)
  -- ^ The real and imaginary parts of a complex value
  | ValString            String
  -- ^ A string literal
  | ValHollerith         String
  -- ^ A Hollerith literal
  | ValVariable          Name
  -- ^ The name of a variable
  | ValIntrinsic         Name
  -- ^ The name of a built-in function
  | ValLogical           String
  -- ^ A boolean value
  | ValOperator          String
  -- ^ User-defined operators in interfaces
  | ValAssignment
  -- ^ Overloaded assignment in interfaces
  | ValType              String
  | ValStar
  | ValColon                   -- see R402 / C403 in Fortran2003 spec.
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- | Declarators.
--
-- Declaration statements can have multiple variables on the right of the double
-- colon, separated by commas. A 'Declarator' identifies a single one of these.
--
-- Each declared variable can have an initializing expression. These expressions
-- are defined in HP's F90 spec to be /initialization expressions/, which are
-- specialized constant expressions.
--
-- The length expressions here are defined in HP's F90 spec to be specifications
-- expressions, which are scalar integer expressions with a bunch of
-- restrictions similar to initialization expressions.
--
-- 'Declarator's are also used for some less-used syntax that let you set
-- variable attributes using statements, like:
--
--     integer arr
--     dimension arr(10)
--
-- Some of these only set part of the 'Declarator' (e.g. @parameter@ only sets
-- the initial value).
--
-- Syntax note: length is set like @character :: str*10@, dimensions are set
-- like @integer :: arr(10)@. Careful to not get confused.
--
-- Note that according to HP's F90 spec, lengths may only be specified for
-- CHARACTER types. So for any declarations that aren't 'TypeCharacter' in the
-- outer 'TypeSpec', the length expression should be Nothing. However, this is
-- not enforced by the AST or parser, so be warned.
data Declarator a =
    DeclVariable a SrcSpan
                 (Expression a)             -- ^ Variable
                 (Maybe (Expression a))     -- ^ Length (character)
                 (Maybe (Expression a))     -- ^ Initial value
  | DeclArray a SrcSpan
              (Expression a)                -- ^ Array
              (AList DimensionDeclarator a) -- ^ Dimensions
              (Maybe (Expression a))        -- ^ Length (character)
              (Maybe (Expression a))        -- ^ Initial value
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

setInitialisation :: Declarator a -> Expression a -> Declarator a
setInitialisation (DeclVariable a s v l Nothing) init =
  DeclVariable a (getTransSpan s init) v l (Just init)
setInitialisation (DeclArray a s v ds l Nothing) init =
  DeclArray a (getTransSpan s init) v ds l (Just init)
-- do nothing when there is already a value
setInitialisation d _ = d

-- | Dimension declarator stored in @dimension@ attributes and 'Declarator's.
data DimensionDeclarator a =
  DimensionDeclarator a SrcSpan (Maybe (Expression a)) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data UnaryOp =
    Plus
  | Minus
  | Not
  | UnCustom String
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary UnaryOp

data BinaryOp =
    Addition
  | Subtraction
  | Multiplication
  | Division
  | Exponentiation
  | Concatenation
  | GT
  | GTE
  | LT
  | LTE
  | EQ
  | NE
  | Or
  | XOr
  | And
  | Equivalent
  | NotEquivalent
  | BinCustom String
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary BinaryOp

-- Retrieving SrcSpan and Annotation from nodes
class Annotated f where
  getAnnotation :: f a -> a
  setAnnotation :: a -> f a -> f a
  modifyAnnotation :: (a -> a) -> f a -> f a
  default getAnnotation :: (FirstParameter (f a) a) => f a -> a
  getAnnotation = getFirstParameter

  default setAnnotation :: (FirstParameter (f a) a) => a -> f a -> f a
  setAnnotation = setFirstParameter

  modifyAnnotation f x = setAnnotation (f (getAnnotation x)) x

instance FirstParameter (ProgramUnit a) a
instance FirstParameter (Prefix a) a
instance FirstParameter (Suffix a) a
instance FirstParameter (Block a) a
instance FirstParameter (Statement a) a
instance FirstParameter (Argument a) a
instance FirstParameter (Use a) a
instance FirstParameter (TypeSpec a) a
instance FirstParameter (ProcDecl a) a
instance FirstParameter (ProcInterface a) a
instance FirstParameter (Selector a) a
instance FirstParameter (Attribute a) a
instance FirstParameter (ImpList a) a
instance FirstParameter (ImpElement a) a
instance FirstParameter (CommonGroup a) a
instance FirstParameter (DataGroup a) a
instance FirstParameter (StructureItem a) a
instance FirstParameter (UnionMap a) a
instance FirstParameter (Namelist a) a
instance FirstParameter (FormatItem a) a
instance FirstParameter (Expression a) a
instance FirstParameter (Index a) a
instance FirstParameter (DoSpecification a) a
instance FirstParameter (FlushSpec a) a
instance FirstParameter (Declarator a) a
instance FirstParameter (DimensionDeclarator a) a
instance FirstParameter (ControlPair a) a
instance FirstParameter (AllocOpt a) a

instance SecondParameter (ProgramUnit a) SrcSpan
instance SecondParameter (Prefix a) SrcSpan
instance SecondParameter (Suffix a) SrcSpan
instance SecondParameter (Block a) SrcSpan
instance SecondParameter (Statement a) SrcSpan
instance SecondParameter (Argument a) SrcSpan
instance SecondParameter (Use a) SrcSpan
instance SecondParameter (TypeSpec a) SrcSpan
instance SecondParameter (ProcDecl a) SrcSpan
instance SecondParameter (ProcInterface a) SrcSpan
instance SecondParameter (Selector a) SrcSpan
instance SecondParameter (Attribute a) SrcSpan
instance SecondParameter (ImpList a) SrcSpan
instance SecondParameter (ImpElement a) SrcSpan
instance SecondParameter (CommonGroup a) SrcSpan
instance SecondParameter (DataGroup a) SrcSpan
instance SecondParameter (StructureItem a) SrcSpan
instance SecondParameter (UnionMap a) SrcSpan
instance SecondParameter (Namelist a) SrcSpan
instance SecondParameter (FormatItem a) SrcSpan
instance SecondParameter (Expression a) SrcSpan
instance SecondParameter (Index a) SrcSpan
instance SecondParameter (DoSpecification a) SrcSpan
instance SecondParameter (FlushSpec a) SrcSpan
instance SecondParameter (Declarator a) SrcSpan
instance SecondParameter (DimensionDeclarator a) SrcSpan
instance SecondParameter (ControlPair a) SrcSpan
instance SecondParameter (AllocOpt a) SrcSpan

instance Annotated (AList t)
instance Annotated ProgramUnit
instance Annotated Block
instance Annotated Statement
instance Annotated Argument
instance Annotated Use
instance Annotated TypeSpec
instance Annotated ProcDecl
instance Annotated ProcInterface
instance Annotated Selector
instance Annotated Attribute
instance Annotated ImpList
instance Annotated ImpElement
instance Annotated CommonGroup
instance Annotated DataGroup
instance Annotated StructureItem
instance Annotated UnionMap
instance Annotated Namelist
instance Annotated FormatItem
instance Annotated Expression
instance Annotated Index
instance Annotated DoSpecification
instance Annotated FlushSpec
instance Annotated Declarator
instance Annotated DimensionDeclarator
instance Annotated ControlPair
instance Annotated AllocOpt

instance Spanned (ProgramUnit a)
instance Spanned (Prefix a)
instance Spanned (Suffix a)
instance Spanned (Statement a)
instance Spanned (Argument a)
instance Spanned (Use a)
instance Spanned (Attribute a)
instance Spanned (TypeSpec a)
instance Spanned (ProcDecl a)
instance Spanned (ProcInterface a)
instance Spanned (Selector a)
instance Spanned (ImpList a)
instance Spanned (ImpElement a)
instance Spanned (Block a)
instance Spanned (CommonGroup a)
instance Spanned (DataGroup a)
instance Spanned (StructureItem a)
instance Spanned (UnionMap a)
instance Spanned (Namelist a)
instance Spanned (FormatItem a)
instance Spanned (Expression a)
instance Spanned (Index a)
instance Spanned (DoSpecification a)
instance Spanned (FlushSpec a)
instance Spanned (Declarator a)
instance Spanned (DimensionDeclarator a)
instance Spanned (ControlPair a)
instance Spanned (AllocOpt a)

instance Spanned (ProgramFile a) where
  getSpan (ProgramFile _ pus) =
    case pus of
      [] -> SrcSpan initPosition initPosition
      pus' -> getSpan pus'

  setSpan _ _ = error "Cannot set span to a program unit"

class Labeled f where
  getLabel :: f a -> Maybe (Expression a)
  getLastLabel :: f a -> Maybe (Expression a)
  setLabel :: f a -> Expression a -> f a

instance Labeled Block where
  getLabel (BlStatement _ _ l _) = l
  getLabel (BlIf _ _ l _ _ _ _) = l
  getLabel (BlCase _ _ l _ _ _ _ _) = l
  getLabel (BlDo _ _ l _ _ _ _ _) = l
  getLabel (BlDoWhile _ _ l _ _ _ _ _) = l
  getLabel _ = Nothing

  getLastLabel b@BlStatement{} = getLabel b
  getLastLabel (BlIf _ _ _ _ _ _ l) = l
  getLastLabel (BlCase _ _ _ _ _ _ _ l) = l
  getLastLabel (BlDo _ _ _ _ _ _ _ l) = l
  getLastLabel (BlDoWhile _ _ _ _ _ _ _ l) = l
  getLastLabel _ = Nothing

  setLabel (BlStatement a s _ st) l = BlStatement a s (Just l) st
  setLabel (BlIf a s _ mn conds bs el) l = BlIf a s (Just l) mn conds bs el
  setLabel (BlDo a s _ mn tl spec bs el) l = BlDo a s (Just l) mn tl spec bs el
  setLabel (BlDoWhile a s _ n tl spec bs el) l = BlDoWhile a s (Just l) n tl spec bs el
  setLabel b _ = b

data ProgramUnitName =
    Named String
  | NamelessBlockData
  | NamelessComment
  | NamelessMain
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary ProgramUnitName
instance NFData ProgramUnitName

class Named a where
  getName :: a -> ProgramUnitName
  setName :: ProgramUnitName -> a -> a

instance Named (ProgramUnit a) where
  getName (PUMain _ _ Nothing _ _) = NamelessMain
  getName (PUMain _ _ (Just n) _ _) = Named n
  getName (PUModule _ _ n _ _) = Named n
  getName (PUSubroutine _ _ _ n _ _ _) = Named n
  getName (PUFunction _ _ _ _ n _ _ _ _) = Named n
  getName (PUBlockData _ _ Nothing _)  = NamelessBlockData
  getName (PUBlockData _ _ (Just n) _) = Named n
  getName PUComment{} = NamelessComment
  setName (Named n) (PUMain a s _ b pus) = PUMain a s (Just n) b pus
  setName _         (PUMain a s _ b pus) = PUMain a s Nothing b pus
  setName (Named n) (PUModule a s _ b pus) = PUModule a s n b pus
  setName (Named n) (PUSubroutine a s r _ p b subs) =
    PUSubroutine a s r n p b subs
  setName (Named n) (PUFunction   a s r rec _ p res b subs) =
    PUFunction a s r rec n p res b subs
  setName (Named n) (PUBlockData  a s _ b) = PUBlockData  a s (Just n) b
  setName _         (PUBlockData  a s _ b) = PUBlockData  a s Nothing b
  -- Identity function if first arg is nameless or second arg is comment.
  setName _ a = a

instance Out MetaInfo
instance Out a => Out (ProgramFile a)
instance Out a => Out (ProgramUnit a)
instance Out a => Out (Prefix a)
instance Out a => Out (Suffix a)
instance Out a => Out (Statement a)
instance Out a => Out (ProcDecl a)
instance Out a => Out (ProcInterface a)
instance Out Only
instance Out ModuleNature
instance Out a => Out (Argument a)
instance Out a => Out (Use a)
instance Out a => Out (Attribute a)
instance Out Intent
instance Out a => Out (ImpList a)
instance Out a => Out (ImpElement a)
instance Out a => Out (Comment a)
instance Out a => Out (Block a)
instance Out a => Out (CommonGroup a)
instance Out a => Out (DataGroup a)
instance Out a => Out (StructureItem a)
instance Out a => Out (UnionMap a)
instance Out a => Out (Namelist a)
instance Out a => Out (FormatItem a)
instance Out a => Out (Expression a)
instance Out a => Out (Index a)
instance Out a => Out (DoSpecification a)
instance Out a => Out (FlushSpec a)
instance Out a => Out (Value a)
instance Out a => Out (TypeSpec a)
instance Out a => Out (Selector a)
instance Out BaseType
instance Out a => Out (Declarator a)
instance Out a => Out (DimensionDeclarator a)
instance Out a => Out (ControlPair a)
instance Out a => Out (AllocOpt a)
instance Out UnaryOp
instance Out BinaryOp
instance Out a => Out (ForallHeader a)

-- Classifiers on statement and blocks ASTs

nonExecutableStatement :: FortranVersion -> Statement a -> Bool
nonExecutableStatement _ s = case s of
    StIntent {}       -> True
    StOptional {}     -> True
    StPublic {}       -> True
    StPrivate {}      -> True
    StProtected {}    -> True
    StSave {}         -> True
    StDimension {}    -> True
    StAllocatable {}  -> True
    StAsynchronous {} -> True
    StPointer {}      -> True
    StTarget {}       -> True
    StValue {}        -> True
    StVolatile {}     -> True
    StData {}         -> True
    StParameter {}    -> True
    StImplicit {}     -> True
    StNamelist {}     -> True
    StEquivalence {}  -> True
    StCommon {}       -> True
    StExternal {}     -> True
    StIntrinsic {}    -> True
    StUse {}          -> True
    StEntry {}        -> True
    StSequence {}     -> True
    StType {}         -> True
    StEndType {}      -> True
    StFormat {}       -> True
    StFormatBogus {}  -> True
    StInclude {}      -> True
    StDeclaration {}  -> True
    StStructure {}    -> True
    _                 -> False

executableStatement :: FortranVersion -> Statement a -> Bool
-- Some statements are both executable and non-executable in Fortran 90 upwards
executableStatement v StFormat{} | v >= Fortran90 = True
executableStatement v StEntry{}  | v >= Fortran90 = True
executableStatement v StData{}   | v >= Fortran90 = True
executableStatement v s = not $ nonExecutableStatement v s

executableStatementBlock :: FortranVersion -> Block a -> Bool
executableStatementBlock v (BlStatement _ _ _ s) = executableStatement v s
executableStatementBlock _ _ = False

nonExecutableStatementBlock :: FortranVersion -> Block a -> Bool
nonExecutableStatementBlock v (BlStatement _ _ _ s) = nonExecutableStatement v s
nonExecutableStatementBlock _ BlInterface{} = True
nonExecutableStatementBlock _ _ = False

instance NFData a => NFData (ProgramFile a)
instance NFData a => NFData (ProgramUnit a)
instance NFData a => NFData (Block a)
instance NFData a => NFData (Expression a)
instance NFData a => NFData (TypeSpec a)
instance NFData a => NFData (Index a)
instance NFData a => NFData (Value a)
instance NFData a => NFData (Comment a)
instance NFData a => NFData (Statement a)
instance NFData a => NFData (ProcDecl a)
instance NFData a => NFData (ProcInterface a)
instance NFData a => NFData (DoSpecification a)
instance NFData a => NFData (Selector a)
instance NFData a => NFData (ForallHeader a)
instance NFData a => NFData (Argument a)
instance NFData a => NFData (Use a)
instance NFData a => NFData (Attribute a)
instance NFData a => NFData (CommonGroup a)
instance NFData a => NFData (ControlPair a)
instance NFData a => NFData (AllocOpt a)
instance NFData a => NFData (DataGroup a)
instance NFData a => NFData (DimensionDeclarator a)
instance NFData a => NFData (Declarator a)
instance NFData a => NFData (FormatItem a)
instance NFData a => NFData (FlushSpec a)
instance NFData a => NFData (ImpElement a)
instance NFData a => NFData (ImpList a)
instance NFData a => NFData (Namelist a)
instance NFData a => NFData (Prefix a)
instance NFData a => NFData (Suffix a)
instance NFData a => NFData (StructureItem a)
instance NFData a => NFData (UnionMap a)
instance NFData MetaInfo
instance NFData BaseType
instance NFData UnaryOp
instance NFData BinaryOp
instance NFData Only
instance NFData ModuleNature
instance NFData Intent

--------------------------------------------------------------------------------


{-
type Type from to = (Str to, Str to -> from)

-- | The field to the right contains a @Maybe [a]@ of the target.
{-# INLINE[1] (|+*) #-}
(|+*) :: Type (Maybe [to] -> from) to -> Maybe [to] -> Type from to
(|+*) (xs, x_) Nothing  = (xs,\xs -> x_ xs [])
(|+*) (xs, x_) (Just y) = (||*) (xs, x_) y
-}

-- TODO: Lies I have made:
--   * General: Expressions can contain Statements: Expression has ExpImpliedDo
--     has DoSpecification has Statement. Is this OK?
--   * General: Statements can contain Blocks: Statement has StInclude has
--     [Block]. Awkward-feeling mutual recursion.
--   * Uniplate PU: PU has Block has BlInterface has [PU].
--   * Uniplate Block: Block has BlStatement has Statement has StInclude has
--     [Block]. *However*, this is only for includes, which we could possibly
--     handle in a less mutually recursive manner.
--   * Uniplate Statement: Statement has StDeclaration has TypeSpec has Selector
--     has Expression has ExpImpliedDo has DoSpecification has Statement. It's
--     bullshit but it's there.
--   * Uniplate Expression: Expression has ExpValue has Value has ValComplex has
--     Expression. But these Expressions are certainly limited in some way.
--
-- Other:
--   * More combinators: for nested lists, ALists
--     * (or maybe if I'm confused, I solve it through a new biplate instance?)

instance {-# OVERLAPS #-} Uniplate (ProgramFile a) where
  uniplate = plate

instance {-# OVERLAPS #-} Biplate (ProgramFile a) (ProgramFile a) where
  biplate = plateSelf

instance {-# OVERLAPS #-} Data a => Uniplate (ProgramUnit a) where
  uniplate (PUMain a ss mName blk     mPUs) =
     plate (PUMain a ss mName blk) |+ mPUs
  uniplate (PUModule a ss name blk     mPUs) =
     plate (PUModule a ss name blk) |+ mPUs
  uniplate (PUSubroutine a pfxSfx ss name mArgs blk     mPUs) =
     plate (PUSubroutine a pfxSfx ss name mArgs blk) |+ mPUs
  uniplate (PUFunction a ss mTSpec pfxSfx name mArgs mRes blk     mPUs) =
     plate (PUFunction a ss mTSpec pfxSfx name mArgs mRes blk) |+ mPUs
  uniplate x@PUBlockData{} = plate x
  uniplate x@PUComment{}   = plate x

instance {-# OVERLAPS #-} Data a => Biplate (ProgramUnit a) (ProgramUnit a) where
  biplate = plateSelf

instance {-# OVERLAPS #-} Data a => Uniplate (Block a) where
  uniplate x@BlStatement{} = plate x
  uniplate (BlForall a ss mLabel mName fahead      body    mEndLabel) =
     plate (BlForall a ss mLabel mName fahead) ||* body |- mEndLabel
  uniplate (BlIf a ss mLabel mName conds     bodys    mEndLabel) =
     --plate (BlIf a ss mLabel mName conds) |||* bodys |- mEndLabel
     plate (BlIf a ss mLabel mName conds) |+ bodys |- mEndLabel
  uniplate (BlCase a ss mLabel mName scrutinee ranges     bodys    mEndLabel) =
     --plate (BlCase a ss mLabel mName scrutinee ranges) |||* bodys |- mEndLabel
     plate (BlCase a ss mLabel mName scrutinee ranges) |+ bodys |- mEndLabel
  -- TODO particularly nasty: DoSpecification has a Stmt, which could be StInclude...
  uniplate (BlDo a ss mLabel mName mTLabel dospec      body    mEndLabel) =
     plate (BlDo a ss mLabel mName mTLabel dospec) ||* body |- mEndLabel
  uniplate (BlDoWhile a ss mLabel mName mTLabel cond      body    mEndLabel) =
     plate (BlDoWhile a ss mLabel mName mTLabel cond) ||* body |- mEndLabel
  -- TODO lying, we have a [PU] here urgh
  uniplate (BlInterface a ss mLabel x pus     body) =
     plate (BlInterface a ss mLabel x pus) ||* body
  uniplate x@BlComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (Block a) (Block a) where
  biplate = trace "biplate block block" plateSelf

instance {-# OVERLAPS #-} Data a => Uniplate (Statement a) where
  -- TODO ignoring StInclude which has [Block]
  uniplate (StIfLogical a ss expr     stmt) =
     plate (StIfLogical a ss expr) |* stmt
  uniplate (StForallStatement a ss forallhead    stmt) =
     plate (StForallStatement a ss forallhead) |* stmt
  uniplate (StWhere a ss expr     stmt) =
     plate (StWhere a ss expr) |* stmt
  uniplate x = plate x

instance {-# OVERLAPS #-} Data a => Biplate (Statement a) (Statement a) where
  biplate = trace "biplate statement statement" plateSelf

instance {-# OVERLAPS #-} Data a => Uniplate (Expression a) where
  uniplate x@ExpValue{} = plate x
  uniplate (ExpBinary a ss binop     e1    e2) =
     plate (ExpBinary a ss binop) |* e1 |* e2
  uniplate (ExpUnary a ss binop     e1) =
     plate (ExpUnary a ss binop) |* e1
  -- TODO Index does use Expressions and it's important (pretending it doesn't
  --      makes function call disambig fail)
  uniplate (ExpSubscript a ss     e    idxs) =
     plate (ExpSubscript a ss) |* e |+ idxs
  uniplate (ExpDataRef a ss     e1    e2) =
     plate (ExpDataRef a ss) |* e1 |* e2
  -- TODO
  uniplate (ExpFunctionCall a ss     e    args) =
     plate (ExpFunctionCall a ss) |* e |+ args
  -- TODO how to treat AList like [Expression a]?
  uniplate (ExpImpliedDo a ss     es    dospec) =
     plate (ExpImpliedDo a ss) |+ es |+ dospec
  uniplate (ExpInitialisation a ss     es) =
     plate (ExpInitialisation a ss) |+ es
  uniplate (ExpReturnSpec a ss     e) =
     plate (ExpReturnSpec a ss) |* e

instance {-# OVERLAPS #-} Data a => Biplate (Expression a) (Expression a) where
  biplate = trace "biplate expr expr" plateSelf

instance {-# OVERLAPS #-} Data a => Biplate (ProgramFile a) (ProgramUnit a) where
  biplate (ProgramFile mi pus) = trace "biplate pf pu" $
    plate (ProgramFile mi) ||* pus

instance {-# OVERLAPS #-} Data a => Biplate (ProgramFile a) (Block a) where
  biplate (ProgramFile mi pus) = trace "biplate pf pu" $
    plate (ProgramFile mi) ||+ pus

instance {-# OVERLAPS #-} Data a => Biplate (ProgramFile a) (Expression a) where
  biplate (ProgramFile mi pus) = trace "biplate pf expr" $
    plate (ProgramFile mi) ||+ pus

instance {-# OVERLAPS #-} Data a => Biplate (ProgramFile a) (Statement a) where
  biplate (ProgramFile mi pus) = trace "biplate pf stmt" $
    plate (ProgramFile mi) ||+ pus

instance {-# OVERLAPS #-} Data a => Biplate (ProgramUnit a) (Block a) where
  -- TODO: it's possible to get a Block from an Expr via DoSpec -> Statement ->
  -- StInclude but I think it's unintended and unparseable
  biplate (PUMain a ss mName      body    mSubprogs) =
    plate (PUMain a ss mName) ||* body |+ mSubprogs
  biplate (PUModule a ss name      body    mSubprogs) =
    plate (PUModule a ss name) ||* body |+ mSubprogs
  biplate (PUSubroutine a ss opts name mArgs      body    mSubprogs) =
    plate (PUSubroutine a ss opts name mArgs) ||* body |+ mSubprogs
  biplate (PUFunction a ss mRetTy opts name mArgs mRes      body    mSubprogs) =
    plate (PUFunction a ss mRetTy opts name mArgs mRes) ||* body |+ mSubprogs
  biplate (PUBlockData a ss mName      body) =
    plate (PUBlockData a ss mName) ||* body
  biplate x@PUComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (ProgramUnit a) (Statement a) where
  -- TODO: it's possible to get a Block from an Expr via DoSpec -> Statement ->
  -- StInclude but I think it's unintended and unparseable
  biplate (PUMain a ss mName     body    mSubprogs) =
    plate (PUMain a ss mName) |+ body |+ mSubprogs
  biplate (PUModule a ss name     body    mSubprogs) =
    plate (PUModule a ss name) |+ body |+ mSubprogs
  biplate (PUSubroutine a ss opts name mArgs     body    mSubprogs) =
    plate (PUSubroutine a ss opts name mArgs) |+ body |+ mSubprogs
  biplate (PUFunction a ss mRetTy opts name mArgs mRes     body    mSubprogs) =
    plate (PUFunction a ss mRetTy opts name mArgs mRes) |+ body |+ mSubprogs
  biplate (PUBlockData a ss mName     body) =
    plate (PUBlockData a ss mName) |+ body
  biplate x@PUComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (ProgramUnit a) (Expression a) where
  biplate (PUMain a ss mName     body    mSubprogs) =
    plate (PUMain a ss mName) |+ body |+ mSubprogs
  biplate (PUModule a ss name     body    mSubprogs) =
    plate (PUModule a ss name) |+ body |+ mSubprogs
  biplate (PUSubroutine a ss opts name     mArgs    body    mSubprogs) =
    plate (PUSubroutine a ss opts name) |+ mArgs |+ body |+ mSubprogs
  -- TODO lying: mRetTy = TypeSpec has Expression
  biplate (PUFunction a ss mRetTy opts name     mArgs    mRes    body    mSubprogs) =
    plate (PUFunction a ss mRetTy opts name) |+ mArgs |+ mRes |+ body |+ mSubprogs
  biplate (PUBlockData a ss mName     body) =
    plate (PUBlockData a ss mName) |+ body
  biplate x@PUComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (Block a) (Statement a) where
  biplate (BlStatement a ss mLabel     stmt) =
    plate (BlStatement a ss mLabel) |* stmt
  biplate (BlForall a ss mLabel mName fahead      body    mEndLabel) =
    plate (BlForall a ss mLabel mName fahead) ||+ body |- mEndLabel
  biplate (BlIf a ss mLabel mName conds      bodys    mEndLabel) =
    plate (BlIf a ss mLabel mName conds) ||+ bodys |- mEndLabel
  biplate (BlCase a ss mLabel mName scrutinee ranges      bodys    mEndLabel) =
    plate (BlCase a ss mLabel mName scrutinee ranges) ||+ bodys |- mEndLabel
  biplate (BlDo a ss mLabel mName mTLabel dospec      body    mEndLabel) =
    plate (BlDo a ss mLabel mName mTLabel dospec) ||+ body |- mEndLabel
  biplate (BlDoWhile a ss mLabel mName mTLabel cond      body    mEndLabel) =
    plate (BlDoWhile a ss mLabel mName mTLabel cond) ||+ body |- mEndLabel
  biplate (BlInterface a ss mLabel x pus     body) =
    plate (BlInterface a ss mLabel x pus) ||+ body
  biplate x@BlComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (Block a) (Expression a) where
  biplate (BlStatement a ss     mLabel    stmt) =
    plate (BlStatement a ss) |+ mLabel |+ stmt
  biplate (BlForall a ss     mLabel    mName    fahead     body    mEndLabel) =
    plate (BlForall a ss) |+ mLabel |- mName |+ fahead ||+ body |+ mEndLabel
  biplate (BlIf a ss     mLabel    mName     conds     bodys    mEndLabel) =
    plate (BlIf a ss) |+ mLabel |- mName ||+ conds ||+ bodys |+ mEndLabel
  biplate (BlCase a ss     mLabel    mName    scrutinee    ranges     bodys    mEndLabel) =
    plate (BlCase a ss) |+ mLabel |- mName |* scrutinee |+ ranges ||+ bodys |+ mEndLabel
  biplate (BlDo a ss     mLabel    mName    mTLabel    dospec     body    mEndLabel) =
    plate (BlDo a ss) |+ mLabel |- mName |+ mTLabel |+ dospec ||+ body |+ mEndLabel
  biplate (BlDoWhile a ss     mLabel    mName    mTLabel    cond     body    mEndLabel) =
    plate (BlDoWhile a ss) |+ mLabel |- mName |+ mTLabel |* cond ||+ body |+ mEndLabel
  biplate (BlInterface a ss     mLabel    x     pus     body) =
    plate (BlInterface a ss) |+ mLabel |- x ||+ pus ||+ body
  biplate x@BlComment{} = plate x

instance {-# OVERLAPS #-} Data a => Biplate (Statement a) (Expression a) where
  biplate (StDeclaration a ss     ts    mAttrs    decls) =
    plate (StDeclaration a ss) |+ ts |- mAttrs |+ decls
  biplate (StStructure a ss mStr     items) =
    plate (StStructure a ss mStr) |+ items
  biplate (StIntent a ss intent     exprs) =
    plate (StIntent a ss intent) |+ exprs
  biplate (StOptional a ss     exprs) =
    plate (StOptional a ss) |+ exprs
  biplate (StPublic a ss     exprs) =
    plate (StPublic a ss) |+ exprs
  biplate (StPrivate a ss     exprs) =
    plate (StPrivate a ss) |+ exprs
  biplate (StProtected a ss     exprs) =
    plate (StProtected a ss) |+ exprs
  biplate (StSave a ss     exprs) =
    plate (StSave a ss) |+ exprs
  biplate (StDimension a ss     decls) =
    plate (StDimension a ss) |+ decls
  biplate (StAllocatable a ss     decls) =
    plate (StAllocatable a ss) |+ decls
  biplate (StAsynchronous a ss     decls) =
    plate (StAsynchronous a ss) |+ decls
  biplate (StPointer a ss     decls) =
    plate (StPointer a ss) |+ decls
  biplate (StTarget a ss     decls) =
    plate (StTarget a ss) |+ decls
  biplate (StValue a ss     decls) =
    plate (StValue a ss) |+ decls
  biplate (StVolatile a ss     decls) =
    plate (StVolatile a ss) |+ decls
  biplate (StData a ss     dgs) =
    plate (StData a ss) |+ dgs
  biplate (StAutomatic a ss     decls) =
    plate (StAutomatic a ss) |+ decls
  biplate (StStatic a ss     decls) =
    plate (StStatic a ss) |+ decls
  biplate (StNamelist a ss     namelists) =
    plate (StNamelist a ss) |+ namelists
  biplate (StParameter a ss     decls) =
    plate (StParameter a ss) |+ decls
  biplate (StExternal a ss     exprs) =
    plate (StExternal a ss) |+ exprs
  biplate (StIntrinsic a ss     exprs) =
    plate (StIntrinsic a ss) |+ exprs
  biplate (StCommon a ss     cgs) =
    plate (StCommon a ss) |+ cgs
  biplate (StEquivalence a ss     exprss) =
    plate (StEquivalence a ss) |+ exprss
  biplate x@StFormat{} = plate x
  biplate (StImplicit a ss     mImps) =
    plate (StImplicit a ss) |+ mImps
  biplate (StEntry a ss     expr    mExprs    mExpr) =
    plate (StEntry a ss) |* expr |+ mExprs |+ mExpr
  -- TODO: ignoring 'Maybe (Block a)' in 'StInclude'
  biplate (StInclude a ss     expr    mBlocks) =
    plate (StInclude a ss) |* expr |- mBlocks
  biplate (StDo a ss str     mExpr    mDospec) =
    plate (StDo a ss str) |+ mExpr |+ mDospec
  biplate (StDoWhile a ss str     mExpr    expr) =
    plate (StDoWhile a ss str) |+ mExpr |* expr
  biplate x@StEnddo{} = plate x
  biplate (StCycle a ss     mExpr) =
    plate (StCycle a ss) |+ mExpr
  biplate (StExit a ss     mExpr) =
    plate (StExit a ss) |+ mExpr
  biplate (StIfLogical a ss     expr    stmt) =
    plate (StIfLogical a ss) |* expr |+ stmt
  biplate (StIfArithmetic a ss     e1    e2    e3    e4) =
    plate (StIfArithmetic a ss) |* e1 |* e2 |* e3 |* e4
  biplate (StSelectCase a ss mStr     expr) =
    plate (StSelectCase a ss mStr) |* expr
  biplate (StCase a ss mStr     mIndices) =
    plate (StCase a ss mStr) |+ mIndices
  biplate x@StEndcase{} = plate x
  biplate (StFunction a ss     e1    es    e2) =
    plate (StFunction a ss) |* e1 |+ es |* e2
  biplate (StExpressionAssign a ss     e1    e2) =
    plate (StExpressionAssign a ss) |* e1 |* e2
  biplate (StPointerAssign a ss     e1    e2) =
    plate (StPointerAssign a ss) |* e1 |* e2
  biplate (StLabelAssign a ss     e1    e2) =
    plate (StLabelAssign a ss) |* e1 |* e2
  biplate (StGotoUnconditional a ss     expr) =
    plate (StGotoUnconditional a ss) |* expr
  biplate (StGotoAssigned a ss     expr    mExprs) =
    plate (StGotoAssigned a ss) |* expr |+ mExprs
  biplate (StGotoComputed a ss     es    e) =
    plate (StGotoComputed a ss) |+ es |* e
  biplate (StCall a ss     expr    mArgs) =
    plate (StCall a ss) |* expr |+ mArgs
  biplate (StReturn a ss     mExpr) =
    plate (StReturn a ss) |+ mExpr
  biplate x@StContinue{} = plate x
  biplate (StStop a ss     mExpr) =
    plate (StStop a ss) |+ mExpr
  biplate (StPause a ss     mExpr) =
    plate (StPause a ss) |+ mExpr
  biplate (StRead a ss     cps    mExprs) =
    plate (StRead a ss) |+ cps |+ mExprs
  biplate (StRead2 a ss     expr    mExprs) =
    plate (StRead2 a ss) |* expr |+ mExprs
  biplate (StWrite a ss     cps    mExprs) =
    plate (StWrite a ss) |+ cps |+ mExprs
  biplate (StPrint a ss     expr    mExprs) =
    plate (StPrint a ss) |* expr |+ mExprs
  biplate (StTypePrint a ss     expr    mExprs) =
    plate (StTypePrint a ss) |* expr |+ mExprs
  biplate (StOpen a ss     cps) =
    plate (StOpen a ss) |+ cps
  biplate (StClose a ss     cps) =
    plate (StClose a ss) |+ cps
  biplate (StFlush a ss     fss) =
    plate (StFlush a ss) |+ fss
  biplate (StInquire a ss     cps) =
    plate (StInquire a ss) |+ cps
  biplate (StRewind a ss     cps) =
    plate (StRewind a ss) |+ cps
  biplate (StRewind2 a ss     expr) =
    plate (StRewind2 a ss) |* expr
  biplate (StBackspace a ss     cps) =
    plate (StBackspace a ss) |+ cps
  biplate (StBackspace2 a ss     expr) =
    plate (StBackspace2 a ss) |* expr
  biplate (StEndfile a ss     cps) =
    plate (StEndfile a ss) |+ cps
  biplate (StEndfile2 a ss     expr) =
    plate (StEndfile2 a ss) |* expr
  biplate (StAllocate a ss     mTs    exprs    mAos) =
    plate (StAllocate a ss) |+ mTs |+ exprs |+ mAos
  biplate (StNullify a ss     exprs) =
    plate (StNullify a ss) |+ exprs
  biplate (StDeallocate a ss     exprs    mAos) =
    plate (StDeallocate a ss) |+ exprs |+ mAos
  biplate (StWhere a ss     expr    stmt) =
    plate (StWhere a ss) |* expr |+ stmt
  biplate (StWhereConstruct a ss mStr     expr) =
    plate (StWhereConstruct a ss mStr) |* expr
  biplate (StElsewhere a ss mStr     mExpr) =
    plate (StElsewhere a ss mStr) |+ mExpr
  biplate x@StEndWhere{} = plate x
  biplate (StUse a ss     expr    mMn    only    mUses) =
    plate (StUse a ss) |* expr |- mMn |- only |+ mUses
  biplate (StModuleProcedure a ss     exprs) =
    plate (StModuleProcedure a ss) |+ exprs
  biplate (StProcedure a ss     mPI    mAttr    pds) =
    plate (StProcedure a ss) |+ mPI |- mAttr |+ pds
  biplate x@StType{} = plate x
  biplate x@StEndType{} = plate x
  biplate x@StSequence{} = plate x
  biplate (StForall a ss mStr     forallHeader) =
    plate (StForall a ss mStr) |+ forallHeader
  biplate (StForallStatement a ss     forallHeader    stmt) =
    plate (StForallStatement a ss) |+ forallHeader |+ stmt
  biplate x@StEndForall{} = plate x
  biplate (StImport a ss     exprs) =
    plate (StImport a ss) |+ exprs
  biplate x@StEnum{} = plate x
  biplate (StEnumerator a ss     decls) =
    plate (StEnumerator a ss) |+ decls
  biplate x@StEndEnum{} = plate x
  biplate x@StFormatBogus{} = plate x
