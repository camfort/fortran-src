{-# OPTIONS_GHC -fno-warn-orphans #-} -- for Out (NonEmpty a)

{- | Data types for representing Fortran code (for various versions of Fortran).

The same representation is used for all supported Fortran standards. Constructs
only available in certain versions are gated by the parsers (and the pretty
printer). In general, the definitions here are highly permissible, partly to
allow for all the oddities of older standards & extensions.

Useful Fortran standard references:

  * Fortran 2018 standard: WD 1539-1 J3/18-007r1
  * Fortran 2008 standard: WD 1539-1 J3/10-007r1
  * Fortran 90 standard: ANSI X3.198-1992 (also ISO/IEC 1539:1991)
  * Fortran 90 Handbook (J. Adams)
  * Fortran 77 standard: ANSI X3.9-1978
-}

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
  , KindParam(..)
  , ComplexPart(..)
  , UnaryOp(..)
  , BinaryOp(..)

  -- ** Types and declarations
  , Name
  , BaseType(..)
  , TypeSpec(..)
  , Selector(..)
  , Declarator(..)
  , DeclaratorType(..)
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
  , ForallHeaderPart(..)
  , Only(..)
  , MetaInfo(..)
  , Prefixes
  , Suffixes
  , PrefixSuffix
  , ModuleNature(..)
  , Use(..)
  , Argument(..)
  , ArgumentExpression(..)
  , argExprNormalize
  , argExtractExpr
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

  -- * Re-exports
  , NonEmpty(..)

  ) where

import Prelude hiding ( init )

import Language.Fortran.AST.Common ( Name )
import Language.Fortran.AST.AList
import Language.Fortran.AST.Literal
import Language.Fortran.AST.Literal.Real
import Language.Fortran.AST.Literal.Boz ( Boz )
import Language.Fortran.AST.Literal.Complex
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter
import Language.Fortran.AST.Annotated
import Language.Fortran.Version

import Data.Data
import Data.Binary
import Control.DeepSeq
import Text.PrettyPrint.GenericPretty
import Data.List.NonEmpty ( NonEmpty(..) )

-- | The empty annotation.
type A0 = ()

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
  deriving stock (Ord, Eq, Show, Data, Generic)
  deriving anyclass (Binary)

-- | The type specification of a declaration statement, containing the syntactic
--   type name and kind selector.
--
-- See HP's F90 spec pg.24.
data TypeSpec a = TypeSpec
  { typeSpecAnno :: a
  , typeSpecSpan :: SrcSpan
  , typeSpecBaseType :: BaseType
  , typeSpecSelector :: Maybe (Selector a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

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
data Selector a = Selector
  { selectorAnno :: a
  , selectorSpan :: SrcSpan
  , selectorLength :: Maybe (Expression a)
  , selectorKind   :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

data MetaInfo = MetaInfo { miVersion :: FortranVersion, miFilename :: String }
  deriving stock (Eq, Show, Data, Generic)

-- Program structure definition
data ProgramFile a = ProgramFile
  { programFileMeta :: MetaInfo
  , programFileProgramUnits :: [ ProgramUnit a ]
  } deriving stock (Eq, Show, Data, Generic, Functor)

pfSetFilename :: String -> ProgramFile a -> ProgramFile a
pfSetFilename fn (ProgramFile mi pus) = ProgramFile (mi { miFilename = fn }) pus
pfGetFilename :: ProgramFile a -> String
pfGetFilename (ProgramFile mi _) = miFilename mi

-- | A Fortran program unit. _(F2008 2.2)_
--
-- A Fortran program is made up of many program units.
--
-- Related points from the Fortran 2008 specification:
--
--   * There must be exactly one main program, and any number of other program
--     units.
--   * Note 2.3: There may be at most 1 unnamed block data program unit.
data ProgramUnit a =
    PUMain                          -- ^ Main program
      a SrcSpan
      (Maybe Name)                  -- ^ Program name
      [Block a]                     -- ^ Body
      (Maybe [ProgramUnit a])       -- ^ Subprograms

  | PUModule                        -- ^ Module
      a SrcSpan
      Name                          -- ^ Program name
      [Block a]                     -- ^ Body
      (Maybe [ProgramUnit a])       -- ^ Subprograms

  | PUSubroutine                    -- ^ Subroutine subprogram (procedure)
      a SrcSpan
      (PrefixSuffix a)              -- ^ Options (elemental, pure etc.)
      Name                          -- ^ Name
      (Maybe (AList Expression a))  -- ^ Arguments
      [Block a]                     -- ^ Body
      (Maybe [ProgramUnit a])       -- ^ Subprograms

  | PUFunction                      -- ^ Function subprogram (procedure)
      a SrcSpan
      (Maybe (TypeSpec a))          -- ^ Return type
      (PrefixSuffix a)              -- ^ Options (elemental, pure etc.)
      Name                          -- ^ Name
      (Maybe (AList Expression a))  -- ^ Arguments
      (Maybe (Expression a))        -- ^ Result
      [Block a]                     -- ^ Body
      (Maybe [ProgramUnit a])       -- ^ Subprograms

  | PUBlockData                     -- ^ Block data (named or unnamed).
      a SrcSpan
      (Maybe Name)                  -- ^ Optional block
      [Block a] -- Body

  | PUComment                       -- ^ Program unit-level comment
      a SrcSpan
      (Comment a)
  deriving stock (Eq, Show, Data, Generic, Functor)

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
  deriving stock (Eq, Show, Data, Generic, Functor)

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
  deriving stock (Eq, Show, Data, Generic, Functor)

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
  deriving stock (Eq, Show, Data, Generic, Functor)

data Block a =
    BlStatement                              -- ^ Statement
                a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Statement a)                -- ^ Wrapped statement

  | BlForall                                 -- ^ FORALL array assignment syntax
                a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (ForallHeader a)             -- ^ Header information
                [ Block a ]                  -- ^ Body
                (Maybe (Expression a))       -- ^ Label to END DO

  | BlIf                                     -- ^ IF block construct
                a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (NonEmpty (Expression a, [Block a])) -- ^ IF, ELSE IF clauses
                (Maybe [Block a])            -- ^ ELSE block
                (Maybe (Expression a))       -- ^ Label to END IF

  | BlCase                                   -- ^ SELECT CASE construct
                a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (Expression a)               -- ^ Scrutinee
                [(AList Index a, [Block a])] -- ^ CASE clauses
                (Maybe [Block a])            -- ^ CASE default
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

  | BlAssociate
  -- ^ The first 'Expression' in the abbreviation tuple is always an
  --   @ExpValue _ _ (ValVariable id)@. Also guaranteed nonempty. TODO
                a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                (Maybe String)               -- ^ Construct name
                (AList (ATuple Expression Expression) a) -- ^ Expression abbreviations
                [ Block a ]                  -- ^ Body
                (Maybe (Expression a))       -- ^ Label

  | BlInterface a SrcSpan
                (Maybe (Expression a))       -- ^ Label
                Bool                         -- ^ Is this an abstract interface?
                [ ProgramUnit a ]            -- ^ Interface procedures
                [ Block a ]                  -- ^ Module procedures

  | BlComment                                -- ^ Block-level comment
                a SrcSpan
                (Comment a)
  deriving stock (Eq, Show, Data, Generic, Functor)

data Statement a  =
    StDeclaration
    -- ^ Declare variable(s) at a given type.
        a SrcSpan
        (TypeSpec a)                -- ^ Type specification
        (Maybe (AList Attribute a)) -- ^ Attributes
        (AList Declarator a)        -- ^ Declarators

  | StStructure
    -- ^ A structure (pre-F90 extension) declaration.
        a SrcSpan
        (Maybe String)          -- ^ Structure name
        (AList StructureItem a) -- ^ Structure fields

  | StIntent              a SrcSpan Intent (AList Expression a)
  | StOptional            a SrcSpan (AList Expression a)
  | StPublic              a SrcSpan (Maybe (AList Expression a))
  | StPrivate             a SrcSpan (Maybe (AList Expression a))
  | StProtected           a SrcSpan (Maybe (AList Expression a))

  | StSave
    -- ^ SAVE statement: variable retains its value between invocations
        a SrcSpan
        (Maybe (AList Expression a))
        -- ^ Save the given variables, or all saveable items in the program unit
        --   if 'Nothing'

  | StDimension           a SrcSpan (AList Declarator a)
    -- ^ DIMENSION attribute as statement.

  | StAllocatable         a SrcSpan (AList Declarator a)
    -- ^ ALLOCATABLE attribute statement.

  | StAsynchronous        a SrcSpan (AList Declarator a)
    -- ^ ASYNCHRONOUS attribute statement.

  | StPointer             a SrcSpan (AList Declarator a)
    -- ^ POINTER attribute statement.

  | StTarget              a SrcSpan (AList Declarator a)
    -- ^ TARGET attribute statement.

  | StValue               a SrcSpan (AList Declarator a)
    -- ^ VALUE attribute statement.

  | StVolatile            a SrcSpan (AList Declarator a)
    -- ^ VOLATILE attribute statement.

  | StData                a SrcSpan (AList DataGroup a)
  | StAutomatic           a SrcSpan (AList Declarator a)
  | StStatic              a SrcSpan (AList Declarator a)
  | StNamelist            a SrcSpan (AList Namelist a)

  | StParameter           a SrcSpan (AList Declarator a)
    -- ^ PARAMETER attribute as statement.

  | StExternal            a SrcSpan (AList Expression a)
  | StIntrinsic           a SrcSpan (AList Expression a)
  | StCommon              a SrcSpan (AList CommonGroup a)
  -- ^ A COMMON statement, defining a list of common blocks.
  | StEquivalence         a SrcSpan (AList (AList Expression) a)
  | StFormat              a SrcSpan (AList FormatItem a)
  | StImplicit            a SrcSpan (Maybe (AList ImpList a))
  | StEntry
        a SrcSpan
        (Expression a)               -- ^ name (guaranteed @ExpValue ValVariable@)
        (Maybe (AList Expression a)) -- ^ argument variables
        (Maybe (Expression a))       -- ^ optional result variable (guaranteed @ExpValue ValVariable@)

  | StInclude
        a SrcSpan
        (Expression a) -- ^ file name to include. guaranteed @ExpValue ValString@
        (Maybe [Block a]) -- ^ First parsed to 'Nothing', then potentially "expanded out" in a post-parse step.

  | StDo                  a SrcSpan (Maybe String) (Maybe (Expression a)) (Maybe (DoSpecification a))
  | StDoWhile             a SrcSpan (Maybe String) (Maybe (Expression a)) (Expression a)
  | StEnddo               a SrcSpan (Maybe String)
  | StCycle               a SrcSpan (Maybe (Expression a)) -- ^ guaranteed @ExpValue ValVariable@
  | StExit                a SrcSpan (Maybe (Expression a))
  | StIfLogical
        a SrcSpan
        (Expression a) -- ^ condition
        (Statement a)  -- ^ statement (should not further recurse)
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)

  | StSelectCase -- ^ CASE construct opener.
        a SrcSpan
        (Maybe String) -- ^ block name
        (Expression a)
        -- ^ CASE expression. Should be one of scalar CHARACTER, INTEGER or LOGICAL.

  | StCase -- ^ inner CASE clause
        a SrcSpan
        (Maybe String) -- ^ block name (must match a corresponding opener)
        (Maybe (AList Index a))
        -- ^ CASE indices (expressions). 'Nothing' means it's CASE DEFAULT.

  | StEndcase -- ^ END SELECT statement
        a SrcSpan
        (Maybe String) -- ^ block name (must match corresponding opener name)

  | StFunction            a SrcSpan (Expression a) (AList Expression a) (Expression a)
  | StExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | StPointerAssign       a SrcSpan (Expression a) (Expression a)
  | StLabelAssign         a SrcSpan (Expression a) (Expression a)
  | StGotoUnconditional   a SrcSpan (Expression a)
  | StGotoAssigned        a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StGotoComputed        a SrcSpan (AList Expression a) (Expression a)
  | StCall                a SrcSpan (Expression a) (AList Argument a)
  | StReturn              a SrcSpan (Maybe (Expression a))
  | StContinue            a SrcSpan
  | StStop                a SrcSpan (Maybe (Expression a))
  | StPause               a SrcSpan (Maybe (Expression a))
  | StRead                a SrcSpan (AList ControlPair a) (Maybe (AList Expression a))
  | StRead2               a SrcSpan (Expression a) (Maybe (AList Expression a))
  | StWrite               a SrcSpan (AList ControlPair a) (Maybe (AList Expression a))
  | StPrint               a SrcSpan (Expression a) (Maybe (AList Expression a))

  | StTypePrint
    -- ^ Special TYPE "print" statement (~F77 syntactic sugar for PRINT/WRITE)
    --
    -- Not to be confused with the TYPE construct in later standards for
    -- defining derived data types.
        a SrcSpan
        (Expression a) -- ^ format identifier
        (Maybe (AList Expression a)) -- ^ variables etc. to print

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

  | StAllocate -- ^ ALLOCATE: associate pointers with targets
        a SrcSpan
        (Maybe (TypeSpec a))
        (AList Expression a) -- ^ pointers (variables/references)
        (Maybe (AList AllocOpt a))
  | StNullify -- ^ NULLIFY: disassociate pointers from targets
        a SrcSpan
        (AList Expression a) -- ^ pointers (variables/references)
  | StDeallocate -- ^ DEALLOCATE: disassociate pointers from targets
        a SrcSpan
        (AList Expression a) -- ^ pointers (variables/references)
        (Maybe (AList AllocOpt a))

  | StWhere
        a SrcSpan
        (Expression a) -- ^ must be LOGICAL
        (Statement a) -- ^ guaranteed to be 'StExpressionAssign'

  | StWhereConstruct -- ^ begin WHERE block
        a SrcSpan
        (Maybe String) -- ^ block name
        (Expression a) -- ^ must be LOGICAL
  | StElsewhere -- ^ WHERE clause. compare to IF, IF ELSE
        a SrcSpan
        (Maybe String) -- ^ block name
        (Maybe (Expression a)) -- ^ must be LOGICAL
  | StEndWhere -- ^ end WHERE block
        a SrcSpan
        (Maybe String) -- ^ block name

  | StUse
    -- ^ Import definitions (procedures, types) from a module. /(F2018 14.2.2)/
    --
    -- If a module nature isn't provided and there are both intrinsic and
    -- nonintrinsic modules with that name, the nonintrinsic module is selected.
        a SrcSpan
        (Expression a)
        -- ^ name of module to use, guaranteed to be @ExpValue ValVariable@
        (Maybe ModuleNature)  -- ^ optional explicit module nature
        Only
        (Maybe (AList Use a)) -- ^ definitions to import (including renames)

  | StModuleProcedure
        a SrcSpan
        (AList Expression a)
        -- ^ procedure names, guaranteed @ExpValue ValVariable@
  | StProcedure           a SrcSpan (Maybe (ProcInterface a)) (Maybe (AList Attribute a)) (AList ProcDecl a)

  | StType -- ^ TYPE ... = begin a DDT (derived data type) definition block
        a SrcSpan
        (Maybe (AList Attribute a)) -- ^ attributes (subset permitted)
        String                      -- ^ DDT name

  | StEndType
    -- ^ END TYPE [ type-name ] = end a DDT definition block
        a SrcSpan
        (Maybe String) -- ^ optional type name (must match corresponding opener)

  | StSequence            a SrcSpan

  | StForall -- ^ FORALL ... = begin a FORALL block
        a SrcSpan
        (Maybe String)   -- ^ block name
        (ForallHeader a) -- ^ FORALL header syntax
  | StEndForall
    -- ^ END FORALL [ construct-name ]
        a SrcSpan
        (Maybe String) -- ^ block name

  | StForallStatement -- ^ FORALL statement - essentially an inline FORALL block
        a SrcSpan
        (ForallHeader a) -- ^ FORALL header syntax
        (Statement a)    -- ^ guaranteed 'StExpressionAssign' or 'StPointerAssign'

  | StImport
        a SrcSpan
        (AList Expression a) -- ^ guaranteed @ExpValue ValVariable@

  | StEnum                a SrcSpan
  | StEnumerator          a SrcSpan (AList Declarator a)
  | StEndEnum             a SrcSpan
  -- Following is a temporary solution to a complicated FORMAT statement
  -- parsing problem.
  | StFormatBogus         a SrcSpan String
  deriving stock (Eq, Show, Data, Generic, Functor)

-- R1214 proc-decl is procedure-entity-name [=> null-init]
data ProcDecl a = ProcDecl
  { procDeclAnno       :: a
  , procDeclSpan       :: SrcSpan
  , procDeclEntityName :: Expression a
  , procDeclInitName   :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

-- R1212 proc-interface is interface-name or declaration-type-spec
data ProcInterface a = ProcInterfaceName a SrcSpan (Expression a)
                     | ProcInterfaceType a SrcSpan (TypeSpec a)
  deriving stock (Eq, Show, Data, Generic, Functor)

-- | Part of a FORALL statement. Introduced in Fortran 95.
data ForallHeader a = ForallHeader
  { forallHeaderAnno    :: a
  , forallHeaderSpan    :: SrcSpan
  , forallHeaderHeaders :: [ForallHeaderPart a]
  , forallHeaderScaling :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

data ForallHeaderPart a = ForallHeaderPart
  { forallHeaderPartAnno   :: a
  , forallHeaderPartSpan   :: SrcSpan
  , forallHeaderPartName   :: Name
  , forallHeaderPartStart  :: Expression a
  , forallHeaderPartEnd    :: Expression a
  , forallHeaderPartStride :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

data Only = Exclusive | Permissive
  deriving stock (Eq, Show, Data, Generic)

data ModuleNature = ModIntrinsic | ModNonIntrinsic
  deriving stock (Eq, Show, Data, Generic)

-- | Part of USE statement. /(F2018 14.2.2)/
--
-- Expressions may be names or operators.
data Use a =
    UseRename
        a SrcSpan
        (Expression a) -- ^ local name
        (Expression a) -- ^ use name
  | UseID
        a SrcSpan
        (Expression a) -- ^ name
  deriving stock (Eq, Show, Data, Generic, Functor)

-- TODO potentially should throw Maybe String into ArgumentExpression too?
data Argument a = Argument
  { argumentAnno :: a
  , argumentSpan :: SrcSpan
  , argumentName :: Maybe String
  , argumentExpr :: ArgumentExpression a
  } deriving stock (Eq, Show, Data, Generic, Functor)

data ArgumentExpression a
  = ArgExpr              (Expression a)
  | ArgExprVar a SrcSpan Name
  deriving stock (Eq, Show, Data, Generic, Functor)

argExprNormalize :: ArgumentExpression a -> Expression a
argExprNormalize = \case ArgExpr         e -> e
                         ArgExprVar a ss v -> ExpValue a ss (ValVariable v)

argExtractExpr :: Argument a -> Expression a
argExtractExpr (Argument _ _ _ ae) = argExprNormalize ae

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
  deriving stock (Eq, Show, Data, Generic, Functor)

data Intent = In | Out | InOut
  deriving stock (Eq, Show, Data, Generic)

data ControlPair a = ControlPair
  { controlPairAnno :: a
  , controlPairSpan :: SrcSpan
  , controlPairName :: Maybe String
  , controlPairExpr :: Expression a
  } deriving stock (Eq, Show, Data, Generic, Functor)

-- | Part of ALLOCATE statement.
--
-- There are restrictions on how ALLOCATE options can be combined. See F2018
-- 9.7.1, or:
-- https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/a-to-b/allocate-statement.html
data AllocOpt a =
    AOStat   -- ^ (output) status of allocation
        a SrcSpan
        (Expression a) -- ^ scalar integer variable
  | AOErrMsg -- ^ (output) error condition if present
        a SrcSpan
        (Expression a) -- ^ scalar character variable
  | AOSource a SrcSpan (Expression a)
  deriving stock (Eq, Show, Data, Generic, Functor)

-- | List of names for an IMPLICIT statement.
data ImpList a = ImpList
  { impListAnno :: a
  , impListSpan :: SrcSpan
  , impListType :: TypeSpec a
  , impListElements :: AList ImpElement a
  } deriving stock (Eq, Show, Data, Generic, Functor)

data ImpElement a = ImpElement
  { impElementAnno :: a
  , impElementSpan :: SrcSpan
  , impElementFrom :: Char
  , impElementTo   :: Maybe Char
  } deriving stock (Eq, Show, Data, Generic, Functor)

-- | A single COMMON block definition.
--
-- The 'Declarator's here shall not contain initializing expressions.
data CommonGroup a = CommonGroup
  { commonGroupAnno :: a
  , commonGroupSpan :: SrcSpan
  , commonGroupName :: Maybe (Expression a)
  , commonGroupVars :: AList Declarator a
  } deriving stock (Eq, Show, Data, Generic, Functor)

data Namelist a = Namelist
  { namelistAnno :: a
  , namelistSpan :: SrcSpan
  , namelistName :: Expression a
  , namelistVars :: AList Expression a
  } deriving stock (Eq, Show, Data, Generic, Functor)

-- | The part of a DATA statement describing a single set of initializations.
--
-- The initializer list must be compatible with the name list. Generally, that
-- means either the lengths must be equal, or the name list is the singleton
-- list referring to an array, and the initializer list is compatible with that
-- array's shape.
data DataGroup a = DataGroup
  { dataGroupAnno         :: a
  , dataGroupSpan         :: SrcSpan
  , dataGroupNames        :: AList Expression a
  , dataGroupInitializers :: AList Expression a
  } deriving stock (Eq, Show, Data, Generic, Functor)

-- | Field types in pre-Fortran 90 non-standard structure/record/union
--   extension.
--
-- Structures were obsoleted by derived types in later standards.
--
-- The outer structure is stored in 'StStructure'.
data StructureItem a =
    StructFields    -- ^ Regular field
        a SrcSpan
        (TypeSpec a)                -- ^ Type
        (Maybe (AList Attribute a)) -- ^ Attributes
        (AList Declarator a)        -- ^ Declarators
  | StructUnion     -- ^ Union field
        a SrcSpan
        (AList UnionMap a) -- ^ Union fields
  | StructStructure -- ^ Substructure (nested/inline record/structure)
        a SrcSpan
        (Maybe String)          -- ^ Substructure name
        String                  -- ^ Field name
        (AList StructureItem a) -- ^ Substructure fields
  deriving stock (Eq, Show, Data, Generic, Functor)

data UnionMap a = UnionMap
  { unionMapAnno   :: a
  , unionMapSpan   :: SrcSpan
  , unionMapFields :: AList StructureItem a
  } deriving stock (Eq, Show, Data, Generic, Functor)

data FormatItem a =
    FIFormatList            a             SrcSpan   (Maybe String) (AList FormatItem a)
  | FIHollerith             a             SrcSpan   (Value a)
  | FIDelimiter             a             SrcSpan
--  descriptor type       | annotation  | span    | repeat          | descriptor  | width   | integer
  | FIFieldDescriptorDEFG   a             SrcSpan   (Maybe Integer)   Char          Integer   Integer
  | FIFieldDescriptorAIL    a             SrcSpan   (Maybe Integer)   Char          Integer
  | FIBlankDescriptor       a             SrcSpan   Integer
  | FIScaleFactor           a             SrcSpan   Integer
  deriving stock (Eq, Show, Data, Generic, Functor)

-- | Part of the newer (Fortran 2003?) FLUSH statement.
--
-- See: https://www.ibm.com/docs/en/xl-fortran-aix/15.1.0?topic=attributes-flush-fortran-2003
data FlushSpec a
  = FSUnit
        a SrcSpan
        (Expression a) -- ^ scalar integer expression
  | FSIOStat
        a SrcSpan
        (Expression a) -- ^ scalar integer variable
  | FSIOMsg
        a SrcSpan
        (Expression a) -- ^ scalar character variable
  | FSErr
        a SrcSpan
        (Expression a) -- ^ statement label
    deriving stock (Eq, Show, Data, Generic, Functor)

data DoSpecification a = DoSpecification
  { doSpecAnno      :: a
  , doSpecSpan      :: SrcSpan
  , doSpecInitial   :: Statement a -- ^ Guaranteed to be 'StExpressionAssign'
  , doSpecLimit     :: Expression a
  , doSpecIncrement :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

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
  | ExpFunctionCall  a SrcSpan (Expression a) (AList Argument a)
  -- ^ A function expression applied to a list of arguments.
  | ExpImpliedDo     a SrcSpan (AList Expression a) (DoSpecification a)
  -- ^ Implied do (i.e. one-liner do loops)
  | ExpInitialisation  a SrcSpan (AList Expression a)
  -- ^ Array initialisation
  | ExpReturnSpec    a SrcSpan (Expression a)
  -- ^ Function return value specification
  deriving stock (Eq, Show, Data, Generic, Functor)

data Index a =
    IxSingle a SrcSpan (Maybe String) (Expression a)
  | IxRange a SrcSpan
            (Maybe (Expression a)) -- ^ Lower index
            (Maybe (Expression a)) -- ^ Upper index
            (Maybe (Expression a)) -- ^ Stride
  deriving stock (Eq, Show, Data, Generic, Functor)

-- | Values and literals.
data Value a
  = ValInteger           String  (Maybe (KindParam a))
  -- ^ The string representation of an integer literal
  | ValReal         RealLit (Maybe (KindParam a))
  -- ^ The string representation of a real literal
  | ValComplex      (ComplexLit a)
  -- ^ The real and imaginary parts of a complex literal @(real, imag)@.
  | ValString       String
  -- ^ A string literal
  | ValBoz          Boz
  -- ^ A BOZ literal constant
  | ValHollerith    String
  -- ^ A Hollerith literal
  | ValVariable     Name
  -- ^ The name of a variable
  | ValIntrinsic    Name
  -- ^ The name of a built-in function
  | ValLogical      Bool (Maybe (KindParam a))
  -- ^ A boolean value
  | ValOperator     String
  -- ^ User-defined operators in interfaces
  | ValAssignment
  -- ^ Overloaded assignment in interfaces
  | ValType         String
  | ValStar
  | ValColon                   -- see R402 / C403 in Fortran2003 spec.
    deriving stock    (Eq, Show, Data, Generic, Functor)
    deriving anyclass (NFData, Out)

-- | Declarators. R505 entity-decl from F90 ISO spec.
--
-- Declaration statements can have multiple variables on the right of the double
-- colon, separated by commas. A 'Declarator' identifies a single one of these.
-- In F90, they look like this:
--
--     VAR_NAME ( OPT_ARRAY_DIMS ) * CHAR_LENGTH_EXPR = INIT_EXPR
--
-- F77 doesn't standardize so nicely -- in particular, I'm not confident in
-- initializing expression syntax. So no example.
--
-- Only CHARACTERs may specify a length. However, a nonstandard syntax feature
-- uses non-CHARACTER lengths as a kind parameter. We parse regardless of type
-- and warn during analysis.
data Declarator a = Declarator
  { declaratorAnno     :: a
  , declaratorSpan     :: SrcSpan
  , declaratorVariable :: Expression a
  , declaratorType     :: DeclaratorType a
  , declaratorLength   :: Maybe (Expression a)
  , declaratorInitial  :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

data DeclaratorType a
  = ScalarDecl
  | ArrayDecl (AList DimensionDeclarator a)
  deriving stock (Eq, Show, Data, Generic, Functor)

-- | Set a 'Declarator''s initializing expression only if it has none already.
setInitialisation :: Declarator a -> Expression a -> Declarator a
setInitialisation (Declarator a s v dt l Nothing) init =
  Declarator a (getTransSpan s init) v dt l (Just init)
setInitialisation d _ = d

-- | Dimension declarator stored in @dimension@ attributes and 'Declarator's.
data DimensionDeclarator a = DimensionDeclarator
  { dimDeclAnno :: a
  , dimDeclSpan :: SrcSpan
  , dimDeclLower :: Maybe (Expression a)
  , dimDeclUpper :: Maybe (Expression a)
  } deriving stock (Eq, Show, Data, Generic, Functor)

data UnaryOp =
    Plus
  | Minus
  | Not
  | UnCustom String
  deriving stock (Eq, Ord, Show, Data, Generic)

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
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Binary BinaryOp

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
instance FirstParameter (ForallHeader a) a
instance FirstParameter (ForallHeaderPart a) a

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
instance SecondParameter (ForallHeader a) SrcSpan
instance SecondParameter (ForallHeaderPart a) SrcSpan

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
instance Annotated ForallHeader
instance Annotated ForallHeaderPart

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
instance Spanned (ForallHeader a)
instance Spanned (ForallHeaderPart a)

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
  setLabel (BlIf a s _ mn clauses elseBlock el) l = BlIf a s (Just l) mn clauses elseBlock el
  setLabel (BlDo a s _ mn tl spec bs el) l = BlDo a s (Just l) mn tl spec bs el
  setLabel (BlDoWhile a s _ n tl spec bs el) l = BlDoWhile a s (Just l) n tl spec bs el
  setLabel b _ = b

data ProgramUnitName =
    Named String
  | NamelessBlockData
  | NamelessComment
  | NamelessMain
  deriving stock (Ord, Eq, Show, Data, Generic)

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
instance Out a => Out (ArgumentExpression a)
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
instance Out a => Out (TypeSpec a)
instance Out a => Out (Selector a)
instance Out BaseType
instance Out a => Out (Declarator a)
instance Out a => Out (DimensionDeclarator a)
instance Out a => Out (DeclaratorType a)
instance Out a => Out (ControlPair a)
instance Out a => Out (AllocOpt a)
instance Out UnaryOp
instance Out BinaryOp
instance Out a => Out (ForallHeader a)
instance Out a => Out (ForallHeaderPart a)

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
instance NFData a => NFData (Comment a)
instance NFData a => NFData (Statement a)
instance NFData a => NFData (ProcDecl a)
instance NFData a => NFData (ProcInterface a)
instance NFData a => NFData (DoSpecification a)
instance NFData a => NFData (Selector a)
instance NFData a => NFData (ForallHeader a)
instance NFData a => NFData (ForallHeaderPart a)
instance NFData a => NFData (Argument a)
instance NFData a => NFData (ArgumentExpression a)
instance NFData a => NFData (Use a)
instance NFData a => NFData (Attribute a)
instance NFData a => NFData (CommonGroup a)
instance NFData a => NFData (ControlPair a)
instance NFData a => NFData (AllocOpt a)
instance NFData a => NFData (DataGroup a)
instance NFData a => NFData (DimensionDeclarator a)
instance NFData a => NFData (Declarator a)
instance NFData a => NFData (DeclaratorType a)
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

instance Out a => Out (NonEmpty a)
