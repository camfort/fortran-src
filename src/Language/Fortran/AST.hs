{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
-- orphans are instances of package-natives
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Fortran.AST where

import Prelude hiding (init)
import Data.Data
import Data.Generics.Uniplate.Data ()
import Data.Typeable ()
import Data.Binary
import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Language.Fortran.ParserMonad (FortranVersion(..))

import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter


type A0 = ()

type Name = String

-- AST is polymorphic on some type a as that type is used for arbitrary
-- annotations

-- Many AST nodes such as executable statements, declerations, etc. may
-- appear in lists, hence a dedicated annotated list type is defined
data AList t a = AList a SrcSpan [t a] deriving (Eq, Show, Data, Typeable, Generic)
instance Functor t => Functor (AList t) where
  fmap f (AList a s xs) = AList (f a) s (map (fmap f) xs)


-- Convert non-empty list to AList.
fromList :: Spanned (t a) => a -> [ t a ] -> AList t a
fromList a xs = AList a (getSpan xs) xs

-- Nothing iff list is empty
fromList' :: Spanned (t a) => a -> [ t a ] -> Maybe (AList t a)
fromList' _ [] = Nothing
fromList' a xs = Just $ fromList a xs

fromReverseList :: Spanned (t ()) => [ t () ] -> AList t ()
fromReverseList = fromList () . reverse

fromReverseList' :: Spanned (t ()) => [ t () ] -> Maybe (AList t ())
fromReverseList' = fromList' () . reverse

aCons :: t a -> AList t a -> AList t a
aCons x (AList a s xs) = AList a s $ x:xs

infixr 5 `aCons`

aReverse :: AList t a -> AList t a
aReverse (AList a s xs) = AList a s $ reverse xs

aStrip :: AList t a -> [t a]
aStrip (AList _ _ l) = l

aStrip' :: Maybe (AList t a) -> [t a]
aStrip' Nothing = []
aStrip' (Just a) = aStrip a

aMap :: (t a -> r a) -> AList t a -> AList r a
aMap f (AList a s xs) = AList a s (map f xs)

-- Basic AST nodes
data BaseType =
    TypeInteger
  | TypeReal
  | TypeDoublePrecision
  | TypeComplex
  | TypeDoubleComplex
  | TypeLogical
  | TypeCharacter (Maybe CharacterLen) (Maybe String) -- ^ len and kind, if specified
  | TypeCustom String
  | ClassStar
  | ClassCustom String
  | TypeByte
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary BaseType

data CharacterLen = CharLenStar    -- ^ specified with a *
                  | CharLenColon   -- ^ specified with a : (Fortran2003)
                    -- FIXME, possibly, with a more robust const-exp:
                  | CharLenExp     -- ^ specified with a non-trivial expression
                  | CharLenInt Int -- ^ specified with a constant integer
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary CharacterLen

charLenSelector :: Maybe (Selector a) -> (Maybe CharacterLen, Maybe String)
charLenSelector Nothing                          = (Nothing, Nothing)
charLenSelector (Just (Selector _ _ mlen mkind)) = (l, k)
  where
    l | Just (ExpValue _ _ ValStar) <- mlen        = Just CharLenStar
      | Just (ExpValue _ _ ValColon) <- mlen       = Just CharLenColon
      | Just (ExpValue _ _ (ValInteger i)) <- mlen = Just $ CharLenInt (read i)
      | Nothing <- mlen                            = Nothing
      | otherwise                                  = Just CharLenExp
    k | Just (ExpValue _ _ (ValInteger i)) <- mkind  = Just i
      | Just (ExpValue _ _ (ValVariable s)) <- mkind = Just s
      -- FIXME: some references refer to things like kind=kanji but I can't find any spec for it
      | otherwise                                    = Nothing

data TypeSpec a = TypeSpec a SrcSpan BaseType (Maybe (Selector a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Selector a =
--                   Maybe length         | Maybe kind
  Selector a SrcSpan (Maybe (Expression a)) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

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
                (Maybe (Expression a))       -- Label
                (Statement a)                -- Statement

  | BlForall    a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                (ForallHeader a)             -- Header information
                [ Block a ]                  -- Body
                (Maybe (Expression a))       -- Label to END DO

  | BlIf        a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                [ Maybe (Expression a) ]     -- Conditions
                [ [ Block a ] ]              -- Bodies
                (Maybe (Expression a))       -- Label to END IF

  | BlCase      a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                (Expression a)               -- Scrutinee
                [ Maybe (AList Index a) ]    -- Case ranges
                [ [ Block a ] ]              -- Bodies
                (Maybe (Expression a))       -- Label to END SELECT

  | BlDo        a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                (Maybe (Expression a))       -- Target label
                (Maybe (DoSpecification a))  -- Do Specification
                [ Block a ]                  -- Body
                (Maybe (Expression a))       -- Label to END DO

  | BlDoWhile   a SrcSpan
                (Maybe (Expression a))       -- Label
                (Maybe String)               -- Construct name
                (Maybe (Expression a))       -- Target label
                (Expression a)               -- Condition
                [ Block a ]                  -- Body
                (Maybe (Expression a))       -- Label to END DO

  | BlInterface a SrcSpan
                (Maybe (Expression a))       -- label
                Bool                         -- abstract?
                [ ProgramUnit a ]            -- Routine decls. in the interface
                [ Block a ]                  -- Module procedures

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
  | StNamelist            a SrcSpan (AList Namelist a)
  | StParameter           a SrcSpan (AList Declarator a)
  | StExternal            a SrcSpan (AList Expression a)
  | StIntrinsic           a SrcSpan (AList Expression a)
  | StCommon              a SrcSpan (AList CommonGroup a)
  | StEquivalence         a SrcSpan (AList (AList Expression) a)
  | StFormat              a SrcSpan (AList FormatItem a)
  | StImplicit            a SrcSpan (Maybe (AList ImpList a))
  | StEntry               a SrcSpan (Expression a) (Maybe (AList Expression a)) (Maybe (Expression a))
  | StInclude             a SrcSpan (Expression a) (Maybe [Block a])
  | StDo                  a SrcSpan (Maybe String) (Maybe (Expression a)) (Maybe (DoSpecification a))
  | StDoWhile             a SrcSpan (Maybe String) (Maybe (Expression a)) (Expression a)
  | StEnddo               a SrcSpan (Maybe String)
  | StCycle               a SrcSpan (Maybe (Expression a))
  | StExit                a SrcSpan (Maybe (Expression a))
  | StIfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)
  | StIfThen              a SrcSpan (Maybe String) (Expression a)
  | StElse                a SrcSpan (Maybe String)
  | StElsif               a SrcSpan (Maybe String) (Expression a)
  | StEndif               a SrcSpan (Maybe String)
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

data CommonGroup a =
  CommonGroup a SrcSpan (Maybe (Expression a)) (AList Expression a)
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
  | StructStructure a SrcSpan (Maybe String) (AList StructureItem a)
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
  -- ^ A unary operator applied to two expressions.
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
            (Maybe (Expression a)) -- Lower index
            (Maybe (Expression a)) -- Upper index
            (Maybe (Expression a)) -- Stride
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

data Declarator a =
    DeclVariable a SrcSpan
                 (Expression a) -- Variable
                 (Maybe (Expression a)) -- Length (character)
                 (Maybe (Expression a)) -- Initial value
  | DeclArray a SrcSpan
              (Expression a) -- Array
              (AList DimensionDeclarator a) -- Dimensions
              (Maybe (Expression a)) -- Length (character)
              (Maybe (Expression a)) -- Initial value
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

setInitialisation :: Declarator a -> Expression a -> Declarator a
setInitialisation (DeclVariable a s v l Nothing) init =
  DeclVariable a (getTransSpan s init) v l (Just init)
setInitialisation (DeclArray a s v ds l Nothing) init =
  DeclArray a (getTransSpan s init) v ds l (Just init)
-- do nothing when there is already a value
setInitialisation d _ = d

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

instance FirstParameter (AList t a) a
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

instance SecondParameter (AList t a) SrcSpan
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

instance Spanned (AList t a)
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

instance (Spanned a) => Spanned [a] where
  getSpan [] = error "Trying to find how long an empty list spans for."
  getSpan [x]   = getSpan x
  getSpan (x:xs) = getTransSpan x (last xs)
  setSpan _ _ = error "Cannot set span to an array"

instance (Spanned a, Spanned b) => Spanned (a, Maybe b) where
  getSpan (x, Just y) = getTransSpan x y
  getSpan (x,_) = getSpan x
  setSpan _ = undefined

instance (Spanned a, Spanned b) => Spanned (Maybe a, b) where
  getSpan (Just x,y) = getTransSpan x y
  getSpan (_,y) = getSpan y
  setSpan _ = undefined

instance (Spanned a, Spanned b) => Spanned (Either a b) where
  getSpan (Left x) = getSpan x
  getSpan (Right x) = getSpan x
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b) => Spanned (a, b) where
  getSpan (x,y) = getTransSpan x y
  setSpan _ = undefined

instance {-# OVERLAPPING #-}(Spanned a, Spanned b, Spanned c) => Spanned (Maybe a, Maybe b, Maybe c) where
  getSpan (Just x,_,Just z) = getTransSpan x z
  getSpan (Just x,Just y,Nothing) = getTransSpan x y
  getSpan (Nothing,Just y,Just z) = getTransSpan y z
  getSpan (Just x,Nothing,Nothing) = getSpan x
  getSpan (Nothing,Just y,Nothing) = getSpan y
  getSpan (Nothing,Nothing,Just z) = getSpan z
  getSpan (Nothing,Nothing,Nothing) = undefined
  setSpan _ = undefined

instance {-# OVERLAPPING #-}(Spanned a, Spanned b, Spanned c) => Spanned (a, Maybe b, Maybe c) where
  getSpan (x,_,Just z) = getTransSpan x z
  getSpan (x,Just y,Nothing) = getTransSpan x y
  getSpan (x,Nothing,Nothing) = getSpan x
  setSpan _ = undefined

instance {-# OVERLAPPING #-} (Spanned a, Spanned b, Spanned c) => Spanned (Maybe a, b, c) where
  getSpan (Just x,_,z) = getTransSpan x z
  getSpan (_,y,z) = getSpan (y,z)
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b, Spanned c) => Spanned (a, b, c) where
  getSpan (x,_,z) = getTransSpan x z
  setSpan _ = undefined

class (Spanned a, Spanned b) => SpannedPair a b where
  getTransSpan :: a -> b -> SrcSpan

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b) => SpannedPair a b where
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y

instance {-# OVERLAPS #-} (Spanned a, Spanned b) => SpannedPair a [b] where
  getTransSpan x [] = getSpan x
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y

instance {-# OVERLAPS #-} (Spanned a, Spanned b) => SpannedPair a [[b]] where
  getTransSpan x [] = getSpan x
  getTransSpan x y | all null y = getSpan x
  getTransSpan x y | any null y = getTransSpan x (filter (not . null) y)
  getTransSpan x y = SrcSpan l1 l2'
    where SrcSpan l1 _ = getSpan x
          SrcSpan _ l2' = getSpan y

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

class Conditioned f where
  getCondition :: f a -> Maybe (Expression a)

instance Conditioned Block where
  getCondition (BlStatement _ _ _ s) = getCondition s
  getCondition _ = Nothing

instance Conditioned Statement where
  getCondition (StIfThen _ _ _ c) = Just c
  getCondition (StElsif _ _ _ c) = Just c
  getCondition _ = Nothing

data ProgramUnitName =
    Named String
  | NamelessBlockData
  | NamelessComment
  | NamelessMain
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary ProgramUnitName

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

instance Out FortranVersion
instance Out MetaInfo
instance Out a => Out (ProgramFile a)
instance Out a => Out (ProgramUnit a)
instance Out a => Out (Prefix a)
instance Out a => Out (Suffix a)
instance (Out a, Out (t a)) => Out (AList t a)
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
instance Out CharacterLen
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
