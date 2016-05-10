{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Fortran.AST where

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty

import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Debug.Trace

type A0 = ()

type Name = String

-- AST is polymorphic on some type a as that type is used for arbitrary
-- annotations

-- Many AST nodes such as executable statements, declerations, etc. may
-- appear in lists, hence a dedicated annotated list type is defined
data AList t a = AList a SrcSpan [t a] deriving (Eq, Show, Data, Typeable, Generic)
instance Functor t => Functor (AList t) where
  fmap f (AList a s xs) = AList (f a) s (map (fmap f) xs)

fromList :: Spanned (t a) => a -> [ t a ] -> AList t a
fromList a xs = AList a (getSpan xs) xs

fromReverseList :: Spanned (t ()) => [ t () ] -> AList t ()
fromReverseList = fromList () . reverse

aCons :: t a -> AList t a -> AList t a
aCons x (AList a s xs) = AList a s $ x:xs

infixr 5 `aCons`

aReverse :: AList t a -> AList t a
aReverse (AList a s xs) = AList a s $ reverse xs

aStrip :: AList t a -> [t a]
aStrip (AList _ _ l) = l

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
  | TypeCharacter
  | TypeCustom String
  deriving (Eq, Show, Data, Typeable, Generic)

data TypeSpec a = TypeSpec a SrcSpan BaseType (Maybe (Selector a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Selector a =
--                   Maybe length         | Maybe kind
  Selector a SrcSpan (Maybe (Expression a)) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- Program structure definition
data ProgramFile a = ProgramFile [ ([ Block a ], ProgramUnit a) ] [ Block a ]
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

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
      Bool -- Recursive or not
      Name
      (AList Expression a) -- Arguments
      [Block a] -- Body
  | PUFunction
      a SrcSpan
      (Maybe (TypeSpec a)) -- Return type
      Bool -- Recursive or not
      Name
      (AList Expression a) -- Arguments
      (Maybe (Expression a)) -- Result
      [Block a] -- Body
  | PUBlockData
      a SrcSpan
      (Maybe Name)
      [Block a] -- Body
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Block a =
    BlStatement a SrcSpan (Maybe (Expression a)) (Statement a)
  | BlIf a SrcSpan (Maybe (Expression a)) [ Maybe (Expression a) ] [ [ Block a ] ]
  | BlDo a SrcSpan (Maybe (Expression a)) (DoSpecification a) [ Block a ]
  | BlDoWhile a SrcSpan (Maybe (Expression a)) (Expression a) [ Block a ]
  | BlInterface a SrcSpan (Expression a) [ ProgramUnit a ] [ Block a ]
  | BlComment a SrcSpan String
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Statement a  =
    StDeclaration         a SrcSpan (TypeSpec a) (Maybe (AList Attribute a)) (AList Declarator a)
  | StIntent              a SrcSpan Intent (AList Expression a)
  | StOptional            a SrcSpan (AList Expression a)
  | StPublic              a SrcSpan (Maybe (AList Expression a))
  | StPrivate             a SrcSpan (Maybe (AList Expression a))
  | StSave                a SrcSpan (Maybe (AList Expression a))
  | StDimension           a SrcSpan (AList Declarator a)
  | StAllocatable         a SrcSpan (AList Declarator a)
  | StPointer             a SrcSpan (AList Declarator a)
  | StTarget              a SrcSpan (AList Declarator a)
  | StData                a SrcSpan (AList DataGroup a)
  | StNamelist            a SrcSpan (AList Namelist a)
  | StParameter           a SrcSpan (AList Declarator a)
  | StExternal            a SrcSpan (AList Expression a)
  | StIntrinsic           a SrcSpan (AList Expression a)
  | StCommon              a SrcSpan (AList CommonGroup a)
  | StEquivalence         a SrcSpan (AList (AList Expression) a)
  | StFormat              a SrcSpan (AList FormatItem a)
  | StImplicit            a SrcSpan (Maybe (AList ImpList a))
  | StEntry               a SrcSpan (Expression a) (Maybe (AList Expression a)) (Maybe (Expression a))
  | StDo                  a SrcSpan (Maybe (Expression a)) (Maybe (Expression a)) (DoSpecification a)
  | StDoWhile             a SrcSpan (Maybe (Expression a)) (Maybe (Expression a)) (Expression a)
  | StEnddo               a SrcSpan (Maybe (Expression a))
  | StCycle               a SrcSpan (Maybe (Expression a))
  | StExit                a SrcSpan (Maybe (Expression a))
  | StIfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)
  | StIfThen              a SrcSpan (Maybe (Expression a)) (Expression a)
  | StElse                a SrcSpan (Maybe (Expression a))
  | StElsif               a SrcSpan (Maybe (Expression a)) (Expression a)
  | StEndif               a SrcSpan (Maybe (Expression a))
  | StSelectCase          a SrcSpan (Maybe (Expression a)) (Expression a)
  | StCase                a SrcSpan (Maybe (Expression a)) (Maybe (Index a))
  | StEndcase             a SrcSpan (Maybe (Expression a))
  | StFunction            a SrcSpan (Expression a) (AList Expression a) (Expression a)
  | StExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | StPointerAssign       a SrcSpan (Expression a) (Expression a)
  | StLabelAssign         a SrcSpan (Expression a) (Expression a)
  | StGotoUnconditional   a SrcSpan (Expression a)
  | StGotoAssigned        a SrcSpan (Expression a) (AList Expression a)
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
  | StOpen                a SrcSpan (AList ControlPair a)
  | StClose               a SrcSpan (AList ControlPair a)
  | StInquire             a SrcSpan (AList ControlPair a)
  | StRewind              a SrcSpan (AList ControlPair a)
  | StRewind2             a SrcSpan (Expression a)
  | StBackspace           a SrcSpan (AList ControlPair a)
  | StBackspace2          a SrcSpan (Expression a)
  | StEndfile             a SrcSpan (AList ControlPair a)
  | StEndfile2            a SrcSpan (Expression a)
  | StAllocate            a SrcSpan (AList Expression a) (Maybe (ControlPair a))
  | StNullify             a SrcSpan (AList Expression a)
  | StDeallocate          a SrcSpan (AList Expression a) (Maybe (ControlPair a))
  | StWhere               a SrcSpan (Expression a) (Statement a)
  | StWhereConstruct      a SrcSpan (Expression a)
  | StElsewhere           a SrcSpan
  | StEndWhere            a SrcSpan
  | StUse                 a SrcSpan (Expression a) (Maybe (AList Use a))
  | StModuleProcedure     a SrcSpan (AList Expression a)
  | StType                a SrcSpan (Maybe (AList Attribute a)) String
  | StEndType             a SrcSpan (Maybe String)
  | StSequence            a SrcSpan
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Use a =
    UseRename a SrcSpan (Expression a) (Expression a)
  | UseID a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Argument a = Argument a SrcSpan (Maybe String) (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Attribute a =
    AttrParameter a SrcSpan
  | AttrPublic a SrcSpan
  | AttrPrivate a SrcSpan
  | AttrAllocatable a SrcSpan
  | AttrDimension a SrcSpan (AList DimensionDeclarator a)
  | AttrExternal a SrcSpan
  | AttrIntent a SrcSpan Intent
  | AttrIntrinsic a SrcSpan
  | AttrOptional a SrcSpan
  | AttrPointer a SrcSpan
  | AttrSave a SrcSpan
  | AttrTarget a SrcSpan
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Intent = In | Out | InOut
  deriving (Eq, Show, Data, Typeable, Generic)

data ControlPair a = ControlPair a SrcSpan (Maybe String) (Expression a)
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

data DoSpecification a =
  DoSpecification a SrcSpan (Statement a) (Expression a) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Expression a =
    ExpValue         a SrcSpan (Value a)
  | ExpBinary        a SrcSpan BinaryOp (Expression a) (Expression a)
  | ExpUnary         a SrcSpan UnaryOp (Expression a)
  | ExpSubscript     a SrcSpan (Expression a) (AList Index a)
  | ExpDataRef       a SrcSpan (Expression a) (Expression a)
  | ExpFunctionCall  a SrcSpan (Expression a) (AList Argument a)
  | ExpImpliedDo     a SrcSpan (AList Expression a) (DoSpecification a)
  | ExpInitialisation  a SrcSpan (AList Expression a)
  | ExpReturnSpec    a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data Index a =
    IxSingle a SrcSpan (Expression a)
  | IxRange a SrcSpan
            (Maybe (Expression a)) -- Lower index
            (Maybe (Expression a)) -- Upper index
            (Maybe (Expression a)) -- Stride
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

-- All recursive Values
data Value a =
    ValInteger           String
  | ValReal              String
  | ValComplex           (Expression a) (Expression a)
  | ValString            String
  | ValHollerith         String
  | ValLabel             String
  | ValVariable          a Name
  | ValLogical           String
  | ValOperator          String
  | ValAssignment
  | ValType              String
  | ValStar
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

data DimensionDeclarator a =
  DimensionDeclarator a SrcSpan (Maybe (Expression a)) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic, Functor)

data UnaryOp =
    Plus
  | Minus
  | Not
  | UnCustom String
  deriving (Eq, Show, Data, Typeable, Generic)

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
  | And
  | Equivalent
  | NotEquivalent
  | BinCustom String
  deriving (Eq, Show, Data, Typeable, Generic)

-- Retrieving SrcSpan and Annotation from nodes
class Annotated f where
  getAnnotation :: f a -> a
  setAnnotation :: a -> f a -> f a

  default getAnnotation :: (FirstParameter (f a) a) => f a -> a
  getAnnotation = getFirstParameter

  default setAnnotation :: (FirstParameter (f a) a) => a -> f a -> f a
  setAnnotation = setFirstParameter

instance FirstParameter (AList t a) a
instance FirstParameter (ProgramUnit a) a
instance FirstParameter (Block a) a
instance FirstParameter (Statement a) a
instance FirstParameter (Argument a) a
instance FirstParameter (Use a) a
instance FirstParameter (TypeSpec a) a
instance FirstParameter (Selector a) a
instance FirstParameter (Attribute a) a
instance FirstParameter (ImpList a) a
instance FirstParameter (ImpElement a) a
instance FirstParameter (CommonGroup a) a
instance FirstParameter (DataGroup a) a
instance FirstParameter (Namelist a) a
instance FirstParameter (FormatItem a) a
instance FirstParameter (Expression a) a
instance FirstParameter (Index a) a
instance FirstParameter (DoSpecification a) a
instance FirstParameter (Declarator a) a
instance FirstParameter (DimensionDeclarator a) a
instance FirstParameter (ControlPair a) a

instance SecondParameter (AList t a) SrcSpan
instance SecondParameter (ProgramUnit a) SrcSpan
instance SecondParameter (Block a) SrcSpan
instance SecondParameter (Statement a) SrcSpan
instance SecondParameter (Argument a) SrcSpan
instance SecondParameter (Use a) SrcSpan
instance SecondParameter (TypeSpec a) SrcSpan
instance SecondParameter (Selector a) SrcSpan
instance SecondParameter (Attribute a) SrcSpan
instance SecondParameter (ImpList a) SrcSpan
instance SecondParameter (ImpElement a) SrcSpan
instance SecondParameter (CommonGroup a) SrcSpan
instance SecondParameter (DataGroup a) SrcSpan
instance SecondParameter (Namelist a) SrcSpan
instance SecondParameter (FormatItem a) SrcSpan
instance SecondParameter (Expression a) SrcSpan
instance SecondParameter (Index a) SrcSpan
instance SecondParameter (DoSpecification a) SrcSpan
instance SecondParameter (Declarator a) SrcSpan
instance SecondParameter (DimensionDeclarator a) SrcSpan
instance SecondParameter (ControlPair a) SrcSpan

instance Annotated (AList t)
instance Annotated ProgramUnit
instance Annotated Block
instance Annotated Statement
instance Annotated Argument
instance Annotated Use
instance Annotated TypeSpec
instance Annotated Selector
instance Annotated Attribute
instance Annotated ImpList
instance Annotated ImpElement
instance Annotated CommonGroup
instance Annotated DataGroup
instance Annotated Namelist
instance Annotated FormatItem
instance Annotated Expression
instance Annotated Index
instance Annotated DoSpecification
instance Annotated Declarator
instance Annotated DimensionDeclarator
instance Annotated ControlPair

instance Spanned (AList t a)
instance Spanned (ProgramUnit a)
instance Spanned (Statement a)
instance Spanned (Argument a)
instance Spanned (Use a)
instance Spanned (Attribute a)
instance Spanned (TypeSpec a)
instance Spanned (Selector a)
instance Spanned (ImpList a)
instance Spanned (ImpElement a)
instance Spanned (Block a)
instance Spanned (CommonGroup a)
instance Spanned (DataGroup a)
instance Spanned (Namelist a)
instance Spanned (FormatItem a)
instance Spanned (Expression a)
instance Spanned (Index a)
instance Spanned (DoSpecification a)
instance Spanned (Declarator a)
instance Spanned (DimensionDeclarator a)
instance Spanned (ControlPair a)

instance Spanned a => Spanned [a] where
  getSpan [ ] = error "Trying to get find how long an empty list spans."
  getSpan [x] =  getSpan x
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
  getSpan (x,y,z) = getTransSpan x z
  setSpan _ = undefined

getTransSpan :: (Spanned a, Spanned b) => a -> b -> SrcSpan
getTransSpan x y =
  let SrcSpan l1 l2 = getSpan x
      SrcSpan l1' l2' = getSpan y in
        SrcSpan l1 l2'

class Labeled f where
  getLabel :: f a -> Maybe (Expression a)
  setLabel :: f a -> Expression a -> f a

instance Labeled Block where
  getLabel (BlStatement _ _ l _) = l
  getLabel (BlIf _ _ l _ _) = l
  getLabel (BlDo _ _ l _ _) = l
  getLabel (BlDoWhile _ _ l _ _) = l
  getLabel _ = Nothing

  setLabel (BlStatement a s _ st) l = BlStatement a s (Just l) st
  setLabel (BlIf a s _ conds bs) l = BlIf a s (Just l) conds bs
  setLabel (BlDo a s _ spec bs) l = BlDo a s (Just l) spec bs
  setLabel (BlDoWhile a s _ spec bs) l = BlDoWhile a s (Just l) spec bs
  setLabel b l = b

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
  | NamelessMain
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

class Named a where
  getName :: a -> ProgramUnitName
  setName :: ProgramUnitName -> a -> a

instance Named (ProgramUnit a) where
  getName (PUMain _ _ Nothing _ _)     = NamelessMain
  getName (PUMain _ _ (Just n) _ _)    = Named n
  getName (PUModule _ _ n _ _)         = Named n
  getName (PUSubroutine _ _ _ n _ _)   = Named n
  getName (PUFunction _ _ _ _ n _ _ _) = Named n
  getName (PUBlockData _ _ Nothing _)  = NamelessBlockData
  getName (PUBlockData _ _ (Just n) _) = Named n
  setName (Named n) (PUMain       a s _ b pus) = PUMain a s (Just n) b pus
  setName _         (PUMain       a s _ b pus) = PUMain a s Nothing b pus
  setName (Named n) (PUModule     a s _ b pus) = PUModule a s n b pus
  setName (Named n) (PUSubroutine a s r _ p b) = PUSubroutine a s r n p b
  setName (Named n) (PUFunction   a s r rec _ p res b) = PUFunction a s r rec n p res b
  setName (Named n) (PUBlockData  a s _ b)     = PUBlockData  a s (Just n) b
  setName _         (PUBlockData  a s _ b)     = PUBlockData  a s Nothing b

instance Out a => Out (ProgramFile a)
instance Out a => Out (ProgramUnit a)
instance (Out a, Out (t a)) => Out (AList t a)
instance Out a => Out (Statement a)
instance Out a => Out (Argument a)
instance Out a => Out (Use a)
instance Out a => Out (Attribute a)
instance Out Intent
instance Out a => Out (ImpList a)
instance Out a => Out (ImpElement a)
instance Out a => Out (Block a)
instance Out a => Out (CommonGroup a)
instance Out a => Out (DataGroup a)
instance Out a => Out (Namelist a)
instance Out a => Out (FormatItem a)
instance Out a => Out (Expression a)
instance Out a => Out (Index a)
instance Out a => Out (DoSpecification a)
instance Out a => Out (Value a)
instance Out a => Out (TypeSpec a)
instance Out a => Out (Selector a)
instance Out BaseType
instance Out a => Out (Declarator a)
instance Out a => Out (DimensionDeclarator a)
instance Out a => Out (ControlPair a)
instance Out UnaryOp
instance Out BinaryOp


