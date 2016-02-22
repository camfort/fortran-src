{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Forpar.AST where

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data
import GHC.Generics
import Control.Newtype
import Text.PrettyPrint.GenericPretty

import Forpar.Util.Position
import Forpar.Util.FirstParameter
import Forpar.Util.SecondParameter

import Debug.Trace

newtype Flip f x y = Flip (f y x)

instance Newtype (Flip f x y) (f y x) where
  pack = Flip
  unpack (Flip z) = z

type Name = String

-- AST is polymorphic on some type a as that type is used for arbitrary
-- annotations

-- Many AST nodes such as executable statements, declerations, etc. may
-- appear in lists, hence a dedicated annotated list type is defined
data AList t a = AList a SrcSpan [t] deriving (Eq, Show, Data, Typeable, Generic)

instance Functor (Flip AList a) where
  fmap f (Flip (AList a s xs)) = Flip (AList a s (map f xs))

aCons :: t -> AList t a -> AList t a
aCons x (AList a s xs) = AList a s $ x:xs

infixr 5 `aCons`

aReverse :: AList t a -> AList t a
aReverse (AList a s xs) = AList a s $ reverse xs

-- Basic AST nodes
data BaseType a = 
    TypeInteger         a SrcSpan 
  | TypeReal            a SrcSpan 
  | TypeDoublePrecision a SrcSpan 
  | TypeComplex         a SrcSpan 
  | TypeLogical         a SrcSpan
  | TypeCharacter       a SrcSpan (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic)

-- Program structure definition
data ProgramFile a = ProgramFile [ ([ Block a ], ProgramUnit a) ] [ Block a ]
  deriving (Eq, Show, Data, Typeable, Generic)

data ProgramUnit a =
--    program type  | a  | span    | return               | name         | arguments        | body      
      PUMain          a    SrcSpan                          (Maybe Name)                      [Block a]
  |   PUSubroutine    a    SrcSpan                          Name           (AList String a)   [Block a]
  |   PUFunction      a    SrcSpan   (Maybe (BaseType a))   Name           (AList String a)   [Block a]
  |   PUBlockData     a    SrcSpan                          (Maybe Name)                      [Block a]
  deriving (Eq, Show, Data, Typeable, Generic)

data Block a = 
    BlStatement a SrcSpan (Maybe (Expression a)) (Statement a)
  | BlIf a SrcSpan (Maybe (Expression a)) [ Maybe (Expression a) ] [ [ Block a ] ]
  | BlComment a SrcSpan String
  deriving (Eq, Show, Data, Typeable, Generic)

data Statement a  = 
    StExternal            a SrcSpan (AList (Expression a) a)
  | StIntrinsic           a SrcSpan (AList (Expression a) a)
  | StDimension           a SrcSpan (AList (Declarator a) a)
  | StCommon              a SrcSpan (AList (CommonGroup a) a)
  | StEquivalence         a SrcSpan (AList (AList (Expression a) a) a)
  | StData                a SrcSpan (AList (DataGroup a) a)
  | StFormat              a SrcSpan (AList (FormatItem a) a)
  | StDeclaration         a SrcSpan (BaseType a) (AList (Declarator a) a)
  | StImplicit            a SrcSpan (Maybe (AList (ImpList a) a))
  | StParameter           a SrcSpan (AList (Statement a) a)
  | StEntry               a SrcSpan (Expression a) (Maybe (AList (Expression a) a))
  | StDo                  a SrcSpan (Expression a) (DoSpecification a)
  | StIfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)
  | StIfThen              a SrcSpan (Expression a)
  | StElse                a SrcSpan
  | StElsif               a SrcSpan (Expression a)
  | StEndif               a SrcSpan
  | StFunction            a SrcSpan Name (AList Name a) (Expression a)
  | StExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | StLabelAssign         a SrcSpan (Expression a) (Expression a)
  | StGotoUnconditional   a SrcSpan (Expression a)
  | StGotoAssigned        a SrcSpan (Expression a) (AList (Expression a) a)
  | StGotoComputed        a SrcSpan (AList (Expression a) a) (Expression a)
  | StCall                a SrcSpan (Expression a) (Maybe (AList (Expression a) a))
  | StReturn              a SrcSpan (Maybe (Expression a))
  | StSave                a SrcSpan (AList (Expression a) a)
  | StContinue            a SrcSpan
  | StStop                a SrcSpan (Maybe (Expression a))
  | StPause               a SrcSpan (Maybe (Expression a))
  | StRead                a SrcSpan (AList (ControlPair a) a) (Maybe (AList (Expression a) a))
  | StRead2               a SrcSpan (Expression a) (Maybe (AList (Expression a) a))
  | StWrite               a SrcSpan (AList (ControlPair a) a) (Maybe (AList (Expression a) a))
  | StPrint               a SrcSpan (Expression a) (Maybe (AList (Expression a) a))
  | StOpen                a SrcSpan (AList (ControlPair a) a)
  | StClose               a SrcSpan (AList (ControlPair a) a)
  | StInquire             a SrcSpan (AList (ControlPair a) a)
  | StRewind              a SrcSpan (AList (ControlPair a) a)
  | StRewind2             a SrcSpan (Expression a)
  | StBackspace           a SrcSpan (AList (ControlPair a) a)
  | StBackspace2          a SrcSpan (Expression a)
  | StEndfile             a SrcSpan (AList (ControlPair a) a)
  | StEndfile2            a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic)

data ControlPair a = ControlPair a SrcSpan (Maybe String) (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic)

data ImpList a = ImpList a SrcSpan (BaseType a) (AList (ImpElement a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

data ImpElement a = 
    ImpCharacter    a SrcSpan String
  | ImpRange        a SrcSpan String String
  deriving (Eq, Show, Data, Typeable, Generic)

data CommonGroup a = 
  CommonGroup a SrcSpan (Maybe (Expression a)) (AList (Expression a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

data DataGroup a =
  DataGroup a SrcSpan (AList (Expression a) a) (AList (Expression a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

data FormatItem a = 
    FIFormatList            a             SrcSpan   (Maybe String) (AList (FormatItem a) a)
  | FIHollerith             a             SrcSpan   (Value a)
  | FIDelimiter             a             SrcSpan
--  descriptor type       | annotation  | span    | repeat          | descriptor  | width   | integer 
  | FIFieldDescriptorDEFG   a             SrcSpan   (Maybe Integer)   Char          Integer   Integer
  | FIFieldDescriptorAIL    a             SrcSpan   (Maybe Integer)   Char          Integer
  | FIBlankDescriptor       a             SrcSpan   Integer
  | FIScaleFactor           a             SrcSpan   Integer
  deriving (Eq, Show, Data, Typeable, Generic)

data DoSpecification a = 
  DoSpecification a SrcSpan (Statement a) (Expression a) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic)

data Expression a =
    ExpValue         a SrcSpan (Value a)
  | ExpBinary        a SrcSpan BinaryOp (Expression a) (Expression a)
  | ExpUnary         a SrcSpan UnaryOp (Expression a)
  | ExpSubscript     a SrcSpan (Expression a) (AList (Expression a) a)
  | ExpSubstring     a SrcSpan (Expression a) (Maybe (Expression a)) (Maybe (Expression a))
  | ExpFunctionCall  a SrcSpan (Expression a) (AList (Expression a) a)
  | ExpImpliedDo     a SrcSpan (AList (Expression a) a) (DoSpecification a)
  deriving (Eq, Show, Data, Typeable, Generic)

-- All recursive Values 
data Value a =
    ValInteger           String
  | ValReal              String
  | ValComplex           (Expression a) (Expression a)
  | ValString            String
  | ValHollerith         String
  | ValLabel             String
  | ValVariable          Name
  | ValParameter         Name
  | ValArray             Name
  | ValTrue              
  | ValFalse             
  | ValFunctionName      Name
  | ValSubroutineName    Name
  | ValCommonName        Name
  | ValStar
  deriving (Eq, Show, Data, Typeable, Generic)

data Declarator a =
    DeclArray a SrcSpan (Expression a) (AList (DimensionDeclarator a) a)
  | DeclCharArray a SrcSpan (Expression a) (AList (DimensionDeclarator a) a) (Maybe (Expression a))
  | DeclVariable a SrcSpan (Expression a)
  | DeclCharVariable a SrcSpan (Expression a) (Maybe (Expression a))
  deriving (Eq, Show, Data, Typeable, Generic)

data DimensionDeclarator a = 
  DimensionDeclarator a SrcSpan (Maybe (Expression a)) (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic)

data UnaryOp = Plus | Minus | Not deriving (Eq, Show, Data, Typeable, Generic)

data BinaryOp = 
    Addition 
  | Subtraction 
  | Multiplication 
  | Division
  | Exponentiation
  | Concatination
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
instance FirstParameter (ImpList a) a
instance FirstParameter (ImpElement a) a
instance FirstParameter (CommonGroup a) a
instance FirstParameter (DataGroup a) a
instance FirstParameter (FormatItem a) a
instance FirstParameter (Expression a) a
instance FirstParameter (DoSpecification a) a
instance FirstParameter (BaseType a) a
instance FirstParameter (Declarator a) a
instance FirstParameter (DimensionDeclarator a) a
instance FirstParameter (ControlPair a) a

instance SecondParameter (AList t a) SrcSpan
instance SecondParameter (ProgramUnit a) SrcSpan
instance SecondParameter (Block a) SrcSpan
instance SecondParameter (Statement a) SrcSpan
instance SecondParameter (ImpList a) SrcSpan
instance SecondParameter (ImpElement a) SrcSpan
instance SecondParameter (CommonGroup a) SrcSpan
instance SecondParameter (DataGroup a) SrcSpan
instance SecondParameter (FormatItem a) SrcSpan
instance SecondParameter (Expression a) SrcSpan
instance SecondParameter (DoSpecification a) SrcSpan
instance SecondParameter (BaseType a) SrcSpan
instance SecondParameter (Declarator a) SrcSpan
instance SecondParameter (DimensionDeclarator a) SrcSpan
instance SecondParameter (ControlPair a) SrcSpan

instance Annotated (AList t)
instance Annotated ProgramUnit
instance Annotated Block
instance Annotated Statement
instance Annotated ImpList
instance Annotated ImpElement
instance Annotated CommonGroup
instance Annotated DataGroup
instance Annotated FormatItem
instance Annotated Expression
instance Annotated DoSpecification
instance Annotated BaseType
instance Annotated Declarator
instance Annotated DimensionDeclarator
instance Annotated ControlPair

instance Spanned (AList t a)
instance Spanned (ProgramUnit a)
instance Spanned (Statement a)
instance Spanned (ImpList a)
instance Spanned (ImpElement a)
instance Spanned (Block a)
instance Spanned (CommonGroup a)
instance Spanned (DataGroup a)
instance Spanned (FormatItem a)
instance Spanned (Expression a)
instance Spanned (DoSpecification a)
instance Spanned (BaseType a)
instance Spanned (Declarator a)
instance Spanned (DimensionDeclarator a)
instance Spanned (ControlPair a)

instance Spanned a => Spanned [a] where
  getSpan = getListSpan
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

getListSpan :: Spanned a => [a] -> SrcSpan
getListSpan [x] =  getSpan x
getListSpan (x:xs) = getTransSpan x (last xs)

class Labeled f where
  getLabel :: f a -> Maybe (Expression a)
  setLabel :: f a -> (Expression a) -> f a

instance Labeled Block where
  getLabel (BlStatement _ _ l _) = l
  getLabel (BlIf _ _ l _ _) = l

  setLabel (BlStatement a s _ st) l = BlStatement a s (Just l) st
  setLabel (BlIf a s _ conds bs) l = BlIf a s (Just l) conds bs

class Conditioned f where
  getCondition :: f a -> Maybe (Expression a)

instance Conditioned Block where
  getCondition (BlStatement _ _ _ s) = getCondition s
  getCondition _ = Nothing

instance Conditioned Statement where
  getCondition (StIfThen _ _ c) = Just c
  getCondition (StElsif _ _ c) = Just c
  getCondition _ = Nothing

data ProgramUnitName =
    Named String
  | NamelessBlockData
  | NamelessMain
  deriving (Ord, Eq, Show)

class Named a where
  getName :: a -> ProgramUnitName

instance Named (ProgramUnit a) where
  getName (PUMain _ _ Nothing _) = NamelessMain
  getName (PUMain _ _ (Just n) _) = Named n
  getName (PUSubroutine _ _ n _ _) = Named n
  getName (PUFunction _ _ _ n _ _) = Named n
  getName (PUBlockData _ _ Nothing _) = NamelessBlockData
  getName (PUBlockData _ _ (Just n) _) = Named n

instance Out a => Out (ProgramFile a)
instance Out a => Out (ProgramUnit a)
instance (Out a, Out t) => Out (AList t a)
instance Out a => Out (Statement a)
instance Out a => Out (ImpList a)
instance Out a => Out (ImpElement a)
instance Out a => Out (Block a)
instance Out a => Out (CommonGroup a)
instance Out a => Out (DataGroup a)
instance Out a => Out (FormatItem a)
instance Out a => Out (Expression a)
instance Out a => Out (DoSpecification a)
instance Out a => Out (Value a)
instance Out a => Out (BaseType a)
instance Out a => Out (Declarator a)
instance Out a => Out (DimensionDeclarator a)
instance Out a => Out (ControlPair a)
instance Out UnaryOp
instance Out BinaryOp
