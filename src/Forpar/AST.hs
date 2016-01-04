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

-- Basic AST nodes
data BaseType = 
  TypeInteger | TypeReal | TypeDoublePrecision | TypeComplex | TypeLogical
  deriving (Eq, Show, Data, Typeable)

instance Read BaseType where
  readsPrec _ value = 
    let options = [ ("integer", TypeInteger)
                  , ("real", TypeReal)
                  , ("doubleprecision", TypeDoublePrecision)
                  , ("complex", TypeComplex)
                  , ("logical", TypeLogical)] in
      tryTypes options
      where
        tryTypes [] = []
        tryTypes ((attempt,result):xs) = 
          if value == attempt then [(result, "")] else tryTypes xs

-- Program structure definition
type Program a = [ProgramUnit a]

data ProgramUnit a =
--    program type  | a  | span    | name | arguments        | return   | body                                      | comment
      PUMain          a    SrcSpan   Name                                 (AList (Maybe (Expression a), Block a) a)   ([Comment a])
  |   PUSubroutine    a    SrcSpan   Name   (AList a String)              (AList (Maybe (Expression a), Block a) a)   ([Comment a])
  |   PUFunction      a    SrcSpan   Name   (AList a String)   BaseType   (AList (Maybe (Expression a), Block a) a)   ([Comment a])
  |   PUBlockData     a    SrcSpan   Name                                 (AList (Maybe (Expression a), Block a) a)   ([Comment a])
  deriving (Eq, Show, Data, Typeable, Generic)

data Block a = BlStatement a SrcSpan (Statement a) ([Comment a])
  deriving (Eq, Show, Data, Typeable, Generic)

data Comment a = Comment a SrcSpan String deriving (Eq, Show, Data, Typeable, Generic)

data Statement a  = 
    StExternal            (AList (Expression a) a)
  | StDimension           (AList (Expression a) a)
  | StCommon              (AList (CommonGroup a) a)
  | StEquivalence         (AList (AList (Expression a) a) a)
  | StData                (AList (DataGroup a) a)
  | StFormat              (AList (FormatItem a) a)
  | StDeclaration         BaseType (AList (Expression a) a)
  | StDo                  (Expression a) (Block a) (Expression a) (Maybe (Expression a))
  | StIfLogical           (Expression a) (Block a) -- Statement should not further recurse
  | StIfArithmetic        (Expression a) (Expression a) (Expression a) (Expression a)
  | StFunction            Name (AList Name a) (Expression a)
  | StExpressionAssign    (Expression a) (Expression a)
  | StLabelAssign         (Expression a) (Expression a)
  | StGotoUnconditional   (Expression a)
  | StGotoAssigned        (Expression a) (AList (Expression a) a)
  | StGotoComputed        (AList (Expression a) a) (Expression a)
  | StCall                (Expression a) (AList (Expression a) a)
  | StReturn              
  | StContinue               
  | StStop                (Expression a)
  | StPause               (Expression a)
  | StRead                (Expression a) (Maybe (Expression a)) (AList (IOElement a) a)
  | StWrite               (Expression a) (Maybe (Expression a)) (AList (IOElement a) a)
  | StRewind              (Expression a)
  | StBackspace           (Expression a)
  | StEndfile             (Expression a)
  deriving (Eq, Show, Data, Typeable)

data CommonGroup a = 
  CommonGroup a SrcSpan (Maybe Name) (AList (Expression a) a)
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

data IOElement a = 
    IOExpression (Expression a)
  | IOTuple a SrcSpan (AList (IOElement a) a) (Expression a)
  | IOExpressionList (AList (Expression a) a) 
  deriving (Eq, Show, Data, Typeable)

data Expression a =
    ExpValue         a SrcSpan (Value a)
  | ExpBinary        a SrcSpan BinaryOp (Expression a) (Expression a)
  | ExpUnary         a SrcSpan UnaryOp (Expression a)
  | ExpSubscript     a SrcSpan (Expression a) (AList (Expression a) a)
  | ExpFunctionCall  a SrcSpan (Expression a) (AList (Expression a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

-- All recursive Values 
data Value a =
    ValInteger           String
--                       digits        .digits       e/d       sign          digits
  | ValReal              String
  | ValComplex           (Expression a) (Expression a)
  | ValHollerith         String
  | ValLabel             String
  | ValVariable          Name
  | ValArray             Name
  | ValTrue              
  | ValFalse             
  | ValFunctionName      Name
  | ValSubroutineName    Name
  deriving (Eq, Show, Data, Typeable, Generic)

data UnaryOp = Plus | Minus | Not deriving (Eq, Show, Data, Typeable)

data BinaryOp = 
    Addition 
  | Subtraction 
  | Multiplication 
  | Division
  | Exponentiation
  | GT
  | GTE
  | LT
  | LTE
  | EQ
  | NE
  | Or
  | And
  deriving (Eq, Show, Data, Typeable)

-- Retrieving SrcSpan and Annotation from nodes
class Annotated f where
  getAnnotation :: f a -> a
  setAnnotation :: a -> f a -> f a

  default getAnnotation :: (FirstParameter (f a) a) => f a -> a
  getAnnotation a = getFirstParameter a 

  default setAnnotation :: (FirstParameter (f a) a) => a -> f a -> f a
  setAnnotation e a = setFirstParameter e a 

instance FirstParameter (AList t a) a
instance FirstParameter (ProgramUnit a) a
instance FirstParameter (Block a) a
instance FirstParameter (CommonGroup a) a
instance FirstParameter (DataGroup a) a
instance FirstParameter (Comment a) a
instance FirstParameter (FormatItem a) a
instance FirstParameter (Expression a) a

instance SecondParameter (AList t a) SrcSpan
instance SecondParameter (ProgramUnit a) SrcSpan
instance SecondParameter (Block a) SrcSpan
instance SecondParameter (CommonGroup a) SrcSpan
instance SecondParameter (DataGroup a) SrcSpan
instance SecondParameter (Comment a) SrcSpan
instance SecondParameter (FormatItem a) SrcSpan
instance SecondParameter (Expression a) SrcSpan

instance Annotated (AList t)
instance Annotated ProgramUnit
instance Annotated Block
instance Annotated CommonGroup
instance Annotated DataGroup
instance Annotated Comment
instance Annotated FormatItem
instance Annotated Expression

instance Annotated IOElement where
  getAnnotation (IOExpression value) = getAnnotation value
  getAnnotation (IOTuple a _ _ _) = a
  getAnnotation (IOExpressionList list) = getAnnotation list

  setAnnotation e (IOExpression value) = IOExpression $ setAnnotation e value
  setAnnotation e (IOTuple _ a b c) = IOTuple e a b c
  setAnnotation e (IOExpressionList list) = IOExpressionList $ setAnnotation e list

class Spanned a where
  getSpan :: a -> SrcSpan
  setSpan :: SrcSpan -> a -> a

  default getSpan :: (SecondParameter a SrcSpan) => a -> SrcSpan
  getSpan a = getSecondParameter a

  default setSpan :: (SecondParameter a SrcSpan) => SrcSpan -> a -> a
  setSpan e a = setSecondParameter e a

instance Spanned (AList t a)
instance Spanned (ProgramUnit a)
instance Spanned (Block a)
instance Spanned (CommonGroup a)
instance Spanned (DataGroup a)
instance Spanned (Comment a)
instance Spanned (FormatItem a)
instance Spanned (Expression a)

instance Spanned (IOElement a) where
  getSpan (IOExpression value) = getSpan value
  getSpan (IOTuple _ s _ _) = s
  getSpan (IOExpressionList list) = getSpan list

  setSpan s (IOExpression value) = IOExpression $ setSpan s value
  setSpan s (IOTuple a _ b c) = IOTuple a s b c
  setSpan s (IOExpressionList list) = IOExpressionList $ setSpan s list

getTransSpan :: (Spanned a, Spanned b) => a -> b -> SrcSpan
getTransSpan x y =
  let SrcSpan l1 l2 = getSpan x
      SrcSpan l1' l2' = getSpan y in
        SrcSpan l1 l2'

getListSpan :: Spanned a => [a] -> SrcSpan
getListSpan [x] =  getSpan x
getListSpan (x:xs) = getTransSpan x (last xs)

class Commented f where
  setComments :: f a -> [ Comment a ] -> f a

instance Commented Block where
  setComments (BlStatement a s st _) comments = BlStatement a s st comments

instance Commented ProgramUnit where
  setComments progUnit comments =
    case progUnit of 
      PUMain        a s n b _         -> PUMain        a s n b comments
      PUSubroutine  a s n args b _    -> PUSubroutine  a s n args b comments
      PUFunction    a s n args r b _  -> PUFunction    a s n args r b comments
      PUBlockData   a s n b _         -> PUBlockData   a s n b comments

--------------------------------------------------------------------------------
-- Useful for testing                                                         --
--------------------------------------------------------------------------------

-- To be used in testing it reverts the SrcSpans in AST to dummy initial
-- SrcSpan value.
resetSrcSpan :: Data a => a -> a
resetSrcSpan = transformBi f
  where 
    f x = case cast x :: Maybe SrcSpan of 
      Just _ -> initSrcSpan
      Nothing -> x
