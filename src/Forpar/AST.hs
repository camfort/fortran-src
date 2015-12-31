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

-- Program structure definition
type Program a = [ProgramUnit a]

data ProgramUnit a =
--    program type  | annotation  | span    | name | arguments        | return type | body                    | comment
      PUMain          a             SrcSpan   Name                                    (AList (Block a) a)       ([Comment a])
  |   PUSubroutine    a             SrcSpan   Name   (AList a String)                 (AList (Block a) a)       ([Comment a])
  |   PUFunction      a             SrcSpan   Name   (AList a String)   BaseType      (AList (Block a) a)       ([Comment a])
  |   PUBlockData     a             SrcSpan   Name                                    (AList (Block a) a)       ([Comment a])
  deriving (Eq, Show, Data, Typeable, Generic)

-- This node is for various grouping structures such as large IFs, LOOPs
-- and single statements that don't fit into either category.
data Block a =
    BlStatement a SrcSpan                                           (Statement a)                                   ([Comment a])
  | BlDo        a SrcSpan (Statement a) (Value a) (Maybe (Value a)) (AList (Block a) a)                             ([Comment a])
  | BlIf        a SrcSpan (Expression a)                            (AList (Block a) a) (Maybe (AList (Block a) a)) ([Comment a])
  deriving (Eq, Show, Data, Typeable, Generic)

data Comment a = Comment a SrcSpan String deriving (Eq, Show, Data, Typeable, Generic)

data Statement a  = 
    StExternal            (AList (Expression a) a)
  | StDimension           (AList (Expression a) a)
  | StCommon              (AList (CommonGroup a) a)
  | StEquivalence         (AList (AList (Expression a) a) a)
  | StData                (AList (DataGroup a) a)
  | StFormat              (AList (FormatItem a) a)
  | StFunction            Name (AList Name a) (Expression a)
  | StDeclaration         BaseType (AList (Value a) a)
  | StDo                  (Block a) (Value a) (Maybe (Value a))
  | StIfLogical           (Expression a) (Block a) -- Statement should not further recurse
  | StIfAritchmetic       (Expression a) (Value a) (Value a) (Value a)
  | StExpressionAssign    (Value a) (Expression a)
  | StLabelAssign         (Value a) (Value a)
  | StGotoUnconditional   (Value a)
  | StGotoAssigned        (Expression a) (AList (Value a) a)
  | StGotoComputed        (AList (Value a) a) (Expression a)
  | StCall                (Expression a) (AList (Expression a) a)
  | StReturn              
  | StStop                
  | StPause               (Value a)
  | StRead                (Value a) (Maybe (Form a)) (AList (IOElement a) a)
  | StWrite               (Value a) (Maybe (Form a)) (AList (IOElement a) a)
  | StRewind              (Value a)
  | StBackspace           (Value a)
  | StEndfile             (Value a)
  deriving (Eq, Show, Data, Typeable)

data CommonGroup a = 
  CommonGroup a SrcSpan (Maybe Name) (AList (Expression a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

data DataGroup a =
  DataGroup a SrcSpan (AList (Expression a) a) (AList (Expression a) a)
  deriving (Eq, Show, Data, Typeable, Generic)

data Form a = 
    Format (FormatItem a) 
  | Label (Expression a) 
  deriving (Eq, Show, Data, Typeable)

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
    Value (Expression a)
  | Tuple a SrcSpan (AList (IOElement a) a) (Expression a)
  | ValueList (AList (Value a) a) 
  deriving (Eq, Show, Data, Typeable)

data Expression a =
    ExpValue         a SrcSpan (Value a)
  | ExpBinary        a SrcSpan BinaryOp (Expression a) (Expression a)
  | ExpUnary         a SrcSpan UnaryOp (Expression a)
  | ExpSubscript     a SrcSpan (Value a) (AList (Expression a) a)
  | ExpFunctionCall  a SrcSpan (Value a) (AList (Expression a) a)
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

instance Annotated Form where
  getAnnotation (Format formatItem) = getAnnotation formatItem
  getAnnotation (Label value) = getAnnotation value

  setAnnotation e (Format formatItem) = Format $ setAnnotation e formatItem
  setAnnotation e (Label value) = Label $ setAnnotation e value

instance Annotated IOElement where
  getAnnotation (Value value) = getAnnotation value
  getAnnotation (Tuple a _ _ _) = a
  getAnnotation (ValueList list) = getAnnotation list

  setAnnotation e (Value value) = Value $ setAnnotation e value
  setAnnotation e (Tuple _ a b c) = Tuple e a b c
  setAnnotation e (ValueList list) = ValueList $ setAnnotation e list

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

instance Spanned (Form a) where
  getSpan (Format formatItem) = getSpan formatItem
  getSpan (Label value) = getSpan value

  setSpan s (Format formatItem) = Format $ setSpan s formatItem
  setSpan s (Label value) = Label $ setSpan s value

instance Spanned (IOElement a) where
  getSpan (Value value) = getSpan value
  getSpan (Tuple _ s _ _) = s
  getSpan (ValueList list) = getSpan list

  setSpan s (Value value) = Value $ setSpan s value
  setSpan s (Tuple a _ b c) = Tuple a s b c
  setSpan s (ValueList list) = ValueList $ setSpan s list

getTransSpan :: (Spanned a, Spanned b) => a -> b -> SrcSpan
getTransSpan x y =
  let SrcSpan l1 l2 = getSpan x
      SrcSpan l1' l2' = getSpan y in
        SrcSpan l1 l2'

getListSpan :: Spanned a => [a] -> SrcSpan
getListSpan [x] =  getSpan x
getListSpan (x:xs) = getTransSpan x (last xs)

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
