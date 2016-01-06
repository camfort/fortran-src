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

aCons :: t -> AList t a -> AList t a
aCons x (AList a s xs) = AList a s $ x:xs

aReverse :: AList t a -> AList t a
aReverse (AList a s xs) = AList a s $ reverse xs

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
--    program type  | a  | span    | return           | name         | arguments        | body                              
      PUMain          a    SrcSpan                      (Maybe Name)                      [(Maybe (Expression a), Block a)]
  |   PUSubroutine    a    SrcSpan                      Name           (AList String a)   [(Maybe (Expression a), Block a)]
  |   PUFunction      a    SrcSpan   (Maybe BaseType)   Name           (AList String a)   [(Maybe (Expression a), Block a)]
  |   PUBlockData     a    SrcSpan                                                        [(Maybe (Expression a), Block a)]
  deriving (Eq, Show, Data, Typeable, Generic)

data Block a = BlStatement a SrcSpan (Statement a) ([Comment a])
  deriving (Eq, Show, Data, Typeable, Generic)

data Comment a = Comment a SrcSpan String deriving (Eq, Show, Data, Typeable, Generic)

data Statement a  = 
    StExternal            a SrcSpan (AList (Expression a) a)
  | StDimension           a SrcSpan (AList (Expression a) a)
  | StCommon              a SrcSpan (AList (CommonGroup a) a)
  | StEquivalence         a SrcSpan (AList (AList (Expression a) a) a)
  | StData                a SrcSpan (AList (DataGroup a) a)
  | StFormat              a SrcSpan (AList (FormatItem a) a)
  | StDeclaration         a SrcSpan BaseType (AList (Expression a) a)
  | StDo                  a SrcSpan (Expression a) (Statement a) (Expression a) (Maybe (Expression a))
  | StIfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | StIfArithmetic        a SrcSpan (Expression a) (Expression a) (Expression a) (Expression a)
  | StFunction            a SrcSpan Name (AList Name a) (Expression a)
  | StExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | StLabelAssign         a SrcSpan (Expression a) (Expression a)
  | StGotoUnconditional   a SrcSpan (Expression a)
  | StGotoAssigned        a SrcSpan (Expression a) (AList (Expression a) a)
  | StGotoComputed        a SrcSpan (AList (Expression a) a) (Expression a)
  | StCall                a SrcSpan (Expression a) (AList (Expression a) a)
  | StReturn              a SrcSpan
  | StContinue            a SrcSpan
  | StStop                a SrcSpan (Expression a)
  | StPause               a SrcSpan (Expression a)
  | StRead                a SrcSpan (Expression a) (Maybe (Expression a)) (AList (IOElement a) a)
  | StWrite               a SrcSpan (Expression a) (Maybe (Expression a)) (AList (IOElement a) a)
  | StRewind              a SrcSpan (Expression a)
  | StBackspace           a SrcSpan (Expression a)
  | StEndfile             a SrcSpan (Expression a)
  deriving (Eq, Show, Data, Typeable, Generic)

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
  | IOExpressionList a SrcSpan (AList (Expression a) a) 
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
instance FirstParameter (Statement a) a
instance FirstParameter (CommonGroup a) a
instance FirstParameter (DataGroup a) a
instance FirstParameter (Comment a) a
instance FirstParameter (FormatItem a) a
instance FirstParameter (Expression a) a

instance SecondParameter (AList t a) SrcSpan
instance SecondParameter (ProgramUnit a) SrcSpan
instance SecondParameter (Block a) SrcSpan
instance SecondParameter (Statement a) SrcSpan
instance SecondParameter (CommonGroup a) SrcSpan
instance SecondParameter (DataGroup a) SrcSpan
instance SecondParameter (Comment a) SrcSpan
instance SecondParameter (FormatItem a) SrcSpan
instance SecondParameter (Expression a) SrcSpan

instance Annotated (AList t)
instance Annotated ProgramUnit
instance Annotated Block
instance Annotated Statement
instance Annotated CommonGroup
instance Annotated DataGroup
instance Annotated Comment
instance Annotated FormatItem
instance Annotated Expression

instance Annotated IOElement where
  getAnnotation (IOExpression value) = getAnnotation value
  getAnnotation (IOTuple a _ _ _) = a
  getAnnotation (IOExpressionList a _ _) = a

  setAnnotation e (IOExpression value) = IOExpression $ setAnnotation e value
  setAnnotation e (IOTuple _ s b c) = IOTuple e s b c
  setAnnotation e (IOExpressionList _ s list) = IOExpressionList e s list

instance Spanned (AList t a)
instance Spanned (ProgramUnit a)
instance Spanned (Statement a)
instance Spanned (Block a)
instance Spanned (CommonGroup a)
instance Spanned (DataGroup a)
instance Spanned (Comment a)
instance Spanned (FormatItem a)
instance Spanned (Expression a)

instance Spanned (IOElement a) where
  getSpan (IOExpression value) = getSpan value
  getSpan (IOTuple _ s _ _) = s
  getSpan (IOExpressionList _ s _) = s

  setSpan s (IOExpression value) = IOExpression $ setSpan s value
  setSpan s (IOTuple a _ b c) = IOTuple a s b c
  setSpan s (IOExpressionList a _ list) = IOExpressionList a s list

instance Spanned a => Spanned ([a]) where
  getSpan xs = getListSpan xs
  setSpan _ _ = error "Cannot set span to an array"

instance (Spanned a, Spanned b) => Spanned (Maybe a, b) where
  getSpan (Just x,y) = getTransSpan x y
  getSpan (_,y) = getSpan y
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b) => Spanned (a, b) where
  getSpan (x,y) = getTransSpan x y
  setSpan _ = undefined

instance (Spanned a, Spanned b, Spanned c) => Spanned (a, b, c) where
  getSpan (x,y,z) = getTransSpan x z
  setSpan _ = undefined

instance {-# OVERLAPPABLE #-} (Spanned a, Spanned b, Spanned c) => Spanned (Maybe a, b, c) where
  getSpan (Just x,_,z) = getTransSpan x z
  getSpan (_,y,z) = getSpan (y,z)
  setSpan _ = undefined

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
