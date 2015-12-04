module AST where

type SrcSpan = (Integer, Integer)

type Name = String

-- AST is polymorphic on some type a as that type is used for arbitrary
-- annotations

-- Many AST nodes such as executable statements, declerations, etc. may
-- appear in lists, hence a dedicated annotated list type is defined
data AList t a = AList a (Maybe SrcSpan) [t] deriving (Eq)

instance Functor (AList t a) where
  fmap f (AList xs a) = AList (map f xs) a

-- Basic AST nodes
data BaseType = 
  Integer | Real | DoublePrecision | Complex | Logical
  deriving (Eq)

-- Program structure definition
data Program a = [ProgUnit a] deriving (Eq)

data ProgramUnit a =
  --  program type  | annotation  | span    | name | arguments        | return type     | body 
  |   Main            a             SrcSpan   Name                                        Block a
  |   Subroutine      a             SrcSpan   Name   (AList String a)                     Block a
  |   Function        a             SrcSpan   Name   (AList String a)   BaseType          Block a
  |   BlockData       a             SrcSpan   Name                                        Block a

--             annotation | declerations            | statements
data Block a = Block a      (AList (Prelude a) a)     (AList (Epilogue a) a)

data Prelude a = PreSpec (Specification a)
               | PreData (Data a)
               | PreFormat (Format a)
               | PreFunc (InlineFunction a)

data Epilogue a = EpiExec (Executable a)
                | EpiFormat (Format a)
                | EpiData (Data a)

data Specification a = -- TODO

type Data a = AList a (AList a (Expression a), AList a (Expression a))

data Format a = AList a -- TODO

--                      program type    | annotation  | span    | name  | arguments         | body 
data InlineFunction a = InlineFunction    a             SrcSpan   Name    (AList String a)    Expr

data Executable a = -- TODO
