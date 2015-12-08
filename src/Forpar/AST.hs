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

data Block a = Block  a             (AList (Statement a) a)

data Statement a  = 
    External            a SrcSpan (AList Name a)
  | Dimension           a SrcSpan (AList (Declarator a) a)
  | Common              a SrcSpan (AList (Name, AList Declarator a) a)
  | Equivalence         a SrcSpan (AList (AList Name a) a)
  | Data                a SrcSpan (AList (AList (Declarator a) a, AList (Expression a) a) a)
  | Format              a SrcSpan (AList [FormatItem] a)
  | Function            a SrcSpan Name (AList Name a) Expr
  | Declaration         a SrcSpan BaseType (AList Declarator a)
  | IfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | GotoUnconditional   a SrcSpan (Label a)
  | GotoAssigned        a SrcSpan Name (AList (Label a) a)
  | GotoComputed        a SrcSpan (AList (Label a) a) Name
  | IfAritchmetic       a SrcSpan (Expression a) (Label a) (Label a) (Label a)
  | Call                a SrcSpan Name (AList Name a)
  | Return              a SrcSpan
  | Stop                a SrcSpan
  | Pause               a SrcSpan (Expression a) -- TODO add constant (which will be an expression)
  | Read                a SrcSpan Index (Maybe Form)) (AList  a) -- TODO list of what

data Declarator a = VariableDeclarator  a SrcSpan Name
                  | ArrayDeclarator     a SrcSpan Name (AList Index a)
data Index = Variable Name | Constant String
data Form = Format FormatItem | Label String

data FormatItem = FormatList [FormatItem]
                | Hollerith String
                | Delimiter
--                descriptor type     | repeat  | descriptor  | width   | integer 
                | FieldDescriptorDFEG   Integer   Char          Integer   Integer
                | FieldDescriptorILA    Integer   Char          Integer
                | BlankDescriptor       Integer
                | ScaleFactor           Integer
