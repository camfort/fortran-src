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
  --  program type  | annotation  | span    | name | arguments        | return type | body 
  |   Main            a             SrcSpan   Name                                    (AList (Statement a) a)
  |   Subroutine      a             SrcSpan   Name   (AList String a)                 (AList (Statement a) a)
  |   Function        a             SrcSpan   Name   (AList String a)   BaseType      (AList (Statement a) a)
  |   BlockData       a             SrcSpan   Name                                    (AList (Statement a) a)

data Statement a  = 
    External            a SrcSpan (AList (Procedure a) a)
  | Dimension           a SrcSpan (AList (Expression a) a)
  | Common              a SrcSpan (AList (Name, AList Expression a) a)
  | Equivalence         a SrcSpan (AList (AList Name a) a)
  | Data                a SrcSpan (AList (AList (Expression a) a, AList (Expression a) a) a)
  | Format              a SrcSpan (AList [FormatItem] a)
  | Function            a SrcSpan Name (AList Name a) Expr
  | Declaration         a SrcSpan BaseType (AList (Expression a))
  | Do                  a SrcSpan (Label a) (Expression a) (AList (Expression a) a)
  | IfLogical           a SrcSpan (Expression a) (Statement a) -- Statement should not further recurse
  | IfAritchmetic       a SrcSpan (Expression a) (Label a) (Label a) (Label a)
  | ExpressionAssign    a SrcSpan (Expression a) (Expression a)
  | LabelAssign         a SrcSpan (Expression a) (Label a)
  | GotoUnconditional   a SrcSpan (Label a)
  | GotoAssigned        a SrcSpan (Expression a) (AList (Label a) a)
  | GotoComputed        a SrcSpan (AList (Label a) a) (Expression a)
  | Call                a SrcSpan (Expression a) (AList (CallArgument a) a)
  | Return              a SrcSpan
  | Stop                a SrcSpan
  | Pause               a SrcSpan (Value a)
  | Read                a SrcSpan (Expression a) (Maybe Form) (AList (IOElement a) a)
  | Write               a SrcSpan (Expression a) (Maybe Form) (AList (IOElement a) a)
  | Rewind              a SrcSpan (Expression a)
  | Backspace           a SrcSpan (Expression a)
  | Endfile             a SrcSpan (Expression a)

data Procedure a = Function   a SrcSpan Name
                 | Subroutine a SrcSpan Name

data Label a = Label a SrcSpan String

data Form a = Format a (FormatItem a) | Label a (Label a)

data FormatItem a = FormatList a SrcSpan (AList FormatItem a)
                  | Hollerith  a SrcSpan String
                  | Delimiter  a SrcSpan String
--                  descriptor type      | annotation | span    | repeat  | descriptor  | width   | integer 
                  | FieldDescriptorDFEG    a            SrcSpan   Integer   Char          Integer   Integer
                  | FieldDescriptorILA     a            SrcSpan   Integer   Char          Integer
                  | BlankDescriptor        a            SrcSpan   Integer
                  | ScaleFactor            a            SrcSpan   Integer

data IOElement a = Value (Value a)
                 | Tuple (AList (IOElement a) a) (Expression a) a
                 | ValueList (AList (Value a) a) 

data Expression a =
    Value         a SrcSpan (Value a)
  | Binary        a SrcSpan BinaryOp (Expression a) (Expression a)
  | Unary         a SrcSpan UnaryOp (Expression a)
  | Subscript     a SrcSpan Name (AList (Expression a) a)
  | FunctionCall  a SrcSpan Name (AList (Expression a) a)

data Value a =
    NumericConstant   a SrcSpan String
  | Integer           a SrcSpan Sign (Value a)
  | Real              a SrcSpan (Value a) (Maybe (Value a)) (Maybe (Value a))
  | DoublePrecision   a SrcSpan (Value a) (Maybe (Value a)) (Maybe (Value a))
  | Complex           a SrcSpan (Value a) (Value a)
  | Hollerith         a SrcSpan String
  | Variable          a SrcSpan Name
  | Array             a SrcSpan Name
  | True              a SrcSpan
  | False             a SrcSpan

data UnaryOp = Minus | Not

data Sign = Plus | Minus

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
