-- | Supporting definitions for COMPLEX literals.

module Language.Fortran.AST.Literal.Complex where

import Language.Fortran.AST.Common ( Name )
import Language.Fortran.AST.Literal ( KindParam )
import Language.Fortran.AST.Literal.Real
import Language.Fortran.Util.Position ( SrcSpan, Spanned )

import GHC.Generics                   ( Generic )
import Data.Data                      ( Data, Typeable )
import Control.DeepSeq                ( NFData )
import Text.PrettyPrint.GenericPretty ( Out )
import Language.Fortran.Util.FirstParameter  ( FirstParameter )
import Language.Fortran.Util.SecondParameter ( SecondParameter )
import Language.Fortran.AST.Annotated ( Annotated )

-- | A COMPLEX literal, composed of a real part and an imaginary part.
--
-- Fortran has lots of rules on how COMPLEX literals are defined and used in
-- various contexts. To support all that, we define the syntactic structure
-- 'ComplexLit' to wrap all the parsing rules. Then during a analysis pass, you
-- may (attempt to) convert these into a more regular type, like a Haskell
-- @(Double, Double)@ tuple.
data ComplexLit a = ComplexLit
  { complexLitAnno     :: a
  , complexLitPos      :: SrcSpan
  , complexLitRealPart :: ComplexPart a
  , complexLitImagPart :: ComplexPart a
  } deriving stock    (Eq, Show, Data, Typeable, Generic, Functor)
    deriving anyclass (NFData, Out)

instance FirstParameter  (ComplexLit a) a
instance Annotated       ComplexLit
instance SecondParameter (ComplexLit a) SrcSpan
instance Spanned         (ComplexLit a)

-- | A part (either real or imaginary) of a complex literal.
--
-- Since Fortran 2003, complex literal parts support named constants, which must
-- be resolved in context at compile time (R422, R423).
--
-- Some compilers also allow constant expressions for the parts, and must
-- evaluate at compile time. That's not allowed in any standard. Apparently,
-- gfortran and ifort don't allow it, while nvfortran does. See:
-- https://fortran-lang.discourse.group/t/complex-constants-and-variables/2909/3
--
-- We specifically avoid supporting that by defining complex parts without being
-- mutually recursive with 'Expression'.
data ComplexPart a
  = ComplexPartReal   a SrcSpan RealLit (Maybe (KindParam a)) -- ^ signed real lit
  | ComplexPartInt    a SrcSpan String  (Maybe (KindParam a)) -- ^ signed int  lit
  | ComplexPartNamed  a SrcSpan Name                          -- ^ named constant
    deriving stock    (Eq, Show, Data, Typeable, Generic, Functor)
    deriving anyclass (NFData, Out)

instance FirstParameter  (ComplexPart a) a
instance Annotated       ComplexPart
instance SecondParameter (ComplexPart a) SrcSpan
instance Spanned         (ComplexPart a)

-- | Is the given COMPLEX literal "pure", i.e. does it have no named constant
--   components?
complexLitIsPure :: ComplexLit a -> Bool
complexLitIsPure c =
    check (complexLitRealPart c) && check (complexLitImagPart c)
  where check = \case ComplexPartNamed{} -> False
                      _                  -> True
