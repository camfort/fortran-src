{- |
Precise Fortran type & value model.

Due to Fortran syntax design and the fortran-src definitions handling multiple
evolutions of the language, the syntactic constructs in 'Language.Fortran.AST'
for Fortran types and values are clunky and awkward to use for modelling safe
operations. The representations in this sub-package enable performing efficient
operations with explicit, documented semantics (usually the de facto behaviour,
or adopted from gfortran).

The aims for this representation are _correctness_ and _efficiency_. All values
store enough information on the type level to recover their precise Fortran type
via inspection.

TODO

  * Data (SYB) doesn't play nice with GADTs. They *are* entirely possible
    together with singletons, but remain extremely finicky. It was a source of
    issues during development. So no nice GADTs :(

-}

module Language.Fortran.Repr
  (
  -- * Assorted notes
  -- ** Kind semantics
  -- $kind-semantics

  -- ** Exceptional behaviour
  -- $exceptional-behaviour

  -- ** Naming conventions
  -- $naming-conventions

  -- * Re-exports
  -- ** Fortran types
    module Language.Fortran.Repr.Type
  , module Language.Fortran.Repr.Type.Scalar
  , module Language.Fortran.Repr.Type.Scalar.Common
  , module Language.Fortran.Repr.Type.Scalar.Int
  , module Language.Fortran.Repr.Type.Scalar.Real
  , module Language.Fortran.Repr.Type.Scalar.Complex
  , module Language.Fortran.Repr.Type.Scalar.String

  -- ** Fortran values
  , module Language.Fortran.Repr.Value
  , module Language.Fortran.Repr.Value.Scalar
  , module Language.Fortran.Repr.Value.Scalar.Common
  , module Language.Fortran.Repr.Value.Scalar.Int
  , module Language.Fortran.Repr.Value.Scalar.Real
  , module Language.Fortran.Repr.Value.Scalar.Complex
  , module Language.Fortran.Repr.Value.Scalar.Logical
  , module Language.Fortran.Repr.Value.Scalar.String
  ) where

import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Scalar.Common
import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Type.Scalar.Complex
import Language.Fortran.Repr.Type.Scalar.String

import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar
import Language.Fortran.Repr.Value.Scalar.Common
import Language.Fortran.Repr.Value.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Complex
import Language.Fortran.Repr.Value.Scalar.Logical
import Language.Fortran.Repr.Value.Scalar.String

{- $kind-semantics

Kinds in Fortran are natural number "tags" associated with certain intrinsic
types. They enable Fortran implementations to group similar types of value
together under the same Fortran type. That is, though an @INTEGER(4)@ and an
@INTEGER(8)@ are both integers, most Fortran compilers will use different
representations for the values. We match this in Haskell by defining a sum type
for a given Fortran type, and making a constructor for each valid kind.

Fortran standards do not specify full semantics for kinds, only things like
interactions and precision requirements. However, average modern Fortran
compilers tend to agree on certain things. So we follow gfortran's lead for
semantics. The following general rules exist:

  * The size in bytes of a stored value is equal to its type's kind value. For
    example, a @REAL(4)@ takes 4 bytes. In general, for any type, only powers of
    2 are ever valid kinds.
  * Different types have different permitted kind values. This is what prevents
    us from simply carrying around a type name and a kind. For example, in our
    representation (and most in use), @REAL(2)@ isn't a valid type, while
    @INTEGER(2)@ is.
-}

{- $exceptional-behaviour

Where possible, this representation also matches common exceptional behaviours
in Fortran expression evaluation - specifically using gfortran as a basis. For
example:

  * Integers overflow predictably.
  * Reals should have approximately matching behaviour, since both gfortran and
    Haskell use IEEE floats.
-}

{- $naming-conventions

To prevent clashes with common Haskell types and definitions, most
representation types are prefixed with @F@, read as _Fortran_.
-}
