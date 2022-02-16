{-
Some values store some info also used at the type level. This is done where that
info can be used on the value level to influence how the value is used e.g. to
check bounds.
-}

module Language.Fortran.Repr.Value.Scalar where

import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.AST.Literal.Boz ( Boz )

import           Data.Int                       ( Int8, Int16, Int32, Int64 )
import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                      as Text
import           Data.Void

data FValScalar
  = FValScalarInt     FValInt
  | FValScalarReal    FValReal
  | FValScalarLogical Bool
  | FValScalarComplex FValComplex
  | FValScalarBoz     Boz
  | FValScalarString  Text
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- TODO hide constructor, make smart constructor
-- | Fortran INTEGER value.
data FValInt = FValInt
  { fvalIntKind :: FTypeInt
  , fvalInt     :: Integer
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | A bounds-checked operation on 'ty'. True is valid, False is invalid.
type CheckedOp ty = ty -> ty -> (ty, Bool)

fValIntIsWellBounded :: FValInt -> Bool
fValIntIsWellBounded (FValInt t v) = not $ v > fTypeIntMax t || v < fTypeIntMin t

-- | Apply a binary operator to two 'FValInt's and check that no bounds issues may
--   have been present.
--
-- I think we always do the same checks? For integers, we have to check both
-- bounds every time. So pulled this out.
fValIntSafeBinOp :: (Integer -> Integer -> Integer) -> CheckedOp FValInt
fValIntSafeBinOp op (FValInt t1 v1) (FValInt t2 v2) = (FValInt t v, isInBound)
  where
    v = v1 `op` v2
    t = max t1 t2
    isInBound = fValIntIsWellBounded $ FValInt t v

fValIntSafeAdd :: CheckedOp FValInt
fValIntSafeAdd = fValIntSafeBinOp (+)

fValIntSafeMinus :: CheckedOp FValInt
fValIntSafeMinus = fValIntSafeBinOp (-)

-- Always safe.
fValIntNegate :: FValInt -> FValInt
fValIntNegate (FValInt k i) = FValInt k (-i)

toRuntimeRepr :: FValInt -> FValInt
toRuntimeRepr (FValInt t x) =
  FValInt t $
    case t of
      FTypeInt1 -> toInteger ((fromInteger x) :: Int8)
      FTypeInt2 -> toInteger ((fromInteger x) :: Int16)
      FTypeInt4 -> toInteger ((fromInteger x) :: Int32)
      FTypeInt8 -> toInteger ((fromInteger x) :: Int64)

-- TODO no distinguishing between REAL(4) and REAL(8) on value level
-- could make a sum type instead
data FValReal = FValReal FTypeReal Double
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

fValRealAdd :: FValReal -> FValReal -> FValReal
fValRealAdd (FValReal t1 r1) (FValReal t2 r2) = FValReal (max t1 t2) (r1+r2)

fValRealMinus :: FValReal -> FValReal -> FValReal
fValRealMinus (FValReal t1 r1) (FValReal t2 r2) = FValReal (max t1 t2) (r1-r2)

-- Always safe.
fValRealNegate :: FValReal -> FValReal
fValRealNegate (FValReal k r) = FValReal k (-r)

data FValComplex = FValComplex FTypeReal Double Double
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

fValComplexAdd :: FValComplex -> FValComplex -> FValComplex
fValComplexAdd (FValComplex t1 c1r c1i) (FValComplex t2 c2r c2i) =
    FValComplex (max t1 t2) (c1r+c2r) (c1i+c2i)

fValComplexMinus :: FValComplex -> FValComplex -> FValComplex
fValComplexMinus (FValComplex t1 c1r c1i) (FValComplex t2 c2r c2i) =
    FValComplex (max t1 t2) (c1r-c2r) (c1i-c2i)

-- Always safe.
fValComplexNegate :: FValComplex -> FValComplex
fValComplexNegate (FValComplex k r i) = FValComplex k (-r) (-i)

-- orphan instances...
instance Out Text where
    doc = doc . Text.unpack
    docPrec = const doc
instance Out Void where
    doc = const mempty
    docPrec = const doc
