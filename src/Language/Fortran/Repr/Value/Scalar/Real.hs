module Language.Fortran.Repr.Value.Scalar.Real where

import Language.Fortran.Repr.Type.Scalar.Real
import Language.Fortran.Repr.Value.Scalar.Common
import GHC.Float ( float2Double )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )

data FReal
  = FReal4 {- ^ @REAL(4)@ -} Float
  | FReal8 {- ^ @REAL(8)@ -} Double
    deriving stock (Show, Generic, Data)
    deriving anyclass (Binary, Out)

instance FKinded FReal where
    type FKindedT FReal = FTReal
    type FKindedC FReal a = RealFloat a
    fKind = \case
      FReal4{} -> FTReal4
      FReal8{} -> FTReal8

instance Eq FReal where (==) = fRealBOp (==)

fRealUOp'
    :: (Float  -> r)
    -> (Double -> r)
    -> FReal -> r
fRealUOp' k4f k8f = \case
  FReal4 fl -> k4f fl
  FReal8 db -> k8f db

fRealBOp'
    :: (Float  -> Float  -> r)
    -> (Double -> Double -> r)
    -> FReal -> FReal -> r
fRealBOp' k4f k8f l r = case (l, r) of
  (FReal4 lr, FReal4 rr) -> k4f lr rr
  (FReal8 lr, FReal8 rr) -> k8f lr rr
  (FReal4 lr, FReal8 rr) -> k8f (float2Double lr) rr
  (FReal8 lr, FReal4 rr) -> k8f lr (float2Double rr)

fRealUOpInplace'
    :: (Float  -> Float)
    -> (Double -> Double)
    -> FReal -> FReal
fRealUOpInplace' k4f k8f = fRealUOp' (FReal4 . k4f) (FReal8 . k8f)

fRealBOpInplace'
    :: (Float  -> Float  -> Float)
    -> (Double -> Double -> Double)
    -> FReal -> FReal -> FReal
fRealBOpInplace' k4f k8f = fRealBOp' (f FReal4 k4f) (f FReal8 k8f)
  where f cstr bop l r = cstr $ bop l r

fRealUOp
    :: (forall a. FKindedC FReal a => a -> r)
    -> FReal -> r
fRealUOp f = fRealUOp' f f

fRealUOpInplace
    :: (forall a. FKindedC FReal a => a -> a)
    -> FReal -> FReal
fRealUOpInplace f = fRealUOpInplace' f f

fRealBOp
    :: (forall a. FKindedC FReal a => a -> a -> r)
    -> FReal -> FReal -> r
fRealBOp f = fRealBOp' f f

fRealBOpInplace
    :: (forall a. FKindedC FReal a => a -> a -> a)
    -> FReal -> FReal -> FReal
fRealBOpInplace f = fRealBOpInplace' f f
