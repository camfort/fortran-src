{- | Machine Fortran INTEGER values.

This module stores Fortran INTEGER values in a matching Haskell machine integer
type. For example, an @INT(4)@ would be stored in an 'Int32'. This way, we get
both efficient operations and common overflow behaviour (which hopefully matches
most Fortran compilers), and explicitly encode kinding semantics via promoting
integral types.
-}

module Language.Fortran.Repr.Value.Scalar.Int.Machine where

import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Common
import Data.Int

import Data.Bits ( Bits )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out )
import Text.PrettyPrint.GenericPretty.Orphans()

-- | A Fortran integer value, type @INTEGER(k)@.
data FInt
  = FInt1 {- ^ @INTEGER(1)@ -} Int8
  | FInt2 {- ^ @INTEGER(2)@ -} Int16
  | FInt4 {- ^ @INTEGER(4)@ -} Int32
  | FInt8 {- ^ @INTEGER(8)@ -} Int64
    deriving stock (Show, Generic, Data)
    deriving anyclass (Binary, Out)

instance FKinded FInt where
    type FKindedT FInt = FTInt
    type FKindedC FInt a = (Integral a, Bits a)
    fKind = \case
      FInt1{} -> FTInt1
      FInt2{} -> FTInt2
      FInt4{} -> FTInt4
      FInt8{} -> FTInt8

instance Eq FInt where (==) = fIntBOp (==)

withFInt :: Num a => FInt -> a
withFInt = fIntUOp fromIntegral

-- Pattern matches are ordered to match more common ops earlier.
fIntUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> FInt -> r
fIntUOp' k1f k2f k4f k8f = \case
  FInt4 i32 -> k4f i32
  FInt8 i64 -> k8f i64
  FInt2 i16 -> k2f i16
  FInt1 i8  -> k1f i8

-- Pattern matches are ordered to match more common ops earlier.
fIntBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> FInt -> FInt -> r
fIntBOp' k1f k2f k4f k8f il ir = case (il, ir) of
  (FInt4 l32, FInt4 r32) -> k4f l32 r32
  (FInt8 l64, FInt8 r64) -> k8f l64 r64

  (FInt4 l32, FInt8 r64) -> k8f (fromIntegral l32) r64
  (FInt8 l64, FInt4 r32) -> k8f l64 (fromIntegral r32)

  (FInt4 l32, FInt2 r16) -> k4f l32 (fromIntegral r16)
  (FInt2 l16, FInt4 r32) -> k4f (fromIntegral l16) r32

  (FInt4 l32, FInt1 r8)  -> k4f l32 (fromIntegral r8)
  (FInt1 l8,  FInt4 r32) -> k4f (fromIntegral l8) r32

  (FInt8 l64, FInt2 r16) -> k8f l64 (fromIntegral r16)
  (FInt2 l16, FInt8 r64) -> k8f (fromIntegral l16) r64

  (FInt8 l64, FInt1 r8)  -> k8f l64 (fromIntegral r8)
  (FInt1 l8,  FInt8 r64) -> k8f (fromIntegral l8) r64

  (FInt2 l16, FInt2 r16) -> k2f l16 r16
  (FInt2 l16, FInt1 r8)  -> k2f l16 (fromIntegral r8)
  (FInt1 l8,  FInt2 r16) -> k2f (fromIntegral l8) r16

  (FInt1 l8,  FInt1 r8)  -> k1f l8 r8

fIntUOpInplace'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> FInt -> FInt
fIntUOpInplace' k1f k2f k4f k8f =
    fIntUOp' (FInt1 . k1f) (FInt2 . k2f) (FInt4 . k4f) (FInt8 . k8f)

fIntBOpInplace'
    :: (Int8  -> Int8  -> Int8)
    -> (Int16 -> Int16 -> Int16)
    -> (Int32 -> Int32 -> Int32)
    -> (Int64 -> Int64 -> Int64)
    -> FInt -> FInt -> FInt
fIntBOpInplace' k1f k2f k4f k8f =
    fIntBOp' (f FInt1 k1f) (f FInt2 k2f) (f FInt4 k4f) (f FInt8 k8f)
  where f cstr bop l r = cstr $ bop l r

fIntUOp :: (forall a. FKindedC FInt a => a -> r) -> FInt -> r
fIntUOp f = fIntUOp' f f f f

fIntUOpInplace :: (forall a. FKindedC FInt a => a -> a) -> FInt -> FInt
fIntUOpInplace f = fIntUOpInplace' f f f f

fIntBOp :: (forall a. FKindedC FInt a => a -> a -> r) -> FInt -> FInt -> r
fIntBOp f = fIntBOp' f f f f

fIntBOpInplace :: (forall a. FKindedC FInt a => a -> a -> a) -> FInt -> FInt -> FInt
fIntBOpInplace f = fIntBOpInplace' f f f f

{-

-- TODO improve: always return answer, plus a flag indicating if there was an
-- error, plus this should be in eval instead and this should be simpler
-- (shouldn't be wrapping in Either)
fIntCoerceChecked :: FTInt -> FInt -> Either String FInt
fIntCoerceChecked ty = fIntUOp $ \n ->
    if fromIntegral n > fIntMax @kout then
        Left "too large for new size"
    else if fromIntegral n < fIntMin @kout then
        Left "too small for new size"
    else
        case ty of
          FTInt1  -> Right $ FInt1 $ fromIntegral n
          FTInt2  -> Right $ FInt2 $ fromIntegral n
          FTInt4  -> Right $ FInt4 $ fromIntegral n
          FTInt8  -> Right $ FInt8 $ fromIntegral n
          FTInt16 -> Left "can't represent INTEGER(16) yet, sorry"

-}
