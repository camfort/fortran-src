{- | Machine Fortran INTEGER values.

This module stores Fortran INTEGER values in a matching Haskell machine integer
type. For example, an @INT(4)@ would be stored in an 'Int32'. This way, we get
both efficient operations and common overflow behaviour (which hopefully matches
most Fortran compilers), and explicitly encode kinding semantics via promoting
integral types.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Fortran.Repr.Value.Scalar.Int.Machine
  ( FInt(..)
  , SomeFInt
  , type IsFInt

  , fIntUOp
  , fIntUOp'
  , fIntUOpInplace
  , fIntUOpInplace'
  , fIntUOpInternal

  , fIntBOp
  , fIntBOp'
  , fIntBOpInplace
  , fIntBOpInplace'
  , fIntBOpInternal

  , withFInt

  , fIntConvertChecked
  , fIntType
  , toFInt
  ) where

import Language.Fortran.Repr.Type.Scalar.Int
import Language.Fortran.Repr.Value.Scalar.Common
import Data.Int
import Data.Functor.Const

import Data.Bits ( Bits )

import Language.Fortran.Repr.Util ( natVal'' )
import GHC.TypeNats

-- | A Fortran integer value, tagged with its kind.
data FInt (k :: FTInt) where
    FInt1 :: Int8  -> FInt 'FTInt1 -- ^ @INTEGER(1)@
    FInt2 :: Int16 -> FInt 'FTInt2 -- ^ @INTEGER(2)@
    FInt4 :: Int32 -> FInt 'FTInt4 -- ^ @INTEGER(4)@
    FInt8 :: Int64 -> FInt 'FTInt8 -- ^ @INTEGER(8)@
deriving stock instance Show (FInt k)
deriving stock instance Eq   (FInt k)
deriving stock instance Ord  (FInt k)

type IsFInt a = (Integral a, Bits a)

type SomeFInt = SomeFKinded FTInt FInt
deriving stock instance Show SomeFInt
instance Eq SomeFInt where
    (SomeFKinded l) == (SomeFKinded r) = fIntBOp (==) l r

-- | Low-level 'FInt' unary operator. Runs an operation over some 'FInt', and
--   stores it kinded. The user gets to choose how the kind is used: it can be
--   used to wrap the result back into an 'FInt', or ignored using 'Const'.
--
-- Pattern matches are ordered to match more common ops earlier.
fIntUOpInternal
    :: (Int8  -> ft 'FTInt1)
    -> (Int16 -> ft 'FTInt2)
    -> (Int32 -> ft 'FTInt4)
    -> (Int64 -> ft 'FTInt8)
    -> FInt k -> ft k
fIntUOpInternal k1f k2f k4f k8f = \case
  FInt4 i32 -> k4f i32
  FInt8 i64 -> k8f i64
  FInt2 i16 -> k2f i16
  FInt1 i8  -> k1f i8

-- | Run an operation over some 'FInt', with a concrete function for each kind.
fIntUOp'
    :: (Int8  -> r)
    -> (Int16 -> r)
    -> (Int32 -> r)
    -> (Int64 -> r)
    -> FInt k -> r
fIntUOp' k1f k2f k4f k8f =
      getConst
    . fIntUOpInternal (Const . k1f) (Const . k2f) (Const . k4f) (Const . k8f)

-- | Run an operation over some 'FInt'.
fIntUOp
    :: forall r k
    .  (forall a. IsFInt a => a -> r)
    -> FInt k -> r
fIntUOp f = fIntUOp' f f f f

-- | Run an inplace operation over some 'FInt', with a concrete function for
--   each kind.
fIntUOpInplace'
    :: (Int8  -> Int8)
    -> (Int16 -> Int16)
    -> (Int32 -> Int32)
    -> (Int64 -> Int64)
    -> FInt k -> FInt k
fIntUOpInplace' k1f k2f k4f k8f =
    fIntUOpInternal (FInt1 . k1f) (FInt2 . k2f) (FInt4 . k4f) (FInt8 . k8f)

-- | Run an inplace operation over some 'FInt'.
fIntUOpInplace
    :: (forall a. IsFInt a => a -> a)
    -> FInt k -> FInt k
fIntUOpInplace f = fIntUOpInplace' f f f f

-- | Low-level 'FInt' binary operator. Combine two 'FInt's, coercing different
--   kinds, and store the result kinded.
--
-- Pattern matches are ordered to match more common ops earlier.
fIntBOpInternal
    :: (Int8  -> Int8  -> ft 'FTInt1)
    -> (Int16 -> Int16 -> ft 'FTInt2)
    -> (Int32 -> Int32 -> ft 'FTInt4)
    -> (Int64 -> Int64 -> ft 'FTInt8)
    -> FInt kl -> FInt kr -> ft (FTIntCombine kl kr)
fIntBOpInternal k1f k2f k4f k8f il ir = case (il, ir) of
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

fIntBOp'
    :: (Int8  -> Int8  -> r)
    -> (Int16 -> Int16 -> r)
    -> (Int32 -> Int32 -> r)
    -> (Int64 -> Int64 -> r)
    -> FInt kl -> FInt kr -> r
fIntBOp' k1f k2f k4f k8f il ir =
      getConst
    $ fIntBOpInternal (go k1f) (go k2f) (go k4f) (go k8f) il ir
  where go g l r = Const $ g l r

fIntBOp
    :: (forall a. IsFInt a => a -> a -> r)
    -> FInt kl -> FInt kr -> r
fIntBOp f = fIntBOp' f f f f

fIntBOpInplace'
    :: (Int8  -> Int8  -> Int8)
    -> (Int16 -> Int16 -> Int16)
    -> (Int32 -> Int32 -> Int32)
    -> (Int64 -> Int64 -> Int64)
    -> FInt kl -> FInt kr -> FInt (FTIntCombine kl kr)
fIntBOpInplace' k1f k2f k4f k8f =
    fIntBOpInternal (go FInt1 k1f) (go FInt2 k2f) (go FInt4 k4f) (go FInt8 k8f)
  where go f g l r = f $ g l r

fIntBOpInplace
    :: (forall a. IsFInt a => a -> a -> a)
    -> FInt kl -> FInt kr -> FInt (FTIntCombine kl kr)
fIntBOpInplace f = fIntBOpInplace' f f f f

-- | Treat any 'FInt' as a 'Num'.
--
-- TODO remove. means being explicit with coercions to real in eval.
withFInt :: Num a => FInt k -> a
withFInt = fIntUOp fromIntegral

fIntMax :: forall (k :: FTInt). KnownNat (FTIntMax k) => Int64
fIntMax = fromIntegral $ natVal'' @(FTIntMax k)

fIntMin :: forall (k :: FTInt). KnownNat (FTIntMin k) => Int64
fIntMin = - (fromIntegral $ natVal'' @(FTIntMin k))

fIntConvertChecked
    :: forall kout kin
    .  (KnownNat (FTIntMax kout), KnownNat (FTIntMin kout))
    => SFTInt kout -> FInt kin -> (FInt kout, Maybe String)
fIntConvertChecked ty = fIntUOp $ \n ->
    let mErr =
            if fromIntegral n > fIntMax @kout then
                Just "too large for new size"
            else if fromIntegral n < fIntMin @kout then
                Just "too small for new size"
            else Nothing
     in case ty of
          SFTInt1  -> (FInt1 (fromIntegral n), mErr)
          SFTInt2  -> (FInt2 (fromIntegral n), mErr)
          SFTInt4  -> (FInt4 (fromIntegral n), mErr)
          SFTInt8  -> (FInt8 (fromIntegral n), mErr)
          SFTInt16 ->
            -- safe usage, because @'FInt' 'FTInt16'@ is not a valid type!
            error "impossible; INTEGER(16) value representation unsupported"

-- | Get the term level representation of an 'FInt''s type (i.e. @INTEGER(x)@).
--
-- TODO can also define this (and stronger funcs) with singletons. perhaps have
-- a look
fIntType :: FInt (k :: FTInt) -> FTInt
fIntType = \case
  FInt1{} -> FTInt1
  FInt2{} -> FTInt2
  FInt4{} -> FTInt4
  FInt8{} -> FTInt8

-- | Convert some Haskell number the requested @INTEGER(k)@ type, with no range
--   checks.
toFInt :: SFTInt k -> (forall a. IsFInt a => a) -> FInt k
toFInt ty n = case ty of
  SFTInt1  -> FInt1 n
  SFTInt2  -> FInt2 n
  SFTInt4  -> FInt4 n
  SFTInt8  -> FInt8 n
  SFTInt16 ->
    -- safe usage, because @'FInt' 'FTInt16'@ is not a valid type!
    error "impossible; INTEGER(16) value representation unsupported"
