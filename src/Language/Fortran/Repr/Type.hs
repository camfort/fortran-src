{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

{-
TODO ignoring character kind for now
TODO support kind *16 (e.g. INT*16 -> Int16)
TODO no way to talk about "intrinsic type with any rep" (e.g. ignore INT kind,
     CHAR len). annoying for intrinsics etc.
-}

module Language.Fortran.Repr.Type where

import           Data.Int

import           Data.Data                      ( Data, Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

{-

For a sec, I kind of wanted to do this. Then I realized it's just singletons. So
let's quietly pretend I didn't.

    data TFT where
        TFT :: TFTS a -> TFSR a -> TFT

    data TFTS t where
      TFTSInt  :: TFTS 'TFTSInt
      TFTSReal :: TFTS 'TFTSReal

    type family TFSR (t :: TFTS) where
        TFSR 'TFTSInt  = ()
        TFSR 'TFTSReal = ()

-}

-- | Fortran type.
data FType
  = FTypeScalar FTypeScalar
  | FTypeArray' FTypeArray
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | Fortran scalar type.
data FTypeScalar
  = FTypeScalarInt     FTypeInt
  | FTypeScalarReal    FTypeReal
  | FTypeScalarComplex FTypeComplex
  | FTypeScalarLogical FTypeInt
  | FTypeScalarChar    FTypeChar
  | FTypeScalarCustom  String    -- ^ F77 structure, F90 DDT (non-intrinsic scalar)
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | Fortran INTEGER type.
data FTypeInt
  = FTypeInt1
  | FTypeInt2
  | FTypeInt4
  | FTypeInt8
    deriving stock    (Eq, Ord, Show, Data, Enum, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindInt :: Integer -> Maybe FTypeInt
parseKindInt = \case
  1 -> Just FTypeInt1
  2 -> Just FTypeInt2
  4 -> Just FTypeInt4
  8 -> Just FTypeInt8
  _ -> Nothing

prettyKindInt :: Integral a => FTypeInt -> a
prettyKindInt = \case FTypeInt1 -> 1
                      FTypeInt2 -> 2
                      FTypeInt4 -> 4
                      FTypeInt8 -> 8

fTypeIntMax :: FTypeInt -> Integer
fTypeIntMax = \case FTypeInt1 -> toInteger (maxBound @Int8)
                    FTypeInt2 -> toInteger (maxBound @Int16)
                    FTypeInt4 -> toInteger (maxBound @Int32)
                    FTypeInt8 -> toInteger (maxBound @Int64)

fTypeIntMin :: FTypeInt -> Integer
fTypeIntMin = \case FTypeInt1 -> toInteger (minBound @Int8)
                    FTypeInt2 -> toInteger (minBound @Int16)
                    FTypeInt4 -> toInteger (minBound @Int32)
                    FTypeInt8 -> toInteger (minBound @Int64)

-- | Fortran REAL type.
data FTypeReal
  = FTypeReal4
  | FTypeReal8
    deriving stock    (Eq, Ord, Show, Enum, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindReal :: Integer -> Maybe FTypeReal
parseKindReal = \case
  4 -> Just FTypeReal4
  8 -> Just FTypeReal8
  _ -> Nothing

-- | Fortran COMPLEX type (= 2 REALs).
data FTypeComplex
  = FTypeComplex8
  | FTypeComplex16
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

parseKindComplex :: Integer -> Maybe FTypeComplex
parseKindComplex = \case
  4 -> Just FTypeComplex8
  8 -> Just FTypeComplex16
  _ -> Nothing

-- | Fortran CHARACTER type.
data FTypeChar = FTypeChar CharLen
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | The length of a CHARACTER value.
--
-- IanH provides a great reference on StackOverflow:
-- https://stackoverflow.com/a/25051522/2246637
data CharLen
  = CharLen Integer
  -- ^ @CHARACTER(LEN=x)@ (where @x@ is a constant integer expression). Value
  --   has the given static length.

  | CharLenAssumed
  -- ^ @CHARACTER(LEN=*)@. F90. Value has assumed length. For a dummy argument,
  --   the length is assumed from the actual argument. For a PARAMETER named
  --   constant, the length is assumed from the length of the initializing
  --   expression.

  | CharLenDeferred
  -- ^ @CHARACTER(LEN=:)@. F2003. Value has deferred length. Must have the
  --   ALLOCATABLE or POINTER attribute.

    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

data FTypeArray = FTypeArray
  { fTypeArrayScalar :: FTypeScalar
  , fTypeArrayDims   :: [Dimension]
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

-- | (lower, upper) for indexing into the dimension
type Dimension = (Int, Int)

{-

-- | Recover the most appropriate 'TypeSpec' for the given 'Ty', depending on
--   the given 'FortranVersion'.
--
-- Kinds weren't formalized as a syntactic feature until Fortran 90, so we ask
-- for a context. If possible (>=F90), we prefer the more explicit
-- representation e.g. @REAL(8)@. For older versions, for specific type-kind
-- combinations, @DOUBLE PRECISION@ and @DOUBLE COMPLEX@ are used instead.
-- However, we otherwise don't shy away from adding kind info regardless of
-- theoretical version support.
--
-- This is a "best effort": 'TypeSpec' is a highly syntactic representation and
-- doesn't store array type information. Perhaps better would be to recover a
-- full 'StDeclaration'.
recoverTyTypeSpec
    :: forall a. a -> SrcSpan -> FortranVersion -> Ty -> TypeSpec a
recoverTyTypeSpec a ss v = \case
  TyArrayTy  aTy -> -- nowhere to put array info, so only recover scalar info
    recoverScalarTyTypeSpec (aTyScalar aTy)
  TyScalarTy sTy -> recoverScalarTyTypeSpec sTy
  where
    recoverScalarTyTypeSpec = \case
      ScalarTyIntrinsic iTy -> recoverIntrinsicTyTypeSpec iTy
      ScalarTyCustom ty' -> ts (TypeCustom ty') Nothing
    recoverIntrinsicTyTypeSpec (IntrinsicTy bTy k) =
        case bTy of
          BTyInteger -> buildTs (Just (k, 4)) TypeInteger Nothing
          BTyLogical -> buildTs (Just (k, 4)) TypeLogical Nothing
          BTyReal    ->
            if   v < Fortran90 && k == 8
            then buildTs Nothing TypeDoublePrecision Nothing
            else buildTs (Just (k, 4)) TypeReal Nothing
          BTyComplex ->
            if   v < Fortran90 && k == 16
            then buildTs Nothing TypeDoubleComplex Nothing
            else buildTs (Just (k, 8)) TypeComplex Nothing
          BTyCharacter len ->
            case len of
              CharLen len' ->
                if   len' == 1
                then buildTs (Just (k, 1)) TypeCharacter Nothing
                else buildTs (Just (k, 1)) TypeCharacter (Just (intValExpr len'))
              CharLenAssumed  -> buildTs (Just (k, 1)) TypeCharacter (Just valStarExpr)
              CharLenDeferred -> buildTs (Just (k, 1)) TypeCharacter (Just valColonExpr)
    -- don't get why I need the function signature down here... GHC infers it
    -- correctly (via @:: _@) but I get an Eq a0 complaint??? assuming scoped
    -- typevar weirdness...
    buildTs :: Maybe (Int, Int) -> BaseType -> Maybe (Expression a) -> TypeSpec a
    buildTs (Just (k, kDef)) ty mLen =
        if   k == kDef
        then buildTs Nothing ty mLen
        else ts ty (Just (sel mLen (Just (intValExpr k))))
    buildTs Nothing ty (Just len) = ts ty (Just (sel (Just len) Nothing))
    buildTs Nothing ty Nothing = ts ty Nothing
    intValExpr x = valExpr (ValInteger (show x))
    valStarExpr  = valExpr ValStar
    valColonExpr = valExpr ValStar
    valExpr = ExpValue a ss
    ts  = TypeSpec a ss
    sel = Selector a ss

-}

class JoinType t where
  -- | Calculate the least-upper bound type of two types, if it exists
  -- | (according to the Fortran spec)
  -- | Should be reflexive, symmetric, and transitive
  joinType :: t -> t -> Maybe t

instance JoinType FTypeInt where
  joinType x y | x == y = Just x
  -- Use the pretty printer to get the maximum size of integer, and convert back to the type repr
  joinType x y = parseKindInt (prettyKindInt x `max` prettyKindInt y)