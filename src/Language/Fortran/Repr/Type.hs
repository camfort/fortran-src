{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

{-
TODO ignoring character kind for now
TODO support kind *16 (e.g. INT*16 -> Int16)
TODO no way to talk about "intrinsic type with any rep" (e.g. ignore INT kind,
     CHAR len). annoying for intrinsics etc.
-}

module Language.Fortran.Repr.Type where

import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array

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
data FType = FType
  { fTypeScalar :: FTypeScalar
  , fTypeArray  :: Maybe ArrayShape -- TODO likely replace with "custom" Maybe
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

prettyType :: FType -> String
prettyType (FType sty mashp) =
    case mashp of
      Nothing    -> prettyScalarType sty
      Just _ashp -> undefined

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
  joinType x y = Just $ max x y
