{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Analysis.SemanticTypes where

import           Data.Data                      ( Data, Typeable )
import           Control.DeepSeq                ( NFData )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( BaseType(..)
                                                , Kind
                                                , Expression(..)
                                                , Value(..)
                                                , Selector(..) )
import           Language.Fortran.Repr
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )
import           Text.PrettyPrint               ( (<+>), parens )
import           Language.Fortran.PrettyPrint   ( Pretty(..) )

-- | Semantic type assigned to variables.
--
-- 'BaseType' stores the "type tag" given in syntax. 'SemType's add metadata
-- (kind and length), and resolve some "simple" types to a core type with a
-- preset kind (e.g. `DOUBLE PRECISION` -> `REAL(8)`).
--
-- Fortran 90 (and beyond) features may not be well supported.
data SemType
  = TInteger Kind
  | TReal Kind
  | TComplex Kind
  | TLogical Kind
  | TByte Kind
  | TCharacter CharacterLen Kind
  | TArray SemType (Maybe Dimensions) -- ^ Nothing denotes dynamic dimensions
  | TCustom String                    -- use for F77 structures, F90 DDTs
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary SemType
instance Out    SemType

-- TODO placeholder, not final or tested
-- should really attempt to print with kind info, and change to DOUBLE PRECISION
-- etc. for <F90. Maybe cheat, use 'recoverSemTypeTypeSpec' and print resulting
-- TypeSpec?
instance Pretty SemType where
  pprint' v = \case
    TInteger _ -> "integer"
    TReal _    -> "real"
    TComplex _ -> "complex"
    TLogical _ -> "logical"
    TByte _    -> "byte"
    TCharacter _ _ -> "character"
    TArray st _ -> pprint' v st <+> parens "(A)"
    TCustom str -> pprint' v (TypeCustom str)

data CharacterLen = CharLenStar    -- ^ specified with a *
                  | CharLenColon   -- ^ specified with a : (Fortran2003)
                    -- FIXME, possibly, with a more robust const-exp:
                  | CharLenExp     -- ^ specified with a non-trivial expression
                  | CharLenInt Int -- ^ specified with a constant integer
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary CharacterLen
instance Out    CharacterLen
instance NFData CharacterLen

charLenConcat :: CharacterLen -> CharacterLen -> CharacterLen
charLenConcat l1 l2 = case (l1, l2) of
  (CharLenExp    , _             ) -> CharLenExp
  (_             , CharLenExp    ) -> CharLenExp
  (CharLenStar   , _             ) -> CharLenStar
  (_             , CharLenStar   ) -> CharLenStar
  (CharLenColon  , _             ) -> CharLenColon
  (_             , CharLenColon  ) -> CharLenColon
  (CharLenInt i1 , CharLenInt i2 ) -> CharLenInt (i1 + i2)

charLenSelector :: Maybe (Selector a) -> (Maybe CharacterLen, Maybe String)
charLenSelector Nothing                          = (Nothing, Nothing)
charLenSelector (Just (Selector _ _ mlen mkind)) = (l, k)
  where
    l = charLenSelector' <$> mlen
    k | Just (ExpValue _ _ (ValInteger i)) <- mkind  = Just i
      | Just (ExpValue _ _ (ValVariable s)) <- mkind = Just s
      -- FIXME: some references refer to things like kind=kanji but I can't find any spec for it
      | otherwise                                    = Nothing

charLenSelector' :: Expression a -> CharacterLen
charLenSelector' = \case
  ExpValue _ _ ValStar        -> CharLenStar
  ExpValue _ _ ValColon       -> CharLenColon
  ExpValue _ _ (ValInteger i) -> CharLenInt (read i)
  _                           -> CharLenExp

-- | Attempt to recover the 'Value' that generated the given 'CharacterLen'.
charLenToValue :: CharacterLen -> Maybe (Value a)
charLenToValue = \case
  CharLenStar  -> Just ValStar
  CharLenColon -> Just ValColon
  CharLenInt i -> Just (ValInteger (show i))
  CharLenExp   -> Nothing

--------------------------------------------------------------------------------

-- | Given a 'BaseType' infer the "default" kind (or size of the
-- variable in memory).
--
-- Useful when you need a default kind, but gives you an unwrapped type.
-- Consider using Analysis.deriveSemTypeFromBaseType also.
--
-- Further documentation:
-- https://docs.oracle.com/cd/E19957-01/805-4939/c400041360f5/index.html
kindOfBaseType :: BaseType -> Int
kindOfBaseType = \case
  TypeInteger         -> 4
  TypeReal            -> 4
  TypeDoublePrecision -> 8
  TypeComplex         -> 8
  TypeDoubleComplex   -> 16
  TypeLogical         -> 4
  TypeCharacter{}     -> 1
  TypeByte            -> 1

  -- arbitrary values (>F77 is not tested/used)
  TypeCustom{}        -> 1
  ClassStar           -> 1
  ClassCustom{}       -> 1

getTypeKind :: SemType -> Kind
getTypeKind = \case
  TInteger   k -> k
  TReal      k -> k
  TComplex   k -> k
  TLogical   k -> k
  TByte      k -> k
  TCharacter _ k -> k
  TCustom    _ -> error "TCustom does not have a kind"
  TArray t _   -> getTypeKind t

setTypeKind :: SemType -> Kind -> SemType
setTypeKind st k = case st of
  TInteger   _ -> TInteger   k
  TReal      _ -> TReal      k
  TComplex   _ -> TComplex   k
  TLogical   _ -> TLogical   k
  TByte      _ -> TByte      k
  TCharacter charLen _ -> TCharacter charLen k
  TCustom    _ -> error "can't set kind of TCustom"
  TArray _ _   -> error "can't set kind of TArray"

tyToSemType :: Ty -> SemType
tyToSemType = \case
  TyScalarTy sTy -> scalarTyToSemType sTy
  TyArrayTy  aTy ->
    let sTy' = scalarTyToSemType (aTyScalar aTy)
     in TArray sTy' (Just (aTyDims aTy))

scalarTyToSemType :: ScalarTy -> SemType
scalarTyToSemType = \case
  ScalarTyCustom    ty' -> TCustom ty'
  ScalarTyIntrinsic iTy ->
    case iTyBase iTy of
      BTyInteger -> TInteger (iTyKind iTy)
      BTyReal    -> TReal    (iTyKind iTy)
      BTyComplex -> TComplex (iTyKind iTy)
      BTyLogical -> TLogical (iTyKind iTy)
      BTyCharacter len ->
        case len of
          CharLen len'    -> TCharacter (CharLenInt len') (iTyKind iTy)
          CharLenAssumed  -> TCharacter CharLenStar       (iTyKind iTy)
          CharLenDeferred -> TCharacter CharLenColon      (iTyKind iTy)
