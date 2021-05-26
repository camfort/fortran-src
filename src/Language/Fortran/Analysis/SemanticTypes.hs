{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Analysis.SemanticTypes where

import           Data.Data                      ( Data, Typeable )
import           Control.DeepSeq                ( NFData )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( Kind, Expression(..), Value(..), Selector(..) )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

-- TODO:
--   * cleaner arrays: separate into scalar & array types?
--   * how is F77 structure, F90 DDT support really? (F90 likely untested)
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

-- | The declared dimensions of a staticically typed array variable
-- type is of the form [(dim1_lower, dim1_upper), (dim2_lower, dim2_upper)]
type Dimensions = [(Int, Int)]

--------------------------------------------------------------------------------

data CharacterLen = CharLenStar    -- ^ specified with a *
                  | CharLenColon   -- ^ specified with a : (Fortran2003)
                    -- FIXME, possibly, with a more robust const-exp:
                  | CharLenExp     -- ^ specified with a non-trivial expression
                  | CharLenInt Int -- ^ specified with a constant integer
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary CharacterLen
instance Out    CharacterLen
instance NFData CharacterLen

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

charLenConcat :: CharacterLen -> CharacterLen -> CharacterLen
charLenConcat l1 l2 = case (l1, l2) of
  (CharLenExp    , _             ) -> CharLenExp
  (_             , CharLenExp    ) -> CharLenExp
  (CharLenStar   , _             ) -> CharLenStar
  (_             , CharLenStar   ) -> CharLenStar
  (CharLenColon  , _             ) -> CharLenColon
  (_             , CharLenColon  ) -> CharLenColon
  (CharLenInt i1 , CharLenInt i2 ) -> CharLenInt (i1 + i2)

