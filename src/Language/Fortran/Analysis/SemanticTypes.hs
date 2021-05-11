-- | TODO: copied from fortran-vars (then edited)

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Fortran.Analysis.SemanticTypes where

import           Data.Data                      ( Data, Typeable )
import           Control.DeepSeq                ( NFData )
import           GHC.Generics                   ( Generic )
import           Language.Fortran.AST           ( Kind, Expression(..), Value(..), Selector(..) )
import           Data.Binary                    ( Binary )
import           Text.PrettyPrint.GenericPretty ( Out(..) )

-- | Term type representation.
data SemType
  = STyInteger Kind
  | STyReal Kind
  | STyComplex Kind
  | STyLogical Kind
  | STyByte Kind
  | STyCharacter CharacterLen
  | STyArray SemType (Maybe Dimensions) -- ^ Nothing denotes dynamic dimensions
  | STyCustom String          -- use for F77 structures, F90 DDTs
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Binary SemType
instance Out    SemType

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
    l | Just (ExpValue _ _ ValStar) <- mlen        = Just CharLenStar
      | Just (ExpValue _ _ ValColon) <- mlen       = Just CharLenColon
      | Just (ExpValue _ _ (ValInteger i)) <- mlen = Just $ CharLenInt (read i)
      | Nothing <- mlen                            = Nothing
      | otherwise                                  = Just CharLenExp
    k | Just (ExpValue _ _ (ValInteger i)) <- mkind  = Just i
      | Just (ExpValue _ _ (ValVariable s)) <- mkind = Just s
      -- FIXME: some references refer to things like kind=kanji but I can't find any spec for it
      | otherwise                                    = Nothing
