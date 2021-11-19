{- | Supporting code for handling Fortran BOZ literal constants.

Using the definition from the latest Fortran standards (F2003, F2008), BOZ
constants are bitstrings (untyped!) which have basically no implicit rules. How
they're interpreted depends on context (they are generally limited to DATA
statements and a small handful of intrinsic functions).
-}

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fortran.AST.Boz where

import           GHC.Generics
import           Data.Data
import           Control.DeepSeq                ( NFData )
import           Text.PrettyPrint.GenericPretty ( Out )

import qualified Data.List as List
import qualified Data.Char as Char

-- | A Fortran BOZ literal constant.
--
-- The prefix defines the characters allowed in the string:
--
--   * @B@: @[01]@
--   * @O@: @[0-7]@
--   * @Z@: @[0-9 a-f A-F]@
data Boz = Boz
  { bozPrefix :: BozPrefix
  , bozString :: String
  } deriving (Eq, Show, Data, Typeable, Generic, NFData, Out, Ord)

data BozPrefix
  = BozPrefixB
  | BozPrefixO
  | BozPrefixZ -- also @x@
    deriving (Eq, Show, Data, Typeable, Generic, NFData, Out, Ord)

-- | UNSAFE. Parses a BOZ literal constant string.
--
-- Looks for prefix or suffix. Strips the quotes from the string (single quotes
-- only).
parseBoz :: String -> Boz
parseBoz s =
    case List.uncons s of
      Nothing -> errInvalid
      Just (pc, ps) -> case parsePrefix pc of
                         Just p -> Boz p (shave ps)
                         Nothing -> case parsePrefix (List.last s) of
                                      Just p -> Boz p (shave (init s))
                                      Nothing -> errInvalid
  where
    parsePrefix p
      | p' == 'b'            = Just BozPrefixB
      | p' == 'o'            = Just BozPrefixO
      | p' `elem` ['z', 'x'] = Just BozPrefixZ
      | otherwise            = Nothing
      where p' = Char.toLower p
    errInvalid = error "Language.Fortran.AST.BOZ.parseBoz: invalid BOZ string"
    -- | Remove the first and last elements in a list.
    shave = tail . init

-- | Pretty print a BOZ constant. Uses prefix style, and @z@ over nonstandard
--   @x@ for hexadecimal.
prettyBoz :: Boz -> String
prettyBoz b = prettyBozPrefix (bozPrefix b) : '\'' : bozString b <> "'"
  where prettyBozPrefix = \case
          BozPrefixB -> 'b'
          BozPrefixO -> 'o'
          BozPrefixZ -> 'z'
