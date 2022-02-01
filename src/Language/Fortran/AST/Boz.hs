{- | Supporting code for handling Fortran BOZ literal constants.

Using the definition from the latest Fortran standards (F2003, F2008), BOZ
constants are bitstrings (untyped!) which have basically no implicit rules. How
they're interpreted depends on context (they are generally limited to DATA
statements and a small handful of intrinsic functions).

Note that currently, we don't store BOZ constants as bitstrings. Storing them in
their string representation is easy and in that form, they're easy to safely
resolve to an integer. An alternate option would be to store them as the
bitstring "B" of BOZ, and only implement functions on that. For simple uses
(integer), I'm doubtful that would provide extra utility or performance, but it
may be more sensible in the future. For now, you may retrieve a bitstring by
converting to a numeric type and using something like 'showIntAtBase', or a
'Bits' instance.
-}

module Language.Fortran.AST.Boz where

import           GHC.Generics
import           Data.Data
import           Control.DeepSeq                ( NFData )
import           Text.PrettyPrint.GenericPretty ( Out )

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Numeric   as Num

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
  } deriving stock    (Eq, Show, Generic, Data, Typeable, Ord)
    deriving anyclass (NFData, Out)

data BozPrefix
  = BozPrefixB  -- ^ binary (bitstring)
  | BozPrefixO  -- ^ octal
  | BozPrefixZ  -- ^ hex (also with prefix @x@)
    deriving stock    (Eq, Show, Generic, Data, Typeable, Ord)
    deriving anyclass (NFData, Out)

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
  where prettyBozPrefix = \case BozPrefixB -> 'b'
                                BozPrefixO -> 'o'
                                BozPrefixZ -> 'z'

-- | Resolve a BOZ constant as a natural (positive integer).
--
-- Is actually polymorphic over the output type, but you probably want to
-- resolve to 'Integer' or 'Natural' usually.
--
-- We assume the 'Boz' is well-formed, thus don't bother with digit predicates.
bozAsNatural :: (Num a, Eq a) => Boz -> a
bozAsNatural (Boz pfx str) = runReadS $ parser str
  where
    runReadS = fst . head
    parser = case pfx of BozPrefixB -> Num.readInt 2 (const True) binDigitVal
                         -- (on GHC >=9.2, 'Num.readBin')
                         BozPrefixO -> Num.readOct
                         BozPrefixZ -> Num.readHex
    binDigitVal = \case '0' -> 0
                        '1' -> 1
                        _   -> error "Language.Fortran.AST.BOZ.bozAsNatural: invalid BOZ string"
