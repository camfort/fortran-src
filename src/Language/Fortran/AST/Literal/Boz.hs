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

This type carries _some_ syntactic information that doesn't change meaning. The
expectation is that most users won't want to inspect 'Boz' values, usually just
convert them, so we do it for convenience for checking syntax conformance. Note
that not all info is retained -- which of single or double quotes were used is
not recorded, for example.
-}

module Language.Fortran.AST.Literal.Boz where

import           GHC.Generics
import           Data.Data
import           Control.DeepSeq                ( NFData )
import           Text.PrettyPrint.GenericPretty ( Out )
import           Data.Binary                    ( Binary )

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Numeric   as Num

import           Data.Bits

-- | A Fortran BOZ literal constant.
--
-- You should not create values of this type directly
--
-- The prefix defines the characters allowed in the string:
--
--   * @B@: @[01]@
--   * @O@: @[0-7]@
--   * @Z@: @[0-9 a-f A-F]@
data Boz = Boz
  { bozPrefix :: BozPrefix
  , bozString :: String

  , bozPrefixWasPostfix :: Conforming
  -- ^ Was the prefix actually postfix i.e. @'123'z@? This is non-standard
  --   syntax, disabled by default in gfortran. Syntactic info.
  } deriving stock    (Show, Generic, Data, Typeable, Ord)
    deriving anyclass (NFData, Out, Binary)

-- | Tests prefix & strings match, ignoring conforming/nonconforming flags.
instance Eq Boz where
    b1 == b2 =     bozPrefix b1 == bozPrefix b2
                && bozString b1 == bozString b2

data BozPrefix
  = BozPrefixB              -- ^ binary (bitstring)
  | BozPrefixO              -- ^ octal
  | BozPrefixZ Conforming   -- ^ hex, including nonstandard @x@
    deriving stock    (Show, Generic, Data, Typeable, Ord)
    deriving anyclass (NFData, Out, Binary)

-- | Ignores conforming/nonconforming flags.
instance Eq BozPrefix where
    p1 == p2 = case (p1, p2) of (BozPrefixB,   BozPrefixB)   -> True
                                (BozPrefixO,   BozPrefixO)   -> True
                                (BozPrefixZ{}, BozPrefixZ{}) -> True
                                _                            -> False

data Conforming = Conforming | Nonconforming
    deriving stock    (Eq, Show, Generic, Data, Typeable, Ord)
    deriving anyclass (NFData, Out, Binary)

-- | Parse a BOZ literal constant string, throwing a runtime error on parse
--   failure.
--
-- Looks for prefix or postfix. Strips the quotes from the string (single quotes
-- only).
--
-- This is written to be used internally in the lexer where it's already been
-- matched to a regex, so we don't re-parse. Be careful using it elsewhere.
unsafeParseBoz :: String -> Boz
unsafeParseBoz s =
    case List.uncons s of
      Nothing -> errInvalid
      Just (pc, ps) -> case parsePrefix pc of
                         Just p -> Boz p (shave ps) Conforming
                         Nothing -> case parsePrefix (List.last s) of
                                      Just p -> Boz p (shave (init s)) Nonconforming
                                      Nothing -> errInvalid
  where
    parsePrefix p
      | p' == 'b' = Just $ BozPrefixB
      | p' == 'o' = Just $ BozPrefixO
      | p' == 'z' = Just $ BozPrefixZ Conforming
      | p' == 'x' = Just $ BozPrefixZ Nonconforming
      | otherwise = Nothing
      where p' = Char.toLower p
    errInvalid = error "Language.Fortran.AST.BOZ.parseBoz: invalid BOZ string"
    -- | Remove the first and last elements in a list.
    shave = tail . init

-- | Pretty print a BOZ constant. Uses prefix style (ignores the postfix field),
--   and @z@ over nonstandard @x@ for hexadecimal.
prettyBoz :: Boz -> String
prettyBoz b = prettyBozPrefix (bozPrefix b) : '\'' : bozString b <> "'"
  where prettyBozPrefix = \case BozPrefixB   -> 'b'
                                BozPrefixO   -> 'o'
                                BozPrefixZ{} -> 'z'

-- | Resolve a BOZ constant as as integer.
--
-- Note that the type requested may alter the value returned due to overflow
-- behaviour.
--
-- >>> bozAsInteger @Natural (parseBoz "z'80'")
-- 128
-- >>> bozAsInteger @Int8    (parseBoz "z'80'")
-- -128
-- >>> bozAsInteger @Int16   (parseBoz "z'80'")
-- 128
--
-- For correct negative handling, we rely on Haskell's signed integer types
-- using two's complement and wrapping operations in the regular manner, which
-- should - though I haven't checked - match your average Fortran compiler.
bozAsInteger :: (Num a, Eq a) => Boz -> a
bozAsInteger (Boz pfx str _) = runReadS $ parser str
  where
    runReadS = fst . head
    parser = case pfx of BozPrefixB   -> Num.readInt 2 (const True) binDigitVal
                         -- (on GHC >=9.2, 'Num.readBin')
                         BozPrefixO   -> Num.readOct
                         BozPrefixZ{} -> Num.readHex
    binDigitVal = \case '0' -> 0
                        '1' -> 1
                        _   -> error "Language.Fortran.AST.BOZ.bozAsNatural: invalid BOZ string"
