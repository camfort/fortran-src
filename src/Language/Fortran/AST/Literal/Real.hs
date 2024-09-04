{- |
Supporting code for handling Fortran REAL literals.

Fortran REAL literals have some idiosyncrasies that prevent them from lining up
with Haskell's reals immediately. So, we parse into an intermediate data type
that can be easily exported with full precision later. Things we do:

  * Strip explicit positive signs so that signed values either begin with the
    minus sign @-@ or no sign. ('Read' doesn't allow explicit positive signs.)
  * Make exponent explicit by adding the default exponent @E0@ if not present.
  * Make implicit zeroes explicit. @.123 -> 0.123@, @123. -> 123.0@. (Again,
    Haskell literals do not support this.)

For example, the Fortran REAL literal @1D0@ will be parsed into @1.0D0@.
-}

{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.AST.Literal.Real where

import qualified Data.Char as Char
import           GHC.Generics
import           Data.Data
import           Control.DeepSeq                ( NFData )
import           Text.PrettyPrint.GenericPretty ( Out )

-- | A Fortran real literal. (Does not include the optional kind parameter.)
--
-- A real literal is formed of a signed rational significand, and an 'Exponent'.
--
-- See F90 ISO spec pg.27 / R412-416.
--
-- Note that we support signed real literals, even though the F90 spec indicates
-- non-signed real literals are the "default" (signed are only used in a "spare"
-- rule). Our parsers should parse explicit signs as unary operators. There's no
-- harm in supporting signed literals though, especially since the exponent *is*
-- signed.
data RealLit = RealLit
  { realLitSignificand :: String
  -- ^ A string representing a signed decimal.
  -- ^ Approximate regex: @-? ( [0-9]+ \. [0-9]* | \. [0-9]+ )@
  , realLitExponent    :: Exponent
  } deriving stock (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (NFData, Out)

-- | An exponent is an exponent letter (E, D) and a signed integer.
data Exponent = Exponent
  { exponentLetter :: ExponentLetter
  , exponentNum    :: String
  } deriving stock (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (NFData, Out)

-- Note: Some Fortran language references include extensions here. HP's F90
-- reference provides a Q exponent letter which sets kind to 16.
data ExponentLetter
  = ExpLetterE -- ^ KIND=4 (float)
  | ExpLetterD -- ^ KIND=8 (double)
  | ExpLetterQ -- ^ KIND=16 ("quad", rare? extension)
    deriving stock (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (NFData, Out)

-- | Prettify a 'RealLit' in a Haskell-compatible way.
prettyHsRealLit :: RealLit -> String
prettyHsRealLit r = realLitSignificand r <> "e" <> exponentNum (realLitExponent r)

readRealLit :: (Fractional a, Read a) => RealLit -> a
readRealLit = read . prettyHsRealLit

-- UNSAFE. Expects a valid Fortran REAL literal.
parseRealLit :: String -> RealLit
parseRealLit r =
    let (significandStr, exponentStr) = span isSignificand r
        realLitExponent = parseExponent exponentStr
        realLitSignificand = normalizeSignificand (stripPositiveSign significandStr)
     in RealLit{..}
  where
    -- | Ensure that the given decimal string is in form @x.y@.
    normalizeSignificand str = case span (/= '.') str of
                                 ([], d)  -> '0':d   --    .456
                                 (i, ".") -> i<>".0" -- 123.
                                 (i, "")  -> i<>".0" -- 123
                                 _        -> str     -- 123.456
    parseExponent "" = Exponent { exponentLetter = ExpLetterE, exponentNum = "0" }
    parseExponent (l:str) =
        let exponentLetter = parseExponentLetter l
            exponentNum = stripPositiveSign str
         in Exponent{..}
    stripPositiveSign = \case
      []  -> []
      c:s -> case c of
               '+' ->   s
               _   -> c:s
    isSignificand ch | Char.isDigit ch                 = True
                     | ch `elem` ['.', '-', '+']  = True
                     | otherwise                  = False
    parseExponentLetter ch = case Char.toLower ch of
                               'e' -> ExpLetterE
                               'd' -> ExpLetterD
                               'q' -> ExpLetterQ
                               _   -> error $ "Language.Fortran.AST.Literal.Real.parseRealLit: invalid exponent letter: " <> [ch]
