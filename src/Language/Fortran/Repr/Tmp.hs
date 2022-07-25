module Language.Fortran.Repr.Tmp where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Data.Text as Text
import Data.Text ( Text )
import qualified Data.Char as Char
import qualified Data.List as List

testF :: Float -> Text
testF = prettyHexByteString B.unpack . B.toLazyByteString . B.floatBE

testD :: Double -> Text
testD = prettyHexByteString B.unpack . B.toLazyByteString . B.doubleBE


-- | Pretty print to default format @00 12 AB FF@: space between each byte, all
--   caps.
--
-- This format I consider most human readable. I prefer caps to draw attention
-- to this being data instead of text (you don't see that many capital letters
-- packed together in prose).
prettyHexByteString :: (a -> [Word8]) -> a -> Text
prettyHexByteString unpack =
      Text.concat
    . List.intersperse (Text.singleton ' ')
    . fmap (f . prettyHexByte Char.toUpper)
    . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByte :: (Char -> Char) -> Word8 -> (Char, Char)
prettyHexByte f w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = f . Char.intToDigit -- Char.intToDigit returns lower case

-- | Pretty print to "compact" format @0012abff@ (often output by hashers).
prettyHexByteStringCompact :: (a -> [Word8]) -> a -> Text
prettyHexByteStringCompact unpack =
    Text.concat . fmap (f . prettyHexByte id) . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2
