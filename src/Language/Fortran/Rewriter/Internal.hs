{-# LANGUAGE OverloadedStrings #-}

-- Original code from Bloomberg, used with permission.
--
-- Original authors:
--   * Daniel Beer
--   * Anthony Burzillo
--   * Raoul Hidalgo Charman
--   * Aiden Jeffrey
--   * Jason Xu
--   * Beleth Apophis
--   * Lukasz Kolodziejczyk

module Language.Fortran.Rewriter.Internal where

import           Data.Int
import           Data.Bifunctor                 ( first )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BC
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Data.List                      ( sort
                                                , find
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromMaybe
                                                , fromJust
                                                , maybeToList
                                                )
import qualified Data.Map                      as M
import           Data.Typeable                  ( Typeable )

-- | Represents location in source code.
--
-- Note that, 'SourceLocation' indicates space between characters,
-- i.e the following example:
--
-- @ SourceLocation 0 1 @
--
-- indicates position between first and second characters in a file.
data SourceLocation = SourceLocation Int Int deriving (Show, Eq)

-- | Represents range in source code.
data SourceRange = SourceRange SourceLocation SourceLocation deriving (Eq)
instance Show SourceRange where
  show (SourceRange (SourceLocation l1 c1) (SourceLocation l2 c2)) =
         "("
      ++ show (l1 + 1) ++ ":" ++ show (c1 + 1)
      ++ ")-("
      ++ show (l2 + 1) ++ ":" ++ show (c2 + 1)
      ++ ")"

-- | Represents a character in the original source text along with
-- any replacement operations applied to the character in place.
--
-- It expects a character (in case it's empty, Nothing should be used),
-- whether it should be removed, its 'SourceLocation' and a string that
-- should be put in place of it.
data RChar = RChar (Maybe Char) Bool SourceLocation ByteString deriving (Show, Eq)

-- | Represents the intent to replace content in the file.
--
-- The content in 'Replacement' will be used in place of what is in
-- the range described. Note that the replacement text can be shorter
-- or larger than the original span, and it can also be multi-line.
data Replacement = Replacement SourceRange String deriving (Show, Eq)
instance Ord Replacement where
  (Replacement (SourceRange a _) _) <= (Replacement (SourceRange b _) _) =
    a < b

-- | Exception raised when two 'Replacement' objects overlap
-- ('OverlappingError') or 'Replacement' points at invalid locations
-- ('InvalidRangeError').
data ReplacementError
    = OverlappingError [(Replacement, Replacement)]
    | InvalidRangeError
    deriving (Show, Typeable, Eq)

-- | As we advance through the ['RChar'] list, we consider "chunks"
-- as the unit of text written out. A chunk is either:
--
--     1. original source text up to a newline character, end of file
--        or 'RChar' described in 2.
--     2. a single 'RChar' that has non-empty replacement string
--        or is deleted.
type Chunk = [RChar]

-- | Represents map of files and replacements that will be done.
type ReplacementMap = M.Map String [Replacement]

instance Exception ReplacementError
instance Ord SourceLocation where
  (SourceLocation l1 c1) <= (SourceLocation l2 c2) =
    l1 < l2 || l1 == l2 && c1 <= c2

-- | Parses input string into a list of annotated characters.
toRCharList :: ByteString -> [RChar]
toRCharList = reverse . uncurry appendLast . BC.foldl'
  go
  (SourceLocation 0 0, [])
 where
  go :: (SourceLocation, [RChar]) -> Char -> (SourceLocation, [RChar])
  go (loc@(SourceLocation line col), rcs) c =
    let newLoc = if c /= '\n'
          then SourceLocation line (col + 1)
          else SourceLocation (line + 1) 0
    in  (newLoc, RChar (Just c) False loc "" : rcs)
  appendLast loc = (RChar Nothing False loc "" :)

-- | Marks 'RChars' in a given range to be removed later.
markRChars :: [RChar] -> SourceRange -> [RChar]
markRChars rchars sr = markRChars_ rchars sr (SourceLocation 0 0)

markRChars_ :: [RChar] -> SourceRange -> SourceLocation -> [RChar]
markRChars_ [] _ _ = []
markRChars_ (RChar x odel _ orepl : xs) sr@(SourceRange (SourceLocation sl sc) (SourceLocation el ec)) (SourceLocation l c) =
    rch : rchs
  where
    rch =
        if    l == sl && l == el && c >= sc && c <  ec
           || l == sl && l <  el && c >= sc
           || l == el && l >  sl && c <  ec
           || l >  sl && l <  el
        then RChar x True (SourceLocation l c) ""
        else RChar x odel (SourceLocation l c) orepl
    rchs =
        if x /= Just '\n'
        then markRChars_ xs sr (SourceLocation l (c + 1))
        else markRChars_ xs sr (SourceLocation (l + 1) 0)

-- | Sets replacement string to be prepended to the given location.
setReplacementStringSL
  :: [RChar] -> SourceLocation -> ByteString -> Bool -> [RChar]
setReplacementStringSL [] _ _ _ = []
setReplacementStringSL (RChar och odel osl@(SourceLocation ol oc) orepl : xs) sl@(SourceLocation l c) repl isInsert
  = if l == ol && c == oc
    then if isInsert
      then
        RChar och
              odel
              osl
              -- (repl <> if isNothing och then "" else [fromJust och])
              (repl <> maybe "" BC.singleton och)
          : xs
      else RChar och odel osl repl : xs
    else RChar och odel osl orepl : setReplacementStringSL xs sl repl isInsert

-- | Sets replacement string to be prepended to the begining of the
-- given range.
setReplacementStringSR
  :: [RChar] -> SourceRange -> ByteString -> Bool -> [RChar]
setReplacementStringSR rchars (SourceRange sls _) =
  setReplacementStringSL rchars sls

-- | Applies all deletions and additions and transforms 'RChars' back
-- to a string.
evaluateRChars :: [RChar] -> ByteString
evaluateRChars = BC.concat . map evaluateRChar

-- | If 'RChar' is marked as deleted, it'll be evaluated to its
-- replacement string, otherwise original character will be returned.
evaluateRChar :: RChar -> ByteString
evaluateRChar (RChar char del _ repl) | del = repl
                                      | isNothing char = ""
                                      | otherwise = BC.singleton $ fromJust char

-- | From ['RChar'], obtain a ('Chunk', ['RChars']) where the 'Chunk'
-- is the next 'Chunk' and the ['RChar'] are the remaining 'RChar's.
nextChunk :: [RChar] -> (Chunk, [RChar])
nextChunk [] = ([], [])
-- if the current chunk is the start of inline comment, prepend it to next
nextChunk (rchar@(RChar (Just '!') True _ _) : xs) = Data.Bifunctor.first
  (rchar :)
  rec
  where rec = nextChunk xs
nextChunk (rchar@(RChar _ True _ _) : xs) = ([rchar], xs)
nextChunk rchars                          = nextChunk_ rchars

nextChunk_ :: [RChar] -> (Chunk, [RChar])
nextChunk_ [] = ([], [])
nextChunk_ ls@(RChar _ True _ _ : _) = ([], ls)
nextChunk_ (rchar@(RChar (Just '\n') _ _ _) : xs) = ([rchar], xs)
nextChunk_ (rchar : xs) = Data.Bifunctor.first (rchar :) rec
  where rec = nextChunk_ xs

-- | Splits ['RChar'] into 'Chunk's.
allChunks :: [RChar] -> [Chunk]
allChunks []     = []
allChunks rchars = chunk : allChunks rest
  where (chunk, rest) = nextChunk rchars

-- | Transform a list of 'Chunk's into a single string, applying
-- continuation lines when neccessary.
evaluateChunks :: [Chunk] -> ByteString
evaluateChunks ls = evaluateChunks_ ls 0 Nothing

-- | This expands the chunks from the left to right. If the length
-- of what has already been put into the current line exceeds the
-- limit of 72 characters (excluding inline comments starting with
-- '!' and implicit comments starting at column 73) then it ends
-- the current line with a continuation, otherwise it simply adds
-- the line as-is. It also calculates if the chunk is inside or outside
-- of a string literal, using that to determine where explicit comments are
-- if any.
--
-- In either case, we make sure that we are padding implicit
-- comments *from the original source* even if the tail of that
-- line has been moved onto a continuation line.
evaluateChunks_ :: [Chunk] -> Int64 -> Maybe Char -> ByteString
evaluateChunks_ [] _ _ = ""
evaluateChunks_ (x : xs) currLen quotation =
  let chStr   = evaluateRChars x
      isQuote = (`elem` ['\'', '"'])
      elemIndexOutsideStringLiteral currentState needle haystack = impl
        currentState
        needle
        haystack
        0
         where
          -- Search space is empty, therefore no result is possible
          impl state _ "" _ = (state, Nothing)
          -- We have already entered a string literal
          impl state@(Just quoteChar) query (top : rest) idx
            | top == quoteChar = impl Nothing query rest (idx + 1)
            | otherwise        = impl state query rest (idx + 1)
          -- Searching outside a string literal, might find the query or
          -- enter a string literal
          impl Nothing query (top : rest) idx
            | top == query = (Nothing, Just idx)
            | isQuote top  = impl (Just top) query rest (idx + 1)
            | otherwise    = impl Nothing query rest (idx + 1)

      -- length to the last line
      lastLen = BC.elemIndex '\n' $ BC.reverse chStr
      (nextState, explicitCommentIdx) =
          elemIndexOutsideStringLiteral quotation '!' (BC.unpack chStr)
      -- length of rest of the line ignoring explicit comments
      nextLen = fromMaybe
        (BC.length chStr)
        -- \n cannot occur inside of string literals so it is okay to search
        -- directly for it. '!' on the other hand is allowed inside strings
        -- so it needs to be searched for outside string literals
        (myMin (BC.elemIndex '\n' chStr) explicitCommentIdx)
      overLength = currLen + nextLen > 72 && currLen > 0
  in  if overLength
   -- start a new line
        then
          let targetCol = 72 - 6
          in  "\n     +"
              <> evaluateRChars (padImplicitComments x targetCol)
              <> maybe (evaluateChunks_ xs (6 + nextLen) nextState)
                       (\len -> evaluateChunks_ xs len nextState)
                       lastLen
   -- continue with the current line
        else
          let targetCol = 72 - fromIntegral currLen
          in  evaluateRChars (padImplicitComments x targetCol)
                <> maybe (evaluateChunks_ xs (currLen + nextLen) nextState)
                         (\len -> evaluateChunks_ xs len nextState)
                         lastLen
 where
  -- min for maybes that doesn't short circuit if there's a Nothing
  myMin Nothing  m        = m
  myMin m        Nothing  = m
  myMin (Just a) (Just b) = Just $ min a b
  -- Text after line 72 is an implicit comment, so should stay there regardless
  -- of what happens to the rest of the source
  padImplicitComments :: Chunk -> Int -> Chunk
  padImplicitComments chunk targetCol
    | isMarkedForRemoval chunk = chunk
    | otherwise =
      let zippedChunk = zip [0 ..] chunk
      in  case findCommentRChar zippedChunk of
            Just (index, rc) ->
              case
                  findExclamationRChar zippedChunk
                    >>= \(id2, _) -> return (id2 >= index)
                of
                  Just False -> chunk -- in this case there's a "!" before column 73
                  _ ->
                    take index chunk
                      ++ padCommentRChar rc (targetCol - index)
                      :  drop (index + 1) chunk
            Nothing -> chunk
   where
    -- Find the first location of a '!' in the chunks
    findExclamationRChar = find ((\(RChar c _ _ _) -> c == Just '!') . snd)
    -- Find the location at column 73 in the original source.
    -- If that character is a newline, ignore it
    findCommentRChar     = find
      ( (\(RChar ch _ (SourceLocation _ cl) _) -> cl == 72 && ch /= Just '\n')
      . snd
      )
    padCommentRChar :: RChar -> Int -> RChar
    padCommentRChar (RChar char _ loc repl) padding = RChar
      char
      True
      loc
      (BC.pack (replicate padding ' ' ++ maybeToList char) `BC.append` repl)


isMarkedForRemoval (RChar _ True _ _ : _) = True
isMarkedForRemoval (_ : rs) = isMarkedForRemoval rs
isMarkedForRemoval [] = False

-- | Return TRUE iff the 'Replacement' constitutes a character
-- insertion.
isInsertion :: Replacement -> Bool
isInsertion (Replacement (SourceRange (SourceLocation sl sc) (SourceLocation el ec)) repl)
  = sl == el && sc == ec && repl /= ""

insertionSR :: SourceRange -> SourceRange
insertionSR (SourceRange (SourceLocation sl sc) _) =
  SourceRange (SourceLocation sl sc) (SourceLocation sl (sc + 1))

-- | Sets a single 'Replacement' given a list of 'RChar's.
setReplacement :: [RChar] -> Replacement -> [RChar]
setReplacement rchars repl@(Replacement sr replS) =
  let replBS = BC.pack replS
  in  if isInsertion repl
        then setReplacementStringSR (markRChars rchars (insertionSR sr))
                                    (insertionSR sr)
                                    replBS
                                    True
        else setReplacementStringSR (markRChars rchars sr) sr replBS False

-- | Sets a list of 'Replacement's given a list of 'RChar's.
setReplacements :: [RChar] -> [Replacement] -> [RChar]
setReplacements rchars repls =
  let rchar' = foldl setReplacement rchars repls in adjustLineWrap rchar'


-- | heuristic to wrap line after comma or right parenthesis if applicable
adjustLineWrap :: [RChar] -> [RChar]
adjustLineWrap []  = []
adjustLineWrap [x] = [x]
adjustLineWrap (rc@(RChar _ True _ _) : rs@(RChar (Just c) False _ _ : _))
  | c `elem` [',', ')'] = adjustLineWrapAux rc [] rs
adjustLineWrap (x : xs) = x : adjustLineWrap xs


adjustLineWrapAux :: RChar -> [RChar] -> [RChar] -> [RChar]
adjustLineWrapAux rc1 deleted (rc2@(RChar (Just c) False _ _) : rs)
  | c `elem` [',', ')'] = adjustLineWrapAux (appendRC rc1 c)
                                            (deleteRC rc2 : deleted)
                                            rs
adjustLineWrapAux rc1 deleted rs = (rc1 : reverse deleted) <> adjustLineWrap rs


-- | Mark removal for the input 'RChar'
deleteRC :: RChar -> RChar
deleteRC (RChar _ _ loc s) = RChar Nothing True loc s


-- | Append the input character to the replacement string
appendRC :: RChar -> Char -> RChar
appendRC (RChar mc _ loc s) c = RChar mc True loc (s `BC.snoc` c)


-- | Checks whether two 'Replacement's are not overlapping.
areDisjoint :: Replacement -> Replacement -> Bool
areDisjoint (Replacement (SourceRange (SourceLocation r1sl r1sc) (SourceLocation r1el r1ec)) _) (Replacement (SourceRange (SourceLocation r2sl r2sc) (SourceLocation r2el r2ec)) _)
  | r2sl >  r1el || r1sl >  r2el = True
  | r1el == r2sl && r1ec <= r2sc = True
  | r1sl == r2el && r1sc >= r2ec = True
  | otherwise                    = False

-- | Checks whether:
--
--     1. the start is before the end of the range and
--     2. both start and end locations are within the code.
isValidRange :: SourceRange -> [RChar] -> Bool
isValidRange (SourceRange sl1 sl2) rchars =
  sl1 <= sl2 && isValidLocation sl1 rchars && isValidLocation sl2 rchars

isValidLocation :: SourceLocation -> [RChar] -> Bool
isValidLocation _  []                     = False
isValidLocation sl (RChar _ _ csl _ : xs) = sl == csl || isValidLocation sl xs

checkRanges :: [RChar] -> [Replacement] -> [RChar]
checkRanges rchars repls = if and validList
  then rchars
  else throw InvalidRangeError
  where validList = [ isValidRange sr rchars | (Replacement sr _) <- repls ]

checkOverlapping :: [Replacement] -> [Replacement]
checkOverlapping repls = if null overlappingPairs
  then repls
  else throw $ OverlappingError overlappingPairs
 where
  overlappingPairs = findOverlappingPairs (sort repls)

  findOverlappingPairs :: [Replacement] -> [(Replacement, Replacement)]
  findOverlappingPairs [] = []
  findOverlappingPairs repls' =
    let currentRepl = head repls'
        overlapping = takeWhile (not . areDisjoint currentRepl) (tail repls')
        nextResult  = findOverlappingPairs (tail repls')
    in  [ (currentRepl, x) | x <- overlapping ] <> nextResult

-- | Applies 'Replacement's to a string and return it.
--
-- Firstly, it transforms the string into a list of 'RChar's.
--
-- After that, it validates the 'SourceRange' of each 'Replacement'.
--
-- In the end, it splits up 'RChar's in 'Chunk's, set the
-- 'Replacement's and evaluates the 'Chunk's.
applyReplacements :: ByteString -> [Replacement] -> ByteString
applyReplacements str repls = applyReplacements_ (checkRanges rchars repls)
                                                 (checkOverlapping repls)
  where rchars = toRCharList str

applyReplacements_ :: [RChar] -> [Replacement] -> ByteString
applyReplacements_ rchars repls = evaluateChunks chunks
 where
  replRchars = setReplacements rchars repls
  chunks     = allChunks replRchars
