module Language.Fortran.Transformation.Grouping ( groupForall
                                                , groupIf
                                                , groupDo
                                                , groupLabeledDo
                                                , groupCase
                                                ) where

import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.Analysis
import Language.Fortran.Transformation.TransformMonad

import Data.Data
import Data.Generics.Uniplate.Operations

type ABlocks a = [ Block (Analysis a) ]

genericGroup :: Data a => (ABlocks a -> ABlocks a) -> Transform a ()
genericGroup groupingFunction =
    modifyProgramFile $ transformBi groupingFunction

--------------------------------------------------------------------------------
-- Grouping FORALL statement blocks into FORALL blocks in entire parse tree
--------------------------------------------------------------------------------
groupForall :: Data a => Transform a ()
groupForall = genericGroup groupForall'


groupForall' :: ABlocks a -> ABlocks a
groupForall' [] = []
groupForall' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        | StForall _ _ mTarget header <- st ->
          let ( blocks, leftOverBlocks, endLabel ) =
               collectNonForallBlocks groupedBlocks mTarget
          in ( BlForall a (getTransSpan s blocks) label mTarget header blocks endLabel
             , leftOverBlocks)
        | StForallStatement _ _ header st' <- st ->
          let block = BlStatement a (getSpan st') Nothing st' in
          ( BlForall a (getTransSpan s st') label Nothing header [block] Nothing, groupedBlocks )
      b'' | containsGroups b'' ->
        ( applyGroupingToSubblocks groupForall' b'', groupedBlocks )
      _ -> (b, groupedBlocks)
    groupedBlocks = groupForall' bs

collectNonForallBlocks :: ABlocks a -> Maybe String
                          -> ( ABlocks a
                             , ABlocks a
                             , Maybe (Expression (Analysis a)) )
collectNonForallBlocks blocks mNameTarget =
  case blocks of
    BlStatement _ _ mLabel (StEndForall _ _ mName):rest
      | mName == mNameTarget -> ([], rest, mLabel)
      | otherwise ->
        error "Forall block name does not match that of the end statement."
    b:bs ->
      let (bs', rest, mLabel) = collectNonForallBlocks bs mNameTarget
      in (b : bs', rest, mLabel)
    _ -> error "Premature file ending while parsing structured forall block."


--------------------------------------------------------------------------------
-- Grouping if statement blocks into if blocks in entire parse tree
--------------------------------------------------------------------------------

groupIf :: Data a => Transform a ()
groupIf = genericGroup groupIf'

-- Actual grouping is done here.
-- 1. Case: head is a statement block with an IF statement:
-- 1.1  Group everything to the right of the statement.
-- 1.2  Prepend the head
-- 1.3  Decompose into if components (blocks and condition pairs).
-- 1.4  Using original if statement and decomposition artefacts synthesise a
--        structured if block.
-- 1.5  Prepend the block to the left over artefacts, which have already been
--        grouped in 1.1
-- 2. Case: head is a statement block containing any other statement:
-- 2.1  Group everything to the right and prepend the head.
groupIf' :: ABlocks a -> ABlocks a
groupIf' [] = []
groupIf' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        | StIfThen _ _ mName _ <- st -> -- If statement
          let ( conditions, blocks, leftOverBlocks, endLabel, endStmt ) =
                decomposeIf (b:groupedBlocks)
          in ( BlIf a (getTransSpan s endStmt) label mName conditions blocks endLabel
             , leftOverBlocks)
      b'' | containsGroups b'' -> -- Map to subblocks for groupable blocks
        ( applyGroupingToSubblocks groupIf' b'', groupedBlocks )
      _ -> ( b, groupedBlocks )
    groupedBlocks = groupIf' bs -- Assume everything to the right is grouped.

-- A program has the following structure:
--
--[ block... ]
-- if <condition> then
--   [ block... ]
-- else if <condition>
--   [ block... ]
-- else
--   [ block... ]
-- end if
-- [ block... ]
--
-- This function must only receive a list of blocks that start with if.
--
-- Internally it uses a more permissive breaking function that processes
-- individual (if-then, block), (else-if, block), and (else, block) pairs.
--
-- In that case it decomposes the block into list of (maybe) conditions and
-- blocks that those conditions correspond to. Additionally, it returns
-- whatever is after the if block.
decomposeIf :: ABlocks a
            -> ( [ Maybe (Expression (Analysis a)) ],
                 [ ABlocks a ],
                 ABlocks a,
                 Maybe (Expression (Analysis a)),
                 Statement (Analysis a) )
decomposeIf blocks@(BlStatement _ _ _ (StIfThen _ _ mTargetName _):_) =
    decomposeIf' blocks
  where
    decomposeIf' (BlStatement _ _ mLabel st:rest) =
      case st of
        StIfThen _ _ _ condition -> go (Just condition) rest
        StElsif _ _ _ condition -> go (Just condition) rest
        StElse{} -> go Nothing rest
        StEndif _ _ mName
          | mName == mTargetName -> ([], [], rest, mLabel, st)
          | otherwise -> error $ "If statement name does not match that of " ++
                                   "the corresponding end if statement."
        _ -> error "Block with non-if related statement. Should never occur."
    decomposeIf' _ = error "can't decompose block"
    go maybeCondition blocks' =
      let (nonConditionBlocks, rest') = collectNonConditionalBlocks blocks'
          (conditions, listOfBlocks, rest'', endLabel, endStmt) = decomposeIf' rest'
      in ( maybeCondition : conditions
         , nonConditionBlocks : listOfBlocks
         , rest''
         , endLabel
         , endStmt )
decomposeIf _ = error "can't decompose block"

-- This compiles the executable blocks under various if conditions.
collectNonConditionalBlocks :: ABlocks a -> (ABlocks a, ABlocks a)
collectNonConditionalBlocks blocks =
  case blocks of
    BlStatement _ _ _ StElsif{}:_ -> ([], blocks)
    BlStatement _ _ _ StElse{}:_ -> ([], blocks)
    -- Here end block is included within the blocks unlike the other
    -- conditional directives. The reason is that this block can be
    -- a branch target if it is labeled according to the specification, hence
    -- it is presence in the parse tree is meaningful.
    BlStatement _ _ _ StEndif{}:_ -> ([], blocks)
    -- Catch all case for all non-if related blocks.
    b:bs -> let (bs', rest) = collectNonConditionalBlocks bs in (b : bs', rest)
    -- In this case the structured if block is malformed and the file ends
    -- prematurely.
    _ -> error "Premature file ending while parsing structured if block."

--------------------------------------------------------------------------------
-- Grouping new do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupDo :: Data a => Transform a ()
groupDo = genericGroup groupDo'

groupDo' :: ABlocks a -> ABlocks a
groupDo' [ ] = [ ]
groupDo' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        -- Do While statement
        | StDoWhile _ _ mTarget Nothing condition <- st ->
          let ( blocks, leftOverBlocks, endLabel ) =
                collectNonDoBlocks groupedBlocks mTarget
          in ( BlDoWhile a (getTransSpan s blocks) label mTarget Nothing condition blocks endLabel
             , leftOverBlocks)
        -- Vanilla do statement
        | StDo _ _ mName Nothing doSpec <- st ->
          let ( blocks, leftOverBlocks, endLabel ) =
                collectNonDoBlocks groupedBlocks mName
          in ( BlDo a (getTransSpan s blocks) label mName Nothing doSpec blocks endLabel
             , leftOverBlocks)
      b'' | containsGroups b'' ->
        ( applyGroupingToSubblocks groupDo' b'', groupedBlocks )
      _ -> ( b, groupedBlocks )
    groupedBlocks = groupDo' bs -- Assume everything to the right is grouped.

collectNonDoBlocks :: ABlocks a -> Maybe String
                   -> ( ABlocks a
                      , ABlocks a
                      , Maybe (Expression (Analysis a)) )
collectNonDoBlocks blocks mNameTarget =
  case blocks of
    BlStatement _ _ mLabel (StEnddo _ _ mName):rest
      | mName == mNameTarget -> ([ ], rest, mLabel)
      | otherwise ->
          error "Do block name does not match that of the end statement."
    b:bs ->
      let (bs', rest, mLabel) = collectNonDoBlocks bs mNameTarget
      in (b : bs', rest, mLabel)
    _ -> error "Premature file ending while parsing structured do block."

--------------------------------------------------------------------------------
-- Grouping labeled do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupLabeledDo :: Data a => Transform a ()
groupLabeledDo = genericGroup groupLabeledDo'

groupLabeledDo' :: ABlocks a -> ABlocks a
groupLabeledDo' [ ] = [ ]
groupLabeledDo' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label
        (StDo _ _ mn tl@Just{} doSpec) ->
          let ( blocks, leftOverBlocks ) =
                collectNonLabeledDoBlocks tl groupedBlocks
              lastLabel = getLastLabel $ last blocks
          in ( BlDo a (getTransSpan s blocks) label mn tl doSpec blocks lastLabel
             , leftOverBlocks )
      BlStatement a s label
        (StDoWhile _ _ mn tl@Just{} cond) ->
          let ( blocks, leftOverBlocks ) =
                collectNonLabeledDoBlocks tl groupedBlocks
              lastLabel = getLastLabel $ last blocks
          in ( BlDoWhile a (getTransSpan s blocks) label mn tl cond blocks lastLabel
             , leftOverBlocks )
      b'' | containsGroups b'' ->
        ( applyGroupingToSubblocks groupLabeledDo' b'', groupedBlocks )
      _ -> (b, groupedBlocks)

    -- Assume everything to the right is grouped.
    groupedBlocks = groupLabeledDo' bs


collectNonLabeledDoBlocks :: Maybe (Expression (Analysis a)) -> ABlocks a
                          -> (ABlocks a, ABlocks a)
collectNonLabeledDoBlocks targetLabel blocks =
  case blocks of
    -- Didn't find a statement with matching label; don't group
    [] -> error "Malformed labeled DO group."

    b:bs
      | compLabel (getLastLabel b) targetLabel -> ([ b ], bs)
      | otherwise ->
          let (bs', rest) = collectNonLabeledDoBlocks targetLabel bs
          in (b : bs', rest)

compLabel :: Maybe (Expression a) -> Maybe (Expression a) -> Bool
compLabel (Just (ExpValue _ _ (ValInteger l1)))
          (Just (ExpValue _ _ (ValInteger l2))) = strip l1 == strip l2
compLabel _ _ = False

strip :: String -> String
strip = dropWhile (=='0')

--------------------------------------------------------------------------------
-- Grouping case statements
--------------------------------------------------------------------------------

groupCase :: Data a => Transform a ()
groupCase = genericGroup groupCase'

groupCase' :: ABlocks a -> ABlocks a
groupCase' [] = []
groupCase' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        | StSelectCase _ _ mName scrutinee <- st ->
          let blocksToDecomp = dropWhile isComment groupedBlocks
              ( conds, blocks, leftOverBlocks, endLabel ) = decomposeCase blocksToDecomp mName
          in ( BlCase a (getTransSpan s blocks) label mName scrutinee conds blocks endLabel
             , leftOverBlocks)
      b'' | containsGroups b'' -> -- Map to subblocks for groupable blocks
        ( applyGroupingToSubblocks groupCase' b'', groupedBlocks )
      _ -> ( b , groupedBlocks )
    groupedBlocks = groupCase' bs -- Assume everything to the right is grouped.
    isComment b'' = case b'' of { BlComment{} -> True; _ -> False }

decomposeCase :: ABlocks a -> Maybe String
              -> ( [ Maybe (AList Index (Analysis a)) ]
                 , [ ABlocks a ]
                 , ABlocks a
                 , Maybe (Expression (Analysis a)) )
decomposeCase (BlStatement _ _ mLabel st:rest) mTargetName =
    case st of
      StCase _ _ mName mCondition
        | Nothing <- mName -> go mCondition rest
        | mName == mTargetName -> go mCondition rest
        | otherwise -> error $ "Case name does not match that of " ++
                                 "the corresponding select case statement."
      StEndcase _ _ mName
        | mName == mTargetName -> ([], [], rest, mLabel)
        | otherwise -> error $ "End case name does not match that of " ++
                                 "the corresponding select case statement."
      _ -> error "Block with non-case related statement. Must not occur."
  where
    go mCondition blocks =
      let (nonCaseBlocks, rest') = collectNonCaseBlocks blocks
          (conditions, listOfBlocks, rest'', endLabel) = decomposeCase rest' mTargetName
      in ( mCondition : conditions
         , nonCaseBlocks : listOfBlocks
         , rest'', endLabel )
decomposeCase _ _ = error "can't decompose case"

-- This compiles the executable blocks under various if conditions.
collectNonCaseBlocks :: ABlocks a -> (ABlocks a, ABlocks a)
collectNonCaseBlocks blocks =
  case blocks of
    BlStatement _ _ _ st:_
      | StCase{} <- st -> ( [], blocks )
      | StEndcase{} <- st -> ( [], blocks )
    -- In this case case block is malformed and the file ends prematurely.
    b:bs -> let (bs', rest) = collectNonCaseBlocks bs in (b : bs', rest)
    _ -> error "Premature file ending while parsing select case block."

--------------------------------------------------------------------------------
-- Helpers for grouping of structured blocks with more blocks inside.
--------------------------------------------------------------------------------

containsGroups :: Block (Analysis a) -> Bool
containsGroups b =
  case b of
    BlStatement{} -> False
    BlIf{} -> True
    BlCase{} -> True
    BlDo{} -> True
    BlDoWhile{} -> True
    BlInterface{} -> False
    BlComment{} -> False
    BlForall{}  -> True

applyGroupingToSubblocks :: (ABlocks a -> ABlocks a) -> Block (Analysis a) -> Block (Analysis a)
applyGroupingToSubblocks f b
  | BlStatement{} <- b =
      error "Individual statements do not have subblocks. Must not occur."
  | BlIf a s l mn conds blocks el <- b = BlIf a s l mn conds (map f blocks) el
  | BlCase a s l mn scrutinee conds blocks el <- b =
      BlCase a s l mn scrutinee conds (map f blocks) el
  | BlDo a s l n tl doSpec blocks el <- b = BlDo a s l n tl doSpec (f blocks) el
  | BlDoWhile a s l n tl doSpec blocks el <- b = BlDoWhile a s l n tl doSpec (f blocks) el
  | BlInterface{} <- b =
      error "Interface blocks do not have groupable subblocks. Must not occur."
  | BlComment{} <- b =
      error "Comment statements do not have subblocks. Must not occur."
  | BlForall a s ml mn h blocks mel <- b =
     BlForall a s ml mn h (f blocks) mel

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
