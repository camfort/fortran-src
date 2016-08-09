module Language.Fortran.Transformation.Grouping ( groupIf
                                      , groupDo
                                      , groupLabeledDo
                                      , groupCase
                                      ) where

import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Transformation.TransformMonad

import Debug.Trace

genericGroup :: ([ Block (Analysis a) ] -> [ Block (Analysis a) ]) -> Transform a ()
genericGroup groupingFunction =
    modifyProgramFile $
      \(ProgramFile pus e) ->
        ProgramFile (zip (map fst pus) . map (go . snd) $ pus) e
  where
    go pu =
      case pu of
        PUMain a s n bs subPUs ->
          PUMain a s n (groupingFunction bs) (map go <$> subPUs)
        PUModule a s n bs subPUs ->
          PUModule a s n (groupingFunction bs) (map go <$> subPUs)
        PUSubroutine a s r n as bs subPUs ->
          PUSubroutine a s r n as (groupingFunction bs) (map go <$> subPUs)
        PUFunction a s r rec n as res bs subPUs ->
          PUFunction a s r rec n as res (groupingFunction bs) (map go <$> subPUs)
        bd@PUBlockData{} -> bd -- Block data cannot have any if statements.

--------------------------------------------------------------------------------
-- Grouping if statement blocks into if blocks in entire parse tree
--------------------------------------------------------------------------------

groupIf :: Transform a ()
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
-- 2. Case: head is a statement block contianing any other statement:
-- 2.1  Group everything to the right and prepend the head.
groupIf' :: [ Block (Analysis a) ] -> [ Block (Analysis a) ]
groupIf' [] = []
groupIf' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        | StIfThen{} <- st -> -- If statement
          let ( conditions, blocks, leftOverBlocks ) =
                decomposeIf (b:groupedBlocks)
          in ( BlIf a (getTransSpan s blocks) label conditions blocks
             , leftOverBlocks)
      b | containsGroups b -> -- Map to subblocks for groupable blocks
        ( applyGroupingToSubblocks groupIf' b, groupedBlocks )
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
decomposeIf :: [ Block (Analysis a) ] -> ([ Maybe (Expression (Analysis a)) ], [ [ Block (Analysis a) ] ], [ Block (Analysis a) ])
decomposeIf blocks@(BlStatement _ _ _ (StIfThen _ _ mTargetName _):rest) =
    decomposeIf' blocks
  where
    decomposeIf' (BlStatement _ _ _ st:rest) =
      case st of
        StIfThen _ _ _ condition -> go (Just condition) rest
        StElsif _ _ _ condition -> go (Just condition) rest
        StElse{} -> go Nothing rest
        StEndif _ _ mName
          | mName == mTargetName -> ([], [], rest)
          | otherwise -> error $ "If statement name does not match that of " ++
                                   "the corresponding end if statement."
        _ -> error "Block with non-if related statement. Should never occur."
    go maybeCondition blocks =
      let (nonConditionBlocks, rest') = collectNonConditionalBlocks blocks
          (conditions, listOfBlocks, rest'') = decomposeIf' rest'
      in ( maybeCondition : conditions
         , nonConditionBlocks : listOfBlocks
         , rest'' )

-- This compiles the executable blocks under various if conditions.
collectNonConditionalBlocks :: [ Block (Analysis a) ] -> ([ Block (Analysis a) ], [ Block (Analysis a) ])
collectNonConditionalBlocks blocks =
  case blocks of
    BlStatement _ _ _ StElsif{}:_ -> ([], blocks)
    BlStatement _ _ _ StElse{}:_ -> ([], blocks)
    -- Here end block is included within the blocks unlike the other
    -- conditional directives. The reason is that this block can be
    -- a branch target if it is labeled according to the specification, hence
    -- it is presence in the parse tree is meaningful.
    b@(BlStatement _ _ _ StEndif{}):_ -> ([ b ], blocks)
    -- Catch all case for all non-if related blocks.
    b:bs -> let (bs', rest) = collectNonConditionalBlocks bs in (b : bs', rest)
    -- In this case the structured if block is malformed and the file ends
    -- prematurely.
    _ -> error "Premature file ending while parsing structured if block."

--------------------------------------------------------------------------------
-- Grouping new do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupDo :: Transform a ()
groupDo = genericGroup groupDo'

groupDo' :: [ Block (Analysis a) ] -> [ Block (Analysis a) ]
groupDo' [ ] = [ ]
groupDo' blocks@(b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        -- Do While statement
        | StDoWhile _ _ mTarget _ condition <- st ->
          let ( blocks, leftOverBlocks ) =
                collectNonDoBlocks groupedBlocks mTarget
          in ( BlDoWhile a (getTransSpan s blocks) label mTarget condition blocks
             , leftOverBlocks)
        -- Vanilla do statement
        | StDo _ _ mName Nothing doSpec <- st ->
          let ( blocks, leftOverBlocks ) =
                collectNonDoBlocks groupedBlocks mName
          in ( BlDo a (getTransSpan s blocks) label mName Nothing doSpec blocks
             , leftOverBlocks)
      b | containsGroups b ->
        ( applyGroupingToSubblocks groupDo' b, groupedBlocks )
      _ -> ( b, groupedBlocks )
    groupedBlocks = groupDo' bs -- Assume everything to the right is grouped.

collectNonDoBlocks :: [ Block (Analysis a) ] -> Maybe String -> ([ Block (Analysis a)], [ Block (Analysis a) ])
collectNonDoBlocks blocks mNameTarget =
  case blocks of
    b@(BlStatement _ _ _ (StEnddo _ _ mName)):rest
      | mName == mNameTarget -> ([ b ], rest)
      | otherwise ->
          error "Do block name does not match that of the end statement."
    b:bs ->
      let (bs', rest) = collectNonDoBlocks bs mNameTarget in (b : bs', rest)
    _ -> error "Premature file ending while parsing structured do block."

--------------------------------------------------------------------------------
-- Grouping labeled do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupLabeledDo :: Transform a ()
groupLabeledDo = genericGroup groupLabeledDo'

groupLabeledDo' :: [ Block (Analysis a) ] -> [ Block (Analysis a) ]
groupLabeledDo' [ ] = [ ]
groupLabeledDo' blos@(b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label (StDo _ _ mn tl@(Just (ExpValue _ _ (ValInteger targetLabel))) doSpec) ->
        let ( blocks, leftOverBlocks ) =
              collectNonLabeledDoBlocks targetLabel groupedBlocks
        in ( BlDo a (getTransSpan s blocks) label mn tl doSpec blocks
           , leftOverBlocks)
      b | containsGroups b ->
        ( applyGroupingToSubblocks groupLabeledDo' b, groupedBlocks )
      _ -> (b, groupedBlocks)
    groupedBlocks = groupLabeledDo' bs -- Assume everything to the right is grouped.

collectNonLabeledDoBlocks :: String -> [ Block (Analysis a) ] -> ([ Block (Analysis a) ], [ Block (Analysis a) ])
collectNonLabeledDoBlocks targetLabel blocks =
  case blocks of
    b@(BlStatement _ _ (Just (ExpValue _ _ (ValInteger label))) _):rest
      | label == targetLabel -> ([ b ], rest)
    b:bs -> let (bs', rest) = collectNonLabeledDoBlocks targetLabel bs in (b : bs', rest)

--------------------------------------------------------------------------------
-- Grouping case statements
--------------------------------------------------------------------------------

groupCase :: Transform a ()
groupCase = genericGroup groupCase'

groupCase' :: [ Block (Analysis a) ] -> [ Block (Analysis a) ]
groupCase' [] = []
groupCase' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        | StSelectCase _ _ mTargetName scrutinee <- st ->
          let blocksToDecomp = dropWhile isComment groupedBlocks
              ( conds, blocks, leftOverBlocks ) = decomposeCase blocksToDecomp mTargetName
          in ( BlCase a (getTransSpan s blocks) label scrutinee conds blocks
             , leftOverBlocks)
      b | containsGroups b -> -- Map to subblocks for groupable blocks
        ( applyGroupingToSubblocks groupCase' b, groupedBlocks )
      _ -> ( b , groupedBlocks )
    groupedBlocks = groupCase' bs -- Assume everything to the right is grouped.
    isComment b = case b of { BlComment{} -> True; _ -> False }

decomposeCase :: [ Block (Analysis a) ] -> Maybe String -> ([ Maybe (AList Index (Analysis a)) ], [ [ Block (Analysis a) ] ], [ Block (Analysis a) ])
decomposeCase blocks@(BlStatement _ _ _ st:rest) mTargetName =
    case st of
      StCase _ _ mName mCondition
        | Nothing <- mName -> go mCondition rest
        | mName == mTargetName -> go mCondition rest
        | otherwise -> error $ "Case name does not match that of " ++
                                 "the corresponding select case statement."
      StEndcase _ _ mName
        | mName == mTargetName -> ([], [], rest)
        | otherwise -> error $ "End case name does not match that of " ++
                                 "the corresponding select case statement."
      _ -> error "Block with non-case related statement. Must not occur."
  where
    go mCondition blocks =
      let (nonCaseBlocks, rest) = collectNonCaseBlocks blocks
          (conditions, listOfBlocks, rest') = decomposeCase rest mTargetName
      in ( mCondition : conditions
         , nonCaseBlocks : listOfBlocks
         , rest' )

-- This compiles the executable blocks under various if conditions.
collectNonCaseBlocks :: [ Block (Analysis a) ] -> ([ Block (Analysis a) ], [ Block (Analysis a) ])
collectNonCaseBlocks blocks =
  case blocks of
    b@(BlStatement _ _ _ st):_
      | StCase{} <- st -> ( [], blocks )
      | StEndcase{} <- st -> ( [ b ], blocks )
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

applyGroupingToSubblocks :: ([ Block (Analysis a) ] -> [ Block (Analysis a) ]) -> Block (Analysis a) -> Block (Analysis a)
applyGroupingToSubblocks f b
  | BlStatement{} <- b =
      error "Individual statements do not have subblocks. Must not occur."
  | BlIf a s l conds blocks <- b = BlIf a s l conds $ map f blocks
  | BlCase a s l scrutinee conds blocks <- b =
      BlCase a s l scrutinee conds $ map f blocks
  | BlDo a s l n tl doSpec blocks <- b = BlDo a s l n tl doSpec $ f blocks
  | BlDoWhile a s l n doSpec blocks <- b = BlDoWhile a s l n doSpec $ f blocks
  | BlInterface{} <- b =
      error "Interface blocks do not have groupable subblocks. Must not occur."
  | BlComment{} <- b =
    error "Comment statements do not have subblocks. Must not occur."

--------------------------------------------------

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
