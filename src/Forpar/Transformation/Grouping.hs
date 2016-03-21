module Forpar.Transformation.Grouping ( groupIf
                                      , groupDo
                                      ) where

import Forpar.AST
import Forpar.Transformation.TransformMonad

genericGroup :: ([ Block a ] -> [ Block a ]) -> Transform a ()
genericGroup groupingFunction = do
    ProgramFile pus e <- getProgramFile
    putProgramFile $ ProgramFile (zip (map fst pus) . map (go . snd) $ pus) e
  where
    go pu =
      case pu of
        PUMain a s n bs -> PUMain a s n $ groupingFunction bs
        PUSubroutine a s n as bs -> PUSubroutine a s n as $ groupingFunction bs
        PUFunction a s r n as bs -> PUFunction a s r n as $ groupingFunction bs
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
groupIf' :: [ Block a ] -> [ Block a ]
groupIf' [] = []
groupIf' blocks@(b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label StIfThen{} -> -- If statement
        let ( conditions, blocks, leftOverBlocks ) =
              breakIntoIfComponents (b:groupedBlocks)
        in ( BlIf a (getTransSpan s blocks) label conditions blocks
           , leftOverBlocks)
      BlDo a s label doSpec bs' -> (BlDo a s label doSpec (groupIf' bs'), groupedBlocks)
      BlDoWhile a s label condition bs' -> (BlDoWhile a s label condition (groupIf' bs'), groupedBlocks)
      _ -> (b, groupedBlocks)
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
breakIntoIfComponents :: [ Block a ] -> ([ Maybe (Expression a) ], [ [ Block a ] ], [ Block a ])
breakIntoIfComponents blocks@(BlStatement _ _ _ StIfThen{}:rest) =
    breakIntoIfComponents' blocks
  where
    breakIntoIfComponents' (BlStatement _ _ _ st:rest) =
      case st of
        StIfThen _ _ condition -> go (Just condition) rest
        StElsif _ _ condition -> go (Just condition) rest
        StElse{} -> go Nothing rest
        StEndif{} -> ([], [], rest)
        _ -> error "Block with non-if related statement. Should never occur."
    go maybeCondition blocks =
      let (nonConditionBlocks, rest') = collectNonConditionalBlocks blocks
          (conditions, listOfBlocks, rest'') = breakIntoIfComponents' rest'
      in ( maybeCondition : conditions
         , nonConditionBlocks : listOfBlocks
         , rest'' )

-- This compiles the executable blocks under various if conditions.
collectNonConditionalBlocks :: [ Block a ] -> ([ Block a ], [ Block a ])
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

groupDo' :: [ Block a ] -> [ Block a ]
groupDo' [ ] = [ ]
groupDo' blocks@(b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label (StDoWhile _ _ condition) -> -- Do While statement
        let ( blocks, leftOverBlocks ) =
              collectNonDoBlocks groupedBlocks
        in ( BlDoWhile a (getTransSpan s blocks) label condition blocks
           , leftOverBlocks)
      BlStatement a s label (StDo _ _ Nothing doSpec) -> -- Vanilla do statement
        let ( blocks, leftOverBlocks ) =
              collectNonDoBlocks groupedBlocks
        in ( BlDo a (getTransSpan s blocks) label doSpec blocks
           , leftOverBlocks)
      BlIf a s label conds blocks -> (BlIf a s label conds (map groupDo' blocks), groupedBlocks)
      _ -> (b, groupedBlocks)
    groupedBlocks = groupDo' bs -- Assume everything to the right is grouped.

collectNonDoBlocks :: [ Block a ] -> ([ Block a], [ Block a ])
collectNonDoBlocks blocks =
  case blocks of
    b@(BlStatement _ _ _ StEnddo{}):rest -> ([ b ], rest)
    b:bs -> let (bs', rest) = collectNonDoBlocks bs in (b : bs', rest)
    _ -> error "Premature file ending while parsing structured do block."

