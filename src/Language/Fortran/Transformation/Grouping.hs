module Language.Fortran.Transformation.Grouping ( groupForall
                                                , groupDo
                                                , groupLabeledDo
                                                ) where

import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.Analysis
import Language.Fortran.Transformation.TransformMonad

import Data.Data
import Data.List (intercalate)
import Data.Generics.Uniplate.Operations

type ABlocks a = [ Block (Analysis a) ]

genericGroup :: Data a => (ABlocks a -> ABlocks a) -> (Statement (Analysis a) -> Bool) -> Transform a ()
genericGroup groupingFunction checkingFunction = do
    pf <- getProgramFile
    let pf' = transformBi groupingFunction pf
        bad = filter checkingFunction $ universeBi pf'
    if null bad
      then putProgramFile pf'
      else let spans = [ apparentFilePath p1 ++ " " ++ show ss | b <- bad, let ss@(SrcSpan p1 _) = getSpan b ] in
             error $ "Mis-matched grouping statements at these position(s): " ++ intercalate ", " spans

--------------------------------------------------------------------------------
-- Grouping FORALL statement blocks into FORALL blocks in entire parse tree
--------------------------------------------------------------------------------
groupForall :: Data a => Transform a ()
groupForall = genericGroup groupForall' isForall


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

isForall :: Statement a -> Bool
isForall (StForall{}) = True
isForall (StForallStatement{}) = True
isForall _ = False


--------------------------------------------------------------------------------
-- Grouping new do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupDo :: Data a => Transform a ()
groupDo = genericGroup groupDo' isDo

groupDo' :: ABlocks a -> ABlocks a
groupDo' [ ] = [ ]
groupDo' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label st
        -- Do While statement
        | StDoWhile _ _ mTarget Nothing condition <- st ->
          let ( blocks, leftOverBlocks, endLabel, stEnd ) =
                collectNonDoBlocks groupedBlocks mTarget
          in ( BlDoWhile a (getTransSpan s stEnd) label mTarget Nothing condition blocks endLabel
             , leftOverBlocks)
        -- Vanilla do statement
        | StDo _ _ mName Nothing doSpec <- st ->
          let ( blocks, leftOverBlocks, endLabel, stEnd ) =
                collectNonDoBlocks groupedBlocks mName
          in ( BlDo a (getTransSpan s stEnd) label mName Nothing doSpec blocks endLabel
             , leftOverBlocks)
      b'' | containsGroups b'' ->
        ( applyGroupingToSubblocks groupDo' b'', groupedBlocks )
      _ -> ( b, groupedBlocks )
    groupedBlocks = groupDo' bs -- Assume everything to the right is grouped.

collectNonDoBlocks :: ABlocks a -> Maybe String
                   -> ( ABlocks a
                      , ABlocks a
                      , Maybe (Expression (Analysis a))
                      , Statement (Analysis a) )
collectNonDoBlocks blocks mNameTarget =
  case blocks of
    BlStatement _ _ mLabel st@(StEnddo _ _ mName):rest
      | mName == mNameTarget -> ([ ], rest, mLabel, st)
      | otherwise ->
          error "Do block name does not match that of the end statement."
    b:bs ->
      let (bs', rest, mLabel, stEnd) = collectNonDoBlocks bs mNameTarget
      in (b : bs', rest, mLabel, stEnd)
    _ -> error "Premature file ending while parsing structured do block."

isDo :: Statement a -> Bool
isDo s = case s of
  StDo _ _ _ Nothing _      -> True
  StDoWhile _ _ _ Nothing _ -> True
  StEnddo{}                 -> True
  _                         -> False

--------------------------------------------------------------------------------
-- Grouping labeled do statement blocks into do blocks in entire parse tree
--------------------------------------------------------------------------------

groupLabeledDo :: Data a => Transform a ()
groupLabeledDo = genericGroup groupLabeledDo' isLabeledDo

groupLabeledDo' :: ABlocks a -> ABlocks a
groupLabeledDo' [ ] = [ ]
groupLabeledDo' (b:bs) = b' : bs'
  where
    (b', bs') = case b of
      BlStatement a s label
        (StDo _ _ mn tl@Just{} doSpec) ->
          let ( blocks, leftOverBlocks, lastLabel ) =
                collectNonLabeledDoBlocks tl groupedBlocks
          in ( BlDo a (getTransSpan s blocks) label mn tl doSpec blocks lastLabel
             , leftOverBlocks )
      BlStatement a s label
        (StDoWhile _ _ mn tl@Just{} cond) ->
          let ( blocks, leftOverBlocks, lastLabel ) =
                collectNonLabeledDoBlocks tl groupedBlocks
          in ( BlDoWhile a (getTransSpan s blocks) label mn tl cond blocks lastLabel
             , leftOverBlocks )
      b'' | containsGroups b'' ->
        ( applyGroupingToSubblocks groupLabeledDo' b'', groupedBlocks )
      _ -> (b, groupedBlocks)

    -- Assume everything to the right is grouped.
    groupedBlocks = groupLabeledDo' bs


collectNonLabeledDoBlocks :: Maybe (Expression (Analysis a)) -> ABlocks a
                          -> (ABlocks a, ABlocks a, Maybe (Expression (Analysis a)))
collectNonLabeledDoBlocks targetLabel blocks =
  case blocks of
    -- Didn't find a statement with matching label; don't group
    [] -> error "Malformed labeled DO group."
    b:bs
      | compLabel (getLastLabel b) targetLabel -> (b1, bs, getLastLabel b)
      | otherwise                              -> (b : bs', rest, ll)
      where (bs', rest, ll) = collectNonLabeledDoBlocks targetLabel bs
            b1 = case b of BlStatement _ _ _ StEnddo{}    -> []
                           BlStatement _ _ _ StContinue{} -> []
                           _                              -> [b]


compLabel :: Maybe (Expression a) -> Maybe (Expression a) -> Bool
compLabel (Just (ExpValue _ _ (ValInteger l1)))
          (Just (ExpValue _ _ (ValInteger l2))) = strip l1 == strip l2
compLabel _ _ = False

strip :: String -> String
strip = dropWhile (=='0')

isLabeledDo :: Statement a -> Bool
isLabeledDo s = case s of
  StDo _ _ _ Just{} _       -> True
  StDoWhile _ _ _ Just{} _  -> True
  _                         -> False

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
