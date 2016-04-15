-- | Analyse a program file and create basic blocks.

{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Forpar.Analysis.BBlocks
  ( analyseBBlocks, genBBlockMap, showBBGr, showAnalysedBBGr, showBBlocks, BBlockMap, genSuperBBGr )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Function hiding ((&))
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.PrettyPrint.GenericPretty (pretty, Out)
import Forpar.Analysis
import Forpar.AST
import Forpar.Util.Position
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (foldl')
import Data.Maybe

--------------------------------------------------

-- | Insert basic block graphs into each program unit's analysis
analyseBBlocks :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseBBlocks = labelBlocksInBBGr . analyseBBlocks'
analyseBBlocks' (ProgramFile cm_pus cs) = ProgramFile (map (fmap toBBlocksPerPU) cm_pus) cs

--------------------------------------------------

type BBlockMap a = M.Map ProgramUnitName (BBGr a)
genBBlockMap :: Data a => ProgramFile (Analysis a) -> BBlockMap (Analysis a)
genBBlockMap (ProgramFile cm_pus _) = M.fromList [
    (getName pu, gr) | (_, pu) <- cm_pus, let Just gr = bBlocks (getAnnotation pu)
  ]

--------------------------------------------------

-- | Insert unique labels on each AST-block, inside each bblock, for
-- easier look-up later
labelBlocksInBBGr :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
labelBlocksInBBGr gr = evalState (transform (nmapM' (mapM eachBlock)) gr) [1..]
  where
    eachBlock :: Data a => Block (Analysis a) -> State [Int] (Block (Analysis a))
    eachBlock b = do
      n:ns <- get
      put ns
      return $ setAnnotation ((getAnnotation b) { insLabel = Just n }) b
    transform :: Data a => (BBGr a -> State [Int] (BBGr a)) ->
                           ProgramFile a -> State [Int] (ProgramFile a)
    transform = transformBiM

--------------------------------------------------

-- Analyse each program unit
toBBlocksPerPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
toBBlocksPerPU pu
  | null bs   = pu
  | otherwise = pu'
  where
    bs  = case pu of PUMain _ _ _ bs -> bs; PUSubroutine _ _ _ _ bs -> bs; PUFunction _ _ _ _ _ bs -> bs
                     _ -> []
    bbs = execBBlocker (processBlocks bs)
    fix = delEmptyBBlocks . delUnreachable . insExitEdges pu lm . delInvalidExits . insEntryEdges pu
    gr  = fix (insEdges (newEdges bbs) (bbGraph bbs))
    pu' = setAnnotation ((getAnnotation pu) { bBlocks = Just gr }) pu
    lm  = labelMap bbs

-- Create node 0 "the start node" and link it
-- for now assume only one entry
insEntryEdges pu = insEdge (0, 1, ()) . insNode (0, bs)
  where
    bs = genInOutAssignments pu False

-- create assignments of the form "x = f[1]" or "f[1] = x" at the
-- entry/exit bblocks.
genInOutAssignments pu exit
  -- for now, designate return-value slot as a "ValVariable" without type-checking.
  | exit, PUFunction _ _ _ n _ _ <- pu = zipWith genAssign (ValVariable a n:vs) [0..]
  | otherwise                          = zipWith genAssign vs [1..]
  where
    puName = getName pu
    name i = case puName of Named n -> n ++ "[" ++ show i ++ "]"
    (a, s, vs) = case pu of
      PUFunction _ _ _ _ (AList a s vs) _ -> (a, s, vs)
      PUSubroutine _ _ _ (AList a s vs) _ -> (a, s, vs)
      _                                   -> (undefined, undefined, [])
    genAssign v i = BlStatement a s Nothing (StExpressionAssign a s (ExpValue a s vl)
                                                                    (ExpValue a s vr))
      where
        (vl, vr) = if exit then (v', v) else (v, v')
        v'       = case v of
          ValArray a _    -> ValArray a (name i)
          ValVariable a _ -> ValVariable a (name i)
          _               -> error $ "unhandled genAssign case: " ++ show (fmap (const ()) v)

-- Remove exit edges for bblocks where standard construction doesn't apply.
delInvalidExits gr = flip delEdges gr $ do
  n  <- nodes gr
  bs <- maybeToList $ lab gr n
  guard $ isFinalBlockCtrlXfer bs
  le <- out gr n
  return $ toEdge le

-- Insert exit edges for bblocks with special handling.
insExitEdges pu lm gr = flip insEdges (insNode (-1, bs) gr) $ do
  n <- nodes gr
  guard $ null (out gr n)
  bs <- maybeToList $ lab gr n
  n' <- examineFinalBlock lm bs
  return (n, n', ())
  where
    bs = genInOutAssignments pu True

-- Find target of Goto statements (Return statements default target to -1).
examineFinalBlock lm bs@(_:_)
  | BlStatement _ _ _ (StGotoUnconditional _ _ k) <- last bs = [lookupBBlock lm k]
  | BlStatement _ _ _ (StGotoAssigned _ _ _ ks)   <- last bs = map (lookupBBlock lm) (aStrip ks)
  | BlStatement _ _ _ (StGotoComputed _ _ ks _)   <- last bs = map (lookupBBlock lm) (aStrip ks)
  | BlStatement _ _ _ (StReturn _ _ _)            <- last bs = [-1]
examineFinalBlock _ _                                        = [-1]

-- True iff the final block in the list is an explicit control transfer.
isFinalBlockCtrlXfer bs@(_:_)
  | BlStatement _ _ _ (StGotoUnconditional {}) <- last bs = True
  | BlStatement _ _ _ (StGotoAssigned {})      <- last bs = True
  | BlStatement _ _ _ (StGotoComputed {})      <- last bs = True
  | BlStatement _ _ _ (StReturn {})            <- last bs = True
isFinalBlockCtrlXfer _                                    = False

lookupBBlock lm (ExpValue _ _ (ValLabel l)) = (-1) `fromMaybe` M.lookup l lm

-- Seek out empty bblocks with a single entrance and a single exit
-- edge, and remove them, re-establishing the edges without them.
delEmptyBBlocks gr
  | (n, s, t, l):_ <- candidates = delEmptyBBlocks . insEdge (s, t, l) . delNode n $ gr
  | otherwise                    = gr
  where
    -- recompute candidate nodes each iteration
    candidates = do
      let emptyBBs = filter (null . snd) (labNodes gr)
      let adjs     = map (\ (n, _) -> (n, inn gr n, out gr n)) emptyBBs
      (n, [(s,_,l)], [(_,t,_)]) <- adjs
      return (n, s, t, l)

-- Delete unreachable nodes.
delUnreachable gr = subgraph (reachable 0 gr) gr

--------------------------------------------------

-- Running state during basic block analyser.
data BBState a = BBS { bbGraph  :: BBGr a
                     , curBB    :: BB a
                     , curNode  :: Node
                     , labelMap :: M.Map String Node
                     , nums     :: [Int]
                     , newEdges :: [LEdge ()] }

-- Initial state
bbs0 = BBS { bbGraph = empty, curBB = [], curNode = 1, labelMap = M.empty, nums = [2..], newEdges = [] }

-- Monad
type BBlocker a = State (BBState a)

-- Monad entry function.
execBBlocker :: BBlocker a b -> BBState a
execBBlocker = flip execState bbs0

--------------------------------------------------

-- Handle a list of blocks (typically from ProgramUnit or nested inside a BlDo, BlIf, etc).
processBlocks :: [Block a] -> BBlocker a (Node, Node)
-- precondition: curNode is not yet in the graph && will label the first block
-- postcondition: final bblock is in the graph labeled as endN && curNode == endN
-- returns start and end nodes for basic block graph corresponding to parameter bs
processBlocks bs = do
  startN <- gets curNode
  mapM_ perBlock bs
  endN   <- gets curNode
  modify $ \ st -> st { bbGraph = insNode (endN, reverse (curBB st)) (bbGraph st)
                      , curBB   = [] }
  return (startN, endN)

--------------------------------------------------

-- Handle an AST-block element
perBlock :: Block a -> BBlocker a ()
-- invariant: curNode corresponds to curBB, and is not yet in the graph
-- invariant: curBB is in reverse order
perBlock b@(BlIf _ _ _ exps bss) = do
  processLabel b
  addToBBlock $ stripNestedBlocks b
  (ifN, _) <- closeBBlock

  -- go through nested AST-blocks
  startEnds <- forM bss $ \ bs -> do
    (thenN, endN) <- processBlocks bs
    genBBlock
    return (thenN, endN)

  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN   <- gets curNode
  let es  = startEnds >>= \ (thenN, endN) -> [(ifN, thenN, ()), (endN, nxtN, ())]
  -- if there is no "Else"-statement then we need an edge from ifN -> nxtN
  createEdges $ if any isNothing exps then es else (ifN, nxtN, ()):es

perBlock b@(BlStatement a ss _ (StIfLogical _ _ exp stm)) = do
  processLabel b
  addToBBlock $ stripNestedBlocks b

  -- start a bblock for the nested statement inside the If
  (ifN, thenN) <- closeBBlock

  -- build pseudo-AST-block to contain nested statement
  processBlocks [BlStatement a ss Nothing stm]
  endN <- gets curNode

  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN <- genBBlock
  createEdges [(ifN, thenN, ()), (ifN, nxtN, ()), (thenN, nxtN, ())]

perBlock b@(BlStatement _ _ _ (StIfArithmetic {})) = error "BBlocks: StIfArithmetic unsupported"
perBlock b@(BlDo _ _ mlab spec bs) = perDoBlock b bs
perBlock b@(BlDoWhile _ _ mlab spec bs) = perDoBlock b bs
perBlock b@(BlStatement _ _ _ (StReturn {})) =
  processLabel b >> addToBBlock b >> closeBBlock_
perBlock b@(BlStatement _ _ _ (StGotoUnconditional {})) =
  processLabel b >> addToBBlock b >> closeBBlock_
perBlock b@(BlStatement a s l (StCall a' s' cn@(ExpValue _ _ (ValSubroutineName n)) (Just aexps))) = do
  (prevN, formalN) <- closeBBlock

  -- create bblock that assigns formal parameters (n[1], n[2], ...)
  case l of
    Just (ExpValue _ _ (ValLabel l)) -> insertLabel l formalN -- label goes here, if present
    _                                -> return ()
  let name i   = n ++ "[" ++ show i ++ "]"
  let formal (ExpValue a s (ValVariable a' _)) i = ExpValue a s (ValVariable a' (name i))
      formal (ExpValue a s (ValArray a' _)) i    = ExpValue a s (ValArray a' (name i))
      formal e i                                 = ExpValue a s (ValVariable a (name i))
        where a = getAnnotation e; s = getSpan e
  forM_ (zip (aStrip aexps) [1..]) $ \ (e, i) ->
    addToBBlock $ BlStatement a s Nothing (StExpressionAssign a' s' (formal e i) e)
  (_, dummyCallN) <- closeBBlock

  -- create "dummy call" bblock with no parameters in the StCall AST-node.
  addToBBlock $ BlStatement a s Nothing (StCall a' s' cn Nothing)
  (_, returnedN) <- closeBBlock

  -- re-assign the variables using the values of the formal parameters, if possible
  -- (because call-by-reference)
  let formal (ExpValue a s (ValVariable a' _)) i = ExpValue a s (ValVariable a' (name i))
      formal (ExpValue a s (ValArray a' _)) i    = ExpValue a s (ValArray a' (name i))
      formal e i                                 = ExpValue a s (ValVariable a (name i))
        where a = getAnnotation e; s = getSpan e
  forM_ (zip (aStrip aexps) [1..]) $ \ (e, i) ->
    -- this is only possible for l-expressions
    if isLExpr e then
      addToBBlock $ BlStatement a s Nothing (StExpressionAssign a' s' e (formal e i))
    else return ()
  (_, nextN) <- closeBBlock

  -- connect the bblocks
  createEdges [ (prevN, formalN, ()), (formalN, dummyCallN, ())
              , (dummyCallN, returnedN, ()), (returnedN, nextN, ()) ]

perBlock b = processLabel b >> addToBBlock b

--------------------------------------------------
-- helpers

-- Do-block helper
perDoBlock :: Block a -> [Block a] -> BBlocker a ()
perDoBlock b bs = do
  (n, doN) <- closeBBlock
  case getLabel b of
    Just (ExpValue _ _ (ValLabel l)) -> insertLabel l doN
    _                                -> return ()
  addToBBlock $ stripNestedBlocks b
  closeBBlock
  -- process nested bblocks inside of do-statement
  (startN, endN) <- processBlocks bs
  (_, n') <- closeBBlock
  -- connect all the new bblocks with edges, link to subsequent bblock labeled n'
  createEdges [(n, doN, ()), (doN, n', ()), (doN, startN, ()), (endN, doN, ())]

-- Maintains perBlock invariants while potentially starting a new
-- bblock in case of a label.
processLabel :: Block a -> BBlocker a ()
processLabel b | Just (ExpValue _ _ (ValLabel l)) <- getLabel b = do
  (n, n') <- closeBBlock
  insertLabel l n'
  createEdges [(n, n', ())]
processLabel _ = return ()

-- Inserts into labelMap
insertLabel l n = modify $ \ st -> st { labelMap = M.insert l n (labelMap st) }

-- Puts an AST block into the current bblock.
addToBBlock :: Block a -> BBlocker a ()
addToBBlock b = modify $ \ st -> st { curBB = b:curBB st }

-- Closes down the current bblock and opens a new one.
closeBBlock :: BBlocker a (Node, Node)
closeBBlock = do
  n  <- gets curNode
  modify $ \ st -> st { bbGraph = insNode (n, reverse (curBB st)) (bbGraph st), curBB = [] }
  n' <- genBBlock
  return (n, n')
closeBBlock_ = closeBBlock >> return ()

-- Starts up a new bblock.
genBBlock :: BBlocker a Int
genBBlock = do
  n' <- gen
  modify $ \ st -> st { curNode = n', curBB = [] }
  return n'

-- Adds labeled-edge mappings.
createEdges es = modify $ \ st -> st { newEdges = es ++ newEdges st }

-- Generates a new node number.
gen :: BBlocker a Int
gen = do
  n:ns <- gets nums
  modify $ \ s -> s { nums = ns }
  return n

-- Strip nested code not necessary since it is duplicated in another
-- basic block.
stripNestedBlocks (BlDo a s l ds _)         = BlDo a s l ds []
stripNestedBlocks (BlDoWhile a s l e _)     = BlDoWhile a s l e []
stripNestedBlocks (BlIf a s l exps _)       = BlIf a s l exps []
stripNestedBlocks (BlStatement a s l
                   (StIfLogical a' s' e _)) = BlStatement a s l (StIfLogical a' s' e (StEndif a' s'))
stripNestedBlocks b                         = b

--------------------------------------------------
-- Supergraph: all program units in one basic-block graph
-- FIXME: handle functions

genSuperBBGr :: BBlockMap a -> BBGr a
genSuperBBGr bbm = superGraph'
  where
    -- [((PUName, Node), [Block a])]
    namedNodes = [ ((name, n), bs) | (name, gr) <- M.toList bbm, (n, bs) <- labNodes gr ]
    -- [((PUName, Node), (PUName, Node), Label)]
    namedEdges = [ ((name, n), (name, m), l) | (name, gr) <- M.toList bbm, (n, m, l) <- labEdges gr ]
    -- ((PUName, Node) -> SuperNode)
    superNodeMap = M.fromList $ zip (map fst namedNodes) [1..]
    -- ((PUName, Node) -> SuperNode)
    getSuperNode = fromJust . flip M.lookup superNodeMap
    -- [(SuperNode, [Block a])]
    superNodes   = [ (getSuperNode n, bs) | (n, bs) <- namedNodes ]
    -- (((PUName, Node), (PUName, Node), Label), (SuperNode, SuperNode, Label))
    superEdges   = [ (getSuperNode n, getSuperNode m, l) | (n, m, l) <- namedEdges ]
    superGraph   = mkGraph superNodes superEdges
    -- PUName -> SuperNode
    entryMap = M.fromList [ (name, n') | ((name, n), n') <- M.toList superNodeMap, n == 0  ]
    exitMap  = M.fromList [ (name, n') | ((name, n), n') <- M.toList superNodeMap, n == -1 ]
    -- [(SuperNode, String)]
    stCalls  = [ (getSuperNode n, sub) | (n, [BlStatement _ _ _ (StCall _ _ e Nothing)]) <- namedNodes
                                       , ExpValue _ _ (ValSubroutineName sub)            <- [e] ]
    -- [([SuperEdge], SuperNode, String, [SuperEdge])]
    stCallCtxts = [ (inn superGraph n, n, sub, out superGraph n) | (n, sub) <- stCalls ]
    -- [SuperEdge]
    stCallEdges = concat [   [ (m, nEn, l) | (m, _, l) <- inEdges ] ++
                             [ (nEx, m, l) | (_, m, l) <- outEdges ]
                         | (inEdges, _, sub, outEdges) <- stCallCtxts
                         , let nEn = fromJust (M.lookup (Named sub) entryMap)
                         , let nEx = fromJust (M.lookup (Named sub) exitMap) ]
    superGraph' = insEdges stCallEdges . delNodes (map fst stCalls) $ superGraph

--------------------------------------------------

-- | Show a basic block graph in a somewhat decent way.
showBBGr :: (Out a, Show a) => BBGr a -> String
showBBGr gr = execWriter . forM ns $ \ (n, bs) -> do
                let b = "BBLOCK " ++ show n ++ " -> " ++ show (map (\ (_, m, _) -> m) $ out gr n)
                tell $ "\n\n" ++ b
                tell $ "\n" ++ replicate (length b) '-' ++ "\n"
                tell (((++"\n") . pretty) =<< bs)
  where ns = labNodes gr

showAnalysedBBGr :: (Out a, Show a) => BBGr (Analysis a) -> String
showAnalysedBBGr = showBBGr . nmap strip
  where
    strip = map (fmap insLabel)

-- | Pick out and show the basic block graphs in the program file analysis.
showBBlocks :: (Out a, Show a) => ProgramFile (Analysis a) -> String
showBBlocks (ProgramFile cm_pus _) = (perPU . snd) =<< cm_pus
  where
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ showBBGr (nmap strip gr) ++ "\n\n"
      where p = "| Program Unit " ++ show (getName pu) ++ " |"
            dashes = replicate (length p) '-'
    perPU _ = ""
    strip = map (fmap insLabel)

--------------------------------------------------
-- Some helper functions that really should be in fgl.

-- | Fold a function over the graph. Monadically.
ufoldM' :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
ufoldM' f u g
  | isEmpty g = return u
  | otherwise = f c =<< (ufoldM' f u g')
  where
    (c,g') = matchAny g

-- | Map a function over the graph. Monadically.
gmapM' :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM' f = ufoldM' (\ c g -> f c >>= \ c' -> return (c' & g)) empty

-- | Map a function over the 'Node' labels in a graph. Monadically.
nmapM' :: (DynGraph gr, Monad m) => (a -> m c) -> gr a b -> m (gr c b)
nmapM' f = gmapM' (\ (p,v,l,s) -> f l >>= \ l' -> return (p,v,l',s))

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
