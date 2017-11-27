-- | Analyse a program file and create basic blocks.

{-# LANGUAGE FlexibleContexts, PatternGuards, ScopedTypeVariables #-}
module Language.Fortran.Analysis.BBlocks
  ( analyseBBlocks, genBBlockMap, showBBGr, showAnalysedBBGr, showBBlocks, bbgrToDOT, BBlockMap
  , genSuperBBGr, SuperBBGr, showSuperBBGr, superBBGrToDOT, superBBGrGraph, superBBGrClusters, superBBGrEntries
  , findLabeledBBlock )
where

import Data.Generics.Uniplate.Data
import Data.Data
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.PrettyPrint.GenericPretty (pretty, Out)
import Language.Fortran.Analysis
import Language.Fortran.AST
import Language.Fortran.Util.Position
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate)
import Data.Maybe

--------------------------------------------------

-- | Insert basic block graphs into each program unit's analysis
analyseBBlocks :: Data a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseBBlocks pf = evalState (analyse (analyseAllLhsVars pf)) 1
  where
    analyse = labelExprsInBBGr <=< labelBlocksInBBGr <=< return . trans toBBlocksPerPU <=< labelExprs <=< labelBlocks
    trans :: Data a => TransFunc ProgramUnit ProgramFile a
    trans = transformBi

-- | A mapping of program unit names to bblock graphs.
type BBlockMap a = M.Map ProgramUnitName (BBGr a)

-- | Create a mapping of (non-module) program unit names to their
-- associated bblock graph.
genBBlockMap :: Data a => ProgramFile (Analysis a) -> BBlockMap (Analysis a)
genBBlockMap pf = M.fromList [
    (puName pu, gr) | pu <- getPUs pf, Just gr <- [bBlocks (getAnnotation pu)]
  ]
  where
    getPUs :: Data a => ProgramFile (Analysis a) -> [ProgramUnit (Analysis a)]
    getPUs = universeBi

--------------------------------------------------

-- Insert unique labels on each AST-block for easier look-up later.
labelBlocks :: Data a => ProgramFile (Analysis a) -> State Int (ProgramFile (Analysis a))
labelBlocks gr = transform eachBlock gr
  where
    eachBlock :: Data a => Block (Analysis a) -> State Int (Block (Analysis a))
    eachBlock b = do
      n <- get
      put $ (n + 1)
      return . labelWithinBlocks $ setAnnotation ((getAnnotation b) { insLabel = Just n }) b
    transform :: Data a => TransFuncM (State Int) Block ProgramFile a
    transform = transformBiM

-- A version of labelBlocks that works on all AST-blocks inside of a
-- basic-block graph that have not already been labelled with
-- numbers. The reason that this function must exist is because
-- additional AST-blocks are generated within the process of creating
-- basic-block graphs, and must also be labelled.
labelBlocksInBBGr :: Data a => ProgramFile (Analysis a) -> State Int (ProgramFile (Analysis a))
labelBlocksInBBGr pf = transform (nmapM' (mapM eachBlock)) pf
  where
    eachBlock :: Data a => Block (Analysis a) -> State Int (Block (Analysis a))
    eachBlock b
      | a@Analysis { insLabel = Nothing } <- getAnnotation b = do
          n <- get
          put $ n + 1
          return . analyseAllLhsVars1 . labelWithinBlocks $ setAnnotation (a { insLabel = Just n }) b
      | otherwise = return . analyseAllLhsVars1 $ b
    transform :: Data a => (BBGr a -> State Int (BBGr a)) ->
                           ProgramFile a -> State Int (ProgramFile a)
    transform = transformBiM

-- Sets the label on each Index within a Block to match the Block, for
-- later look-up.
labelWithinBlocks :: forall a. Data a => Block (Analysis a) -> Block (Analysis a)
labelWithinBlocks = perBlock
  where
    perBlock :: Block (Analysis a) -> Block (Analysis a)
    perBlock b =
      case b of
        BlStatement a s e st               -> BlStatement a s (mfill i e) (fill i st)
        BlIf        a s e1 mn e2 bss el    -> BlIf        a s (mfill i e1) mn (mmfill i e2) bss el
        BlCase      a s e1 mn e2 is bss el -> BlCase      a s (mfill i e1) mn (fill i e2) (mmfill i is) bss el
        BlDo        a s e1 mn tl e2 bs el  -> BlDo        a s (mfill i e1) mn tl (mfill i e2) bs el
        BlDoWhile   a s e1 n tl e2 bs el   -> BlDoWhile   a s (mfill i e1) n tl (fill i e2) bs el
        _                             -> b
      where i = insLabel $ getAnnotation b

    mfill i  = fmap (fill i)
    mmfill i = fmap (fmap (fill i))

    fill :: forall f. (Data (f (Analysis a))) => Maybe Int -> f (Analysis a) -> f (Analysis a)
    fill Nothing  = id
    fill (Just i) = transform perIndex
      where
        transform :: (Index (Analysis a) -> Index (Analysis a)) -> f (Analysis a) -> f (Analysis a)
        transform = transformBi

        perIndex :: (Index (Analysis a) -> Index (Analysis a))
        perIndex x = setAnnotation ((getAnnotation x) { insLabel = Just i }) x

--------------------------------------------------

-- Insert unique labels on each expression for easier look-up later.
labelExprs :: Data a => ProgramFile (Analysis a) -> State Int (ProgramFile (Analysis a))
labelExprs gr = transform eachExpr gr
  where
    eachExpr :: Data a => Expression (Analysis a) -> State Int (Expression (Analysis a))
    eachExpr e = do
      n <- get
      put $ (n + 1)
      return $ setAnnotation ((getAnnotation e) { insLabel = Just n }) e
    transform :: Data a => TransFuncM (State Int) Expression ProgramFile a
    transform = transformBiM

-- A version of labelExprs that works on all expressions inside of a
-- basic-block graph that have not already been labelled with
-- numbers. The reason that this function must exist is because
-- additional expressions are generated within the process of creating
-- basic-block graphs, and must also be labelled.
labelExprsInBBGr :: Data a => ProgramFile (Analysis a) -> State Int (ProgramFile (Analysis a))
labelExprsInBBGr pf = transformBB (nmapM' (transformExpr eachExpr)) pf
  where
    eachExpr :: Data a => Expression (Analysis a) -> State Int (Expression (Analysis a))
    eachExpr e
      | a@Analysis { insLabel = Nothing } <- getAnnotation e = do
          n <- get
          put $ n + 1
          return $ setAnnotation (a { insLabel = Just n }) e
      | otherwise = return e
    transformBB :: Data a => (BBGr a -> State Int (BBGr a)) ->
                             ProgramFile a -> State Int (ProgramFile a)
    transformBB = transformBiM
    transformExpr :: Data a => (Expression (Analysis a) -> State Int (Expression (Analysis a))) ->
                               [Block (Analysis a)] -> State Int [Block (Analysis a)]
    transformExpr = transformBiM

--------------------------------------------------

-- Analyse each program unit
toBBlocksPerPU :: Data a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
toBBlocksPerPU pu
  | null bs   = pu
  | otherwise = pu'
  where
    bs  =
      case pu of
        PUMain _ _ _ bs _ -> bs;
        PUSubroutine _ _ _ _ _ bs _ -> bs;
        PUFunction _ _ _ _ _ _ _ bs _ -> bs
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
  | exit, PUFunction _ _ _ _ _ _ _ _ _ <- pu = zipWith genAssign (genVar a0 noSrcSpan fn:vs) [0..]
  | otherwise                                = zipWith genAssign vs [1..]
  where
    Named fn      = puName pu
    name i        = fn ++ "[" ++ show i ++ "]"
    a0            = head $ initAnalysis [prevAnnotation a]
    (a, s, vs)    = case pu of
      PUFunction _ _ _ _ _ (Just (AList a s vs)) _ _ _ -> (a, s, vs)
      PUSubroutine _ _ _ _ (Just (AList a s vs)) _ _   -> (a, s, vs)
      PUFunction a s _ _ _ Nothing _ _ _               -> (a, s, [])
      PUSubroutine a s _ _ Nothing _ _                 -> (a, s, [])
      _                                                -> (error "genInOutAssignments", error "genInOutAssignments", [])
    genAssign v i = analyseAllLhsVars1 $ BlStatement a0 s Nothing (StExpressionAssign a0 s vl vr)
      where
        (vl, vr) = if exit then (v', v) else (v, v')
        v'       = case v of
          ExpValue a' s (ValVariable _) -> genVar a0 s (name i)
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
  | BlStatement _ _ _ (StGotoAssigned _ _ _ ks)   <- last bs = map (lookupBBlock lm) (maybe [] aStrip ks)
  | BlStatement _ _ _ (StGotoComputed _ _ ks _)   <- last bs = map (lookupBBlock lm) (aStrip ks)
  | BlStatement _ _ _ (StReturn _ _ _)            <- last bs = [-1]
  | BlStatement _ _ _ (StIfArithmetic _ _ _ k1 k2 k3) <- last bs =
      [lookupBBlock lm k1, lookupBBlock lm k2, lookupBBlock lm k3]
examineFinalBlock _ _                                        = [-1]

-- True iff the final block in the list is an explicit control transfer.
isFinalBlockCtrlXfer bs@(_:_)
  | BlStatement _ _ _ (StGotoUnconditional {}) <- last bs = True
  | BlStatement _ _ _ (StGotoAssigned {})      <- last bs = True
  | BlStatement _ _ _ (StGotoComputed {})      <- last bs = True
  | BlStatement _ _ _ (StReturn {})            <- last bs = True
  | BlStatement _ _ _ (StIfArithmetic {})      <- last bs = True
isFinalBlockCtrlXfer _                                    = False

lookupBBlock lm (ExpValue _ _ (ValInteger l)) = (-1) `fromMaybe` M.lookup l lm
-- This occurs if a variable is being used for a label, e.g., from a Fortran 77 ASSIGN statement
lookupBBlock lm (ExpValue _ _ (ValVariable l)) = (-1) `fromMaybe` M.lookup l lm

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
                     , tempNums :: [Int]
                     , newEdges :: [LEdge ()] }

-- Initial state
bbs0 = BBS { bbGraph = empty, curBB = [], curNode = 1
           , labelMap = M.empty, nums = [2..], tempNums = [0..]
           , newEdges = [] }

-- Monad
type BBlocker a = State (BBState a)

-- Monad entry function.
execBBlocker :: BBlocker a b -> BBState a
execBBlocker = flip execState bbs0

--------------------------------------------------

-- Handle a list of blocks (typically from ProgramUnit or nested inside a BlDo, BlIf, etc).
processBlocks :: Data a => [Block (Analysis a)] -> BBlocker (Analysis a) (Node, Node)
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
perBlock :: Data a => Block (Analysis a) -> BBlocker (Analysis a) ()
-- invariant: curNode corresponds to curBB, and is not yet in the graph
-- invariant: curBB is in reverse order
perBlock b@(BlIf _ _ _ _ exps bss _) = do
  processLabel b
  exps' <- forM (map fromJust . filter isJust $ exps) processFunctionCalls
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

perBlock b@(BlCase _ _ _ _ _ inds bss _) = do
  processLabel b
  addToBBlock $ stripNestedBlocks b
  (selectN, _) <- closeBBlock

  -- go through nested AST-blocks
  startEnds <- forM bss $ \ bs -> do
    (caseN, endN) <- processBlocks bs
    genBBlock
    return (caseN, endN)

  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN   <- gets curNode
  let es  = startEnds >>= \ (caseN, endN) -> [(selectN, caseN, ()), (endN, nxtN, ())]
  -- if there is no "CASE DEFAULT"-statement then we need an edge from selectN -> nxtN
  createEdges $ if any isNothing inds then es else (selectN, nxtN, ()):es

perBlock b@(BlStatement a ss _ (StIfLogical _ _ exp stm)) = do
  processLabel b
  exp' <- processFunctionCalls exp
  addToBBlock $ stripNestedBlocks b

  -- start a bblock for the nested statement inside the If
  (ifN, thenN) <- closeBBlock

  -- build pseudo-AST-block to contain nested statement
  processBlocks [BlStatement a ss Nothing stm]
  endN <- gets curNode

  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN <- genBBlock
  createEdges [(ifN, thenN, ()), (ifN, nxtN, ()), (thenN, nxtN, ())]

perBlock b@(BlStatement _ _ _ (StIfArithmetic {})) =
  -- Treat an arithmetic if similarly to a goto
  processLabel b >> addToBBlock b >> closeBBlock_
perBlock b@(BlDo _ _ mlab _ _ (Just spec) bs _) = do
  let DoSpecification _ _ (StExpressionAssign _ _ _ e1) e2 me3 = spec
  e1'  <- processFunctionCalls e1
  e2'  <- processFunctionCalls e2
  me3' <- case me3 of Just e3 -> Just `fmap` processFunctionCalls e3; Nothing -> return Nothing
  perDoBlock Nothing b bs
perBlock b@(BlDo _ _ _ _ _ Nothing bs _) = perDoBlock Nothing b bs
perBlock b@(BlDoWhile _ _ _ _ _ exp bs _) = perDoBlock (Just exp) b bs
perBlock b@(BlStatement _ _ _ (StReturn {})) =
  processLabel b >> addToBBlock b >> closeBBlock_
perBlock b@(BlStatement _ _ _ (StGotoUnconditional {})) =
  processLabel b >> addToBBlock b >> closeBBlock_
perBlock b@(BlStatement a s l (StCall a' s' cn@(ExpValue _ _ _) Nothing)) = do
  (prevN, callN) <- closeBBlock
  -- put StCall in a bblock by itself
  addToBBlock b
  (_, nextN) <- closeBBlock
  createEdges [ (prevN, callN, ()), (callN, nextN, ()) ]
perBlock b@(BlStatement a s l (StCall a' s' cn@(ExpValue _ _ _) (Just aargs))) = do
  let exps = map extractExp . aStrip $ aargs
  (prevN, formalN) <- closeBBlock

  -- create bblock that assigns formal parameters (n[1], n[2], ...)
  case l of
    Just (ExpValue _ _ (ValInteger l)) -> insertLabel l formalN -- label goes here, if present
    _                                -> return ()
  let name i   = varName cn ++ "[" ++ show i ++ "]"
  let formal (ExpValue a s (ValVariable _)) i = ExpValue a s (ValVariable (name i))
      formal e i                              = ExpValue a s (ValVariable (name i))
        where a = getAnnotation e; s = getSpan e
  forM_ (zip exps [1..]) $ \ (e, i) -> do
    e' <- processFunctionCalls e
    addToBBlock . analyseAllLhsVars1 $ BlStatement a s Nothing (StExpressionAssign a' s' (formal e' i) e')
  (_, dummyCallN) <- closeBBlock

  -- create "dummy call" bblock with no parameters in the StCall AST-node.
  addToBBlock . analyseAllLhsVars1 $ BlStatement a s Nothing (StCall a' s' cn Nothing)
  (_, returnedN) <- closeBBlock

  -- re-assign the variables using the values of the formal parameters, if possible
  -- (because call-by-reference)
  forM_ (zip exps [1..]) $ \ (e, i) ->
    -- this is only possible for l-expressions
    if isLExpr e then
      addToBBlock . analyseAllLhsVars1 $ BlStatement a s Nothing (StExpressionAssign a' s' e (formal e i))
    else return ()
  (_, nextN) <- closeBBlock

  -- connect the bblocks
  createEdges [ (prevN, formalN, ()), (formalN, dummyCallN, ())
              , (dummyCallN, returnedN, ()), (returnedN, nextN, ()) ]

perBlock b = do
  processLabel b
  b' <- descendBiM processFunctionCalls b
  addToBBlock b'

--------------------------------------------------
-- helper monadic combinators

-- Do-block helper
perDoBlock :: Data a => Maybe (Expression (Analysis a)) -> Block (Analysis a) -> [Block (Analysis a)] -> BBlocker (Analysis a) ()
perDoBlock repeatExpr b bs = do
  (n, doN) <- closeBBlock
  case getLabel b of
    Just (ExpValue _ _ (ValInteger l)) -> insertLabel l doN
    _                                  -> return ()
  case repeatExpr of Just e -> processFunctionCalls e >> return (); Nothing -> return ()
  addToBBlock $ stripNestedBlocks b
  closeBBlock
  -- process nested bblocks inside of do-statement
  (startN, endN) <- processBlocks bs
  n' <- genBBlock
  -- connect all the new bblocks with edges, link to subsequent bblock labeled n'
  createEdges [(n, doN, ()), (doN, n', ()), (doN, startN, ()), (endN, doN, ())]

-- Maintains perBlock invariants while potentially starting a new
-- bblock in case of a label.
processLabel :: Block a -> BBlocker a ()
processLabel b | Just (ExpValue _ _ (ValInteger l)) <- getLabel b = do
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

genTemp :: String -> BBlocker a String
genTemp str = do
  n:ns <- gets tempNums
  modify $ \ s -> s { tempNums = ns }
  return $ "_" ++ str ++ "_t#" ++ show n

-- Strip nested code not necessary since it is duplicated in another
-- basic block.
stripNestedBlocks (BlDo a s l mn tl ds _ el)     = BlDo a s l mn tl ds [] el
stripNestedBlocks (BlDoWhile a s l tl n e _ el)  = BlDoWhile a s l tl n e [] el
stripNestedBlocks (BlIf a s l mn exps _ el)      = BlIf a s l mn exps [] el
stripNestedBlocks (BlCase a s l mn sc inds _ el) = BlCase a s l mn sc inds [] el
stripNestedBlocks (BlStatement a s l
                   (StIfLogical a' s' e _))      = BlStatement a s l (StIfLogical a' s' e (StEndif a' s' Nothing))
stripNestedBlocks b                              = b

-- Flatten out function calls within the expression, returning an
-- expression that replaces the original expression (probably becoming
-- a temporary variable).
processFunctionCalls :: Data a => Expression (Analysis a) -> BBlocker (Analysis a) (Expression (Analysis a))
processFunctionCalls = transformBiM processFunctionCall -- work bottom-up

-- Flatten out a single function call.
processFunctionCall :: Data a => Expression (Analysis a) -> BBlocker (Analysis a) (Expression (Analysis a))
-- precondition: there are no more nested function calls within the actual arguments
processFunctionCall (ExpFunctionCall a s fn@(ExpValue a' s' _) aargs) = do
  let a0 = head . initAnalysis $ [prevAnnotation a]
  (prevN, formalN) <- closeBBlock

  let exps = map extractExp (fromMaybe [] (aStrip <$> aargs))

  -- create bblock that assigns formal parameters (fn[1], fn[2], ...)
  let name i   = varName fn ++ "[" ++ show i ++ "]"
  let setName n e = setAnnotation ((getAnnotation e) { uniqueName = Just n, sourceName = Just n }) e
  let formal (ExpValue a s (ValVariable _)) i = setName n $ ExpValue a0 s (ValVariable n)
        where n = name i
      formal e i                              = setName n $ ExpValue a0 s (ValVariable n)
        where a = getAnnotation e; s = getSpan e; n = name i
  forM_ (zip exps [1..]) $ \ (e, i) -> do
    addToBBlock . analyseAllLhsVars1 $ BlStatement a0 s Nothing (StExpressionAssign a' s' (formal e i) e)
  (_, dummyCallN) <- closeBBlock

  let retV = setName (name 0) $ ExpValue a0 s (ValVariable (name 0))
  let dummyArgs = map (Argument a0 s' Nothing) (retV:map (uncurry formal) (zip exps [1..]))

  -- create "dummy call" bblock with dummy arguments in the StCall AST-node.
  addToBBlock . analyseAllLhsVars1 $ BlStatement a s Nothing (StCall a' s' fn (Just $ fromList a0 dummyArgs))
  (_, returnedN) <- closeBBlock

  -- re-assign the variables using the values of the formal parameters, if possible
  -- (because call-by-reference)
  forM_ (zip exps [1..]) $ \ (e, i) ->
    -- this is only possible for l-expressions
    if isLExpr e then
      addToBBlock . analyseAllLhsVars1 $ BlStatement a0 s Nothing (StExpressionAssign a' s' e (formal e i))
    else return ()
  tempName <- genTemp (varName fn)
  let temp = setName tempName $ ExpValue a0 s (ValVariable tempName)

  addToBBlock . analyseAllLhsVars1 $ BlStatement a0 s Nothing (StExpressionAssign a0 s' temp retV)
  (_, nextN) <- closeBBlock

  -- connect the bblocks
  createEdges [ (prevN, formalN, ()), (formalN, dummyCallN, ())
              , (dummyCallN, returnedN, ()), (returnedN, nextN, ()) ]
  return temp
processFunctionCall e = return e

extractExp (Argument _ _ _ exp) = exp

--------------------------------------------------
-- Supergraph: all program units in one basic-block graph

data SuperBBGr a = SuperBBGr { graph :: BBGr a
                             , clusters :: IM.IntMap ProgramUnitName
                             , entries :: M.Map PUName SuperNode }

-- | Extract graph from SuperBBGr
superBBGrGraph :: SuperBBGr a -> BBGr a
superBBGrGraph = graph

-- | Extract entry map from SuperBBGr
superBBGrEntries :: SuperBBGr a -> M.Map ProgramUnitName SuperNode
superBBGrEntries = entries

-- | Extract cluster map from SuperBBGr
superBBGrClusters :: SuperBBGr a -> IM.IntMap ProgramUnitName
superBBGrClusters = clusters

type SuperNode = Node
type SuperEdge = (SuperNode, SuperNode, ELabel)
type PUName = ProgramUnitName
type NLabel a = BB (Analysis a)
type ELabel = ()

genSuperBBGr :: forall a. Data a => BBlockMap (Analysis a) -> SuperBBGr (Analysis a)
genSuperBBGr bbm = SuperBBGr { graph = superGraph'', clusters = cmap, entries = entryMap }
  where
    namedNodes   :: [((PUName, Node), NLabel a)]
    namedNodes   = [ ((name, n), bs) | (name, gr) <- M.toList bbm, (n, bs) <- labNodes gr ]
    namedEdges   :: [((PUName, Node), (PUName, Node), ELabel)]
    namedEdges   = [ ((name, n), (name, m), l) | (name, gr) <- M.toList bbm, (n, m, l) <- labEdges gr ]
    superNodeMap :: M.Map (PUName, Node) SuperNode
    superNodeMap = M.fromList $ zip (map fst namedNodes) [1..]
    getSuperNode :: (PUName, Node) -> SuperNode
    getSuperNode = fromJustMsg "UNDEFINED SUPERNODE" . flip M.lookup superNodeMap
    superNodes   :: [(SuperNode, NLabel a)]
    superNodes   = [ (getSuperNode n, bs) | (n, bs) <- namedNodes ]
    superEdges   :: [(SuperNode, SuperNode, ELabel)]
    superEdges   = [ (getSuperNode n, getSuperNode m, l) | (n, m, l) <- namedEdges ]
    superGraph   :: Gr (NLabel a) ELabel
    superGraph   = mkGraph superNodes superEdges
    entryMap     :: M.Map PUName SuperNode
    entryMap     = M.fromList [ (name, n') | ((name, n), n') <- M.toList superNodeMap, n == 0  ]
    exitMap      :: M.Map PUName SuperNode
    exitMap      = M.fromList [ (name, n') | ((name, n), n') <- M.toList superNodeMap, n == -1 ]
    -- List of Calls and their corresponding SuperNode where they appear.
    -- Assumption: all StCalls appear by themselves in a bblock.
    stCalls      :: [(SuperNode, String)]
    stCalls      = [ (getSuperNode n, sub) | (n, [BlStatement _ _ _ (StCall _ _ e _)]) <- namedNodes
                                           , v@(ExpValue _ _ _)                        <- [e]
                                           , let sub = varName v
                                           , Named sub `M.member` entryMap && Named sub `M.member` exitMap ]
    stCallCtxts  :: [([SuperEdge], SuperNode, String, [SuperEdge])]
    stCallCtxts  = [ (inn superGraph n, n, sub, out superGraph n) | (n, sub) <- stCalls ]
    stCallEdges  :: [SuperEdge]
    stCallEdges  = concat [   [ (m, nEn, l) | (m, _, l) <- inEdges  ] ++
                              [ (nEx, m, l) | (_, m, l) <- outEdges ]
                          | (inEdges, _, sub, outEdges) <- stCallCtxts
                          , let nEn = fromJustMsg ("UNDEFINED: " ++ sub) (M.lookup (Named sub) entryMap)
                          , let nEx = fromJustMsg ("UNDEFINED: " ++ sub) (M.lookup (Named sub) exitMap) ]
    superGraph'  :: Gr (NLabel a) ELabel
    superGraph'  = insEdges stCallEdges . delNodes (map fst stCalls) $ superGraph
    cmap         :: IM.IntMap PUName -- SuperNode ==> PUName
    cmap         = IM.fromList [ (n, name) | ((name, _), n) <- M.toList superNodeMap ]
    mainEntry    :: SuperNode -- (possibly more than one, arbitrarily take first)
    mainEntry:_  = [ n | (n, _) <- labNodes superGraph', null (pre superGraph' n) ]
    -- Rename the main entry point to 0
    superGraph'' :: Gr (NLabel a) ELabel
    superGraph'' = delNode mainEntry .
                   insEdges [ (0, m, l) | (_, m, l) <- out superGraph' mainEntry ] .
                   insNode (0, []) $ superGraph'

fromJustMsg _ (Just x) = x
fromJustMsg msg _      = error msg

--------------------------------------------------

findLabeledBBlock :: String -> BBGr a -> Maybe Node
findLabeledBBlock lab gr =
  listToMaybe [ n | (n, bs) <- labNodes gr, b <- bs
                  , ExpValue _ _ (ValInteger lab') <- maybeToList (getLabel b)
                  , lab == lab' ]

-- | Show a basic block graph in a somewhat decent way.
showBBGr :: (Out a, Show a) => BBGr a -> String
showBBGr gr = execWriter . forM (labNodes gr) $ \ (n, bs) -> do
  let b = "BBLOCK " ++ show n ++ " -> " ++ show (map (\ (_, m, _) -> m) $ out gr n)
  tell $ "\n\n" ++ b
  tell $ "\n" ++ replicate (length b) '-' ++ "\n"
  tell (((++"\n") . pretty) =<< bs)

-- | Show a basic block graph without the clutter
showAnalysedBBGr :: (Out a, Show a) => BBGr (Analysis a) -> String
showAnalysedBBGr = showBBGr . nmap strip
  where
    strip = map (fmap insLabel)

-- | Show a basic block supergraph
showSuperBBGr :: (Out a, Show a) => SuperBBGr (Analysis a) -> String
showSuperBBGr = showAnalysedBBGr . graph

-- | Pick out and show the basic block graphs in the program file analysis.
showBBlocks :: (Data a, Out a, Show a) => ProgramFile (Analysis a) -> String
showBBlocks pf = perPU =<< getPUs pf
  where
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ showBBGr (nmap strip gr) ++ "\n\n"
      where p = "| Program Unit " ++ show (puName pu) ++ " |"
            dashes = replicate (length p) '-'
    perPU _ = ""
    strip = map (fmap insLabel)
    getPUs :: Data a => ProgramFile (Analysis a) -> [ProgramUnit (Analysis a)]
    getPUs = universeBi

-- | Output a graph in the GraphViz DOT format
bbgrToDOT :: BBGr a -> String
bbgrToDOT = bbgrToDOT' IM.empty

-- | Output a supergraph in the GraphViz DOT format
superBBGrToDOT :: SuperBBGr a -> String
superBBGrToDOT sgr = bbgrToDOT' (clusters sgr) (graph sgr)

-- shared code for DOT output
bbgrToDOT' :: IM.IntMap ProgramUnitName -> BBGr a -> String
bbgrToDOT' clusters gr = execWriter $ do
  tell "strict digraph {\n"
  tell "node [shape=box,fontname=\"Courier New\"]\n"
  let entryNodes = filter (\ n -> null (pre gr n)) (nodes gr)
  let nodes = bfsn entryNodes gr
  forM nodes $ \ n -> do
    let Just bs = lab gr n
    let mname = IM.lookup n clusters
    case mname of Just name -> do tell $ "subgraph \"cluster " ++ showPUName name ++ "\" {\n"
                                  tell $ "label=\"" ++ showPUName name ++ "\"\n"
                                  tell $ "fontname=\"Courier New\"\nfontsize=24\n"
                  _         -> return ()
    tell $ "bb" ++ show n ++ "[label=\"" ++ show n ++ "\\l" ++ (concatMap showBlock bs) ++ "\"]\n"
    when (null bs) . tell $ "bb" ++ show n ++ "[shape=circle]\n"
    tell $ "bb" ++ show n ++ " -> {"
    forM (suc gr n) $ \ m -> tell (" bb" ++ show m)
    tell "}\n"
    when (isJust mname) $ tell "}\n"
  tell "}\n"

showPUName (Named n) = n
showPUName (NamelessBlockData) = ".blockdata."
showPUName (NamelessMain) = ".main."

-- Some helper functions to output some pseudo-code for readability
showBlock (BlStatement _ _ mlab st)
    | null (str :: String) = ""
    | otherwise = showLab mlab ++ str ++ "\\l"
  where
    str =
      case st of
        StExpressionAssign _ _ e1 e2 -> showExpr e1 ++ " <- " ++ showExpr e2
        StIfLogical _ _ e1 _         -> "if " ++ showExpr e1
        StWrite _ _ _ (Just aexps)   -> "write " ++ aIntercalate ", " showExpr aexps
        StPrint _ _ _ (Just aexps)   -> "print " ++ aIntercalate ", " showExpr aexps
        StCall _ _ cn _              -> "call " ++ showExpr cn
        StDeclaration _ _ ty Nothing adecls ->
          showType ty ++ " " ++ aIntercalate ", " showDecl adecls
        StDeclaration _ _ ty (Just aattrs) adecls ->
          showType ty ++ " " ++
            aIntercalate ", " showAttr aattrs ++
            aIntercalate ", " showDecl adecls
        StDimension _ _ adecls       -> "dimension " ++ aIntercalate ", " showDecl adecls
        _                            -> ""
showBlock (BlIf _ _ mlab _ (Just e1:_) _ _) = showLab mlab ++ "if " ++ showExpr e1 ++ "\\l"
showBlock (BlDo _ _ mlab _ _ (Just spec) _ _) =
    showLab mlab ++ "do " ++ showExpr e1 ++ " <- " ++
      showExpr e2 ++ ", " ++
      showExpr e3 ++ ", " ++
      maybe "1" showExpr me4 ++ "\\l"
  where DoSpecification _ _ (StExpressionAssign _ _ e1 e2) e3 me4 = spec
showBlock (BlDo _ _ _ _ _ Nothing _ _) = "do"
showBlock _ = ""

showAttr (AttrParameter _ _) = "parameter"
showAttr (AttrPublic _ _) = "public"
showAttr (AttrPrivate _ _) = "private"
showAttr (AttrAllocatable _ _) = "allocatable"
showAttr (AttrDimension _ _ aDimDecs) =
  "dimension ( " ++ aIntercalate ", " showDim aDimDecs ++ " )"
showAttr (AttrExternal _ _) = "external"
showAttr (AttrIntent _ _ In) = "intent (in)"
showAttr (AttrIntent _ _ Out) = "intent (out)"
showAttr (AttrIntent _ _ InOut) = "intent (inout)"
showAttr (AttrIntrinsic _ _) = "intrinsic"
showAttr (AttrOptional _ _) = "optional"
showAttr (AttrPointer _ _) = "pointer"
showAttr (AttrSave _ _) = "save"
showAttr (AttrTarget _ _) = "target"

showLab Nothing = replicate 6 ' '
showLab (Just (ExpValue _ _ (ValInteger l))) = ' ':l ++ replicate (5 - length l) ' '

showValue (ValVariable v)       = v
showValue (ValIntrinsic v)      = v
showValue (ValInteger v)        = v
showValue (ValReal v)           = v
showValue (ValComplex e1 e2)    = "( " ++ showExpr e1 ++ " , " ++ showExpr e2 ++ " )"
showValue _                     = ""

showExpr (ExpValue _ _ v)         = showValue v
showExpr (ExpBinary _ _ op e1 e2) = "(" ++ showExpr e1 ++ showOp op ++ showExpr e2 ++ ")"
showExpr (ExpUnary _ _ op e)      = "(" ++ showUOp op ++ showExpr e ++ ")"
showExpr (ExpSubscript _ _ e1 aexps) = showExpr e1 ++ "[" ++
                                       aIntercalate ", " showIndex aexps ++ "]"
showExpr _                        = ""

showIndex (IxSingle _ _ _ i) = showExpr i
showIndex (IxRange _ _ l u s) =
  maybe "" showExpr l ++ -- Lower
  ':' : maybe "" showExpr u ++ -- Upper
  maybe "" (\u -> ':' : showExpr u) s -- Stride

showUOp Plus = "+"
showUOp Minus = "-"
showUOp Not = "!"

showOp Addition = " + "
showOp Multiplication = " * "
showOp Subtraction = " - "
showOp Division = " / "
showOp op = " ." ++ show op ++ ". "

showType (TypeSpec _ _ t (Just s)) = showBaseType t ++ "(selector)" -- ++ show s
showType (TypeSpec _ _ t Nothing)  = showBaseType t

showBaseType TypeInteger         = "integer"
showBaseType TypeReal            = "real"
showBaseType TypeDoublePrecision = "double"
showBaseType TypeComplex         = "complex"
showBaseType TypeDoubleComplex   = "doublecomplex"
showBaseType TypeLogical         = "logical"
showBaseType TypeCharacter       = "character"
showBaseType (TypeCustom s)      = s

showDecl (DeclArray _ _ e adims length initial) =
  showExpr e ++
    "(" ++ aIntercalate "," showDim adims ++ ")" ++
    maybe "" (\e -> "*" ++ showExpr e) length ++
    maybe "" (\e -> " = " ++ showExpr e) initial
showDecl (DeclVariable _ _ e length initial) =
  showExpr e ++
    maybe "" (\e -> "*" ++ showExpr e) length ++
    maybe "" (\e -> " = " ++ showExpr e) initial

showDim (DimensionDeclarator _ _ me1 me2) = maybe "" ((++":") . showExpr) me1 ++ maybe "" showExpr me2

aIntercalate sep f = intercalate sep . map f . aStrip

noSrcSpan = SrcSpan initPosition initPosition

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
