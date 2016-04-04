{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Forpar.Analysis.BBlocks
  ( analyseBBlocks, showBBGr, showBBlocks )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.PrettyPrint.GenericPretty (pretty, Out)
import Forpar.Analysis
import Forpar.AST
import qualified Data.Map as M
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe

--------------------------------------------------

-- | Insert basic block graphs into each program unit's analysis
analyseBBlocks :: Show a => ProgramFile (Analysis a) -> ProgramFile (Analysis a)
analyseBBlocks (ProgramFile cm_pus cs) = ProgramFile (map (fmap toBBlocksPerPU) cm_pus) cs

toBBlocksPerPU :: Show a => ProgramUnit (Analysis a) -> ProgramUnit (Analysis a)
toBBlocksPerPU pu
  | null bs   = pu
  | otherwise = pu'
  where
    bs  = case pu of PUMain _ _ _ bs -> bs; PUSubroutine _ _ _ _ bs -> bs; PUFunction _ _ _ _ _ bs -> bs
                     _ -> []
    bbs = execBBlocker (processBlocks bs)
    gr  = insEdges (newEdges bbs) (bbGraph bbs)
    pu' = setAnnotation ((getAnnotation pu) { bBlocks = Just gr }) pu

--------------------------------------------------

data BBState a = BBS { bbGraph  :: BBGr a
                     , curBB    :: BB a
                     , curNode  :: Node
                     , labelMap :: M.Map String Node
                     , nums     :: [Int]
                     , newEdges :: [LEdge ()] }
type BBlocker a = State (BBState a)

bbs0 = BBS { bbGraph = empty, curBB = [], curNode = 1, labelMap = M.empty, nums = [2..], newEdges = [] }

execBBlocker :: BBlocker a b -> BBState a
execBBlocker = flip execState bbs0

processBlocks :: [Block a] -> BBlocker a (Node, Node)
-- precondition: curNode is not yet in the graph && will label the first block
-- postcondition: final bblock is in the graph labeled as endN && curNode == endN
-- returns start and end nodes for basic block graph corresponding to parameter bs
processBlocks bs = do
  startN <- gets curNode
  forM_ bs perBlock
  st@BBS { curBB = bb, bbGraph = gr, curNode = endN } <- get
  let gr' = insNode (endN, reverse bb) gr
  modify $ \ st -> st { curBB = [], bbGraph = gr' }
  return (startN, endN)

perBlock :: Block a -> BBlocker a ()
-- invariant: curNode corresponds to curBB, and is not yet in the graph
-- invariant: curBB is in reverse order
perBlock b@(BlIf _ _ _ exps bss) = do
  processLabel b
  st@BBS { curBB = bb, bbGraph = gr, curNode = ifN, labelMap = lm, newEdges = es } <- get
  -- close the previous bblock and save it, putting this If-statement at the end.
  let gr' = insNode (ifN, reverse (b:bb)) gr
  modify $ \ st -> st { bbGraph = gr', curBB = [] }

  -- go through nested AST-blocks
  startEnds <- forM bss $ \ bs -> do
    -- start a bblock for this nested AST-block
    thenN <- gen
    modify $ \ st -> st { curNode = thenN, curBB = [] }
    (_, endN) <- processBlocks bs
    return (thenN, endN)

  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN      <- gen
  let es    = startEnds >>= \ (thenN, endN) -> [(ifN, thenN, ()), (endN, nxtN, ())]
  -- if there is no "Else"-statement then we need an edge from ifN -> nxtN
  let es'   = if any isNothing exps then es else (ifN, nxtN, ()):es
  modify $ \ st -> st { newEdges = es' ++ newEdges st, curNode = nxtN, curBB = [] }
perBlock b@(BlStatement a ss _ (StIfLogical _ _ exp stm)) = do
  processLabel b
  st@BBS { curBB = bb, bbGraph = gr, curNode = ifN, labelMap = lm, newEdges = es } <- get
  -- close the previous bblock and save it, putting this If-statement at the end.
  let gr' = insNode (ifN, reverse (b:bb)) gr

  -- start a bblock for the nested statement inside the If
  thenN <- gen
  modify $ \ st -> st { curNode = thenN, curBB = [], bbGraph = gr' }

  -- build pseudo-AST-block to contain nested statement
  processBlocks [BlStatement a ss Nothing stm]

  st@BBS { bbGraph = gr, newEdges = es, curNode = endN } <- get
  -- connect all the new bblocks with edges, link to subsequent bblock labeled nxtN
  nxtN <- gen
  let es' = (ifN, thenN, ()):(ifN, nxtN, ()):(thenN, nxtN, ()):es
  modify $ \ st -> st { curNode = nxtN, curBB = [], newEdges = es' }
perBlock b@(BlStatement _ _ _ (StIfArithmetic {})) = error "BBlocks: StIfArithmetic unsupported"
perBlock b@(BlDo _ _ mlab spec bs) = perDoBlock b bs
perBlock b@(BlDoWhile _ _ mlab spec bs) = perDoBlock b bs
perBlock b = do
  processLabel b
  bb <- gets curBB
  let bb' = b:bb
  modify $ \ st -> st { curBB = bb' }

perDoBlock :: Block a -> [Block a] -> BBlocker a ()
perDoBlock b bs = do
  st@BBS { curBB = bb, bbGraph = gr, curNode = n, labelMap = lm, newEdges = es } <- get
  -- close the previous bblock and save it
  let gr' = insNode (n, reverse bb) gr

  -- now, create a new bblock to hold the conditional implicit in the do-statement
  doN <- gen
  let lm' = case getLabel b of Just (ExpValue _ _ (ValLabel l)) -> M.insert l doN lm; _ -> lm
  let gr'' = insNode (doN, [b]) gr'

  -- process nested bblocks inside of do-statement
  startN <- gen
  modify $ \ st -> st { curNode = startN, curBB = [], bbGraph = gr'', labelMap = lm' }
  processBlocks bs
  st@BBS { bbGraph = gr, newEdges = es, curNode = endN } <- get

  -- connect all the new bblocks with edges, link to subsequent bblock labeled n'
  n' <- gen
  let es' = (n, doN, ()):(doN, n', ()):(doN, startN, ()):(endN, doN, ()):es
  modify $ \ st -> st { curNode = n', curBB = [], newEdges = es' }

processLabel :: Block a -> BBlocker a ()
-- maintains perBlock invariants while potentially starting a new bblock in case of a label
processLabel b | Just (ExpValue _ _ (ValLabel l)) <- getLabel b = do
  st@BBS { curBB = bb, bbGraph = gr, curNode = n, labelMap = lm, newEdges = es } <- get
  n' <- gen
  let gr' = insNode (n, reverse bb) gr
  let es' = (n, n', ()):es
  let lm' = M.insert l n' lm
  modify $ \ st -> st { curNode = n', curBB = [], bbGraph = gr', labelMap = lm', newEdges = es' }
processLabel _ = return ()

gen :: BBlocker a Int
gen = do
  n:ns <- gets nums
  modify $ \ s -> s { nums = ns }
  return n

--------------------------------------------------

-- | Show a basic block graph in a somewhat decent way
showBBGr :: (Out a, Show a) => BBGr a -> String
showBBGr gr = execWriter . forM ns $ \ (n, bs) -> do
                let b = "BBLOCK " ++ show n ++ " -> " ++ show (map (\ (_, m, _) -> m) $ out gr n)
                tell $ "\n\n" ++ b
                tell $ "\n" ++ replicate (length b) '-' ++ "\n"
                tell (((++"\n") . pretty) =<< bs)
  where ns = labNodes gr

-- | Pick out and show the basic block graphs in the program file analysis
showBBlocks :: (Out a, Show a) => ProgramFile (Analysis a) -> String
showBBlocks (ProgramFile cm_pus _) = (perPU . snd) =<< cm_pus
  where
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ showBBGr (nmap strip gr) ++ "\n\n"
      where p = "| Program Unit " ++ show (getName pu) ++ " |"
            dashes = replicate (length p) '-'
    perPU _ = ""
    strip = map (fmap prevAnnotation)
