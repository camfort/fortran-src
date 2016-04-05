{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Forpar.Analysis.BBlocks
  ( analyseBBlocks, showBBGr, showBBlocks )
where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Function
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
    gr  = deleteUnreachable . insExitEdges lm . insEntryEdges $ insEdges (newEdges bbs) (bbGraph bbs)
    pu' = setAnnotation ((getAnnotation pu) { bBlocks = Just gr }) pu
    lm  = labelMap bbs

insEntryEdges = insEdge (0, 1, ()) . insNode (0, []) -- for now assume only one entry

insExitEdges lm gr = flip insEdges (insNode (-1, []) gr) $ do
  n <- nodes gr
  guard $ null (out gr n)
  bs <- maybeToList $ lab gr n
  n' <- examineFinalBlock lm bs
  return (n, n', ())

-- find target of Goto statements (Return statements default target to -1)
examineFinalBlock lm bs@(_:_)
  | BlStatement _ _ _ (StGotoUnconditional _ _ k) <- last bs = [lookupBBlock lm k]
  | BlStatement _ _ _ (StGotoAssigned _ _ _ ks)   <- last bs = map (lookupBBlock lm) (aStrip ks)
  | BlStatement _ _ _ (StGotoComputed _ _ ks _)   <- last bs = map (lookupBBlock lm) (aStrip ks)
examineFinalBlock _ _ = [-1]

lookupBBlock lm (ExpValue _ _ (ValLabel l)) = (-1) `fromMaybe` M.lookup l lm

deleteUnreachable gr = subgraph (reachable 0 gr) gr

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

-- handle a list of blocks (typically from ProgramUnit or nested inside a BlDo, BlIf, etc).
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

-- handle an AST-block element
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
perBlock b = processLabel b >> addToBBlock b

--------------------------------------------------
-- helpers

-- do-block helper
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

-- maintains perBlock invariants while potentially starting a new bblock in case of a label
processLabel :: Block a -> BBlocker a ()
processLabel b | Just (ExpValue _ _ (ValLabel l)) <- getLabel b = do
  (n, n') <- closeBBlock
  insertLabel l n'
  createEdges [(n, n', ())]
processLabel _ = return ()

-- inserts into labelMap
insertLabel l n = modify $ \ st -> st { labelMap = M.insert l n (labelMap st) }

-- puts an AST block into the current bblock
addToBBlock :: Block a -> BBlocker a ()
addToBBlock b = modify $ \ st -> st { curBB = b:curBB st }

-- closes down the current bblock and opens a new one
closeBBlock :: BBlocker a (Node, Node)
closeBBlock = do
  n  <- gets curNode
  modify $ \ st -> st { bbGraph = insNode (n, reverse (curBB st)) (bbGraph st), curBB = [] }
  n' <- genBBlock
  return (n, n')
closeBBlock_ = closeBBlock >> return ()

-- starts up a new bblock
genBBlock :: BBlocker a Int
genBBlock = do
  n' <- gen
  modify $ \ st -> st { curNode = n', curBB = [] }
  return n'

-- adds labeled-edge mappings
createEdges es = modify $ \ st -> st { newEdges = es ++ newEdges st }

-- generates a new node number
gen :: BBlocker a Int
gen = do
  n:ns <- gets nums
  modify $ \ s -> s { nums = ns }
  return n

-- nested code not necessary since it is duplicated in another basic block
stripNestedBlocks (BlDo a s l ds _)         = BlDo a s l ds []
stripNestedBlocks (BlDoWhile a s l e _)     = BlDoWhile a s l e []
stripNestedBlocks (BlIf a s l exps _)       = BlIf a s l exps []
stripNestedBlocks (BlStatement a s l
                   (StIfLogical a' s' e _)) = BlStatement a s l (StIfLogical a' s' e (StEndif a' s'))
stripNestedBlocks b                         = b

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
