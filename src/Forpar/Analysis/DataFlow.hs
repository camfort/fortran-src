{-# LANGUAGE FlexibleContexts, PatternGuards, ScopedTypeVariables, TupleSections #-}
module Forpar.Analysis.DataFlow
  ( dominators, iDominators, postOrder, revPostOrder
  , dataFlowSolver, liveVariableAnalysis
  , showDataFlow )
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
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.TransClos
import Data.Maybe
import Data.List (foldl', (\\), union, delete, nub, intersect)

--------------------------------------------------

type DomMap = IM.IntMap IS.IntSet

dominators :: BBGr a -> DomMap
dominators = IM.fromList . map (fmap IS.fromList) . flip dom 0

type IDomMap = IM.IntMap Int

iDominators :: BBGr a -> IDomMap
iDominators = IM.fromList . flip iDom 0

type OrderF a = BBGr a -> [Node]

postOrder :: OrderF a
postOrder = postorder . head . dff [0]

revPostOrder :: OrderF a
revPostOrder = reverse . postOrder

preOrder :: OrderF a
preOrder = preorder . head . dff [0]

revPreOrder :: OrderF a
revPreOrder = reverse . preOrder

--------------------------------------------------

type CallGraph = M.Map ProgramUnitName (S.Set Name)

makeCallGraph :: Data a => ProgramFile a -> CallGraph
makeCallGraph pf = flip execState M.empty $ do
  let (ProgramFile cm_pus _) = pf
  forM_ cm_pus $ \ (_, pu) -> do
    let n = getName pu
    let uS :: Data a => ProgramUnit a -> [Statement a]
        uS = universeBi
    let uE :: Data a => ProgramUnit a -> [Expression a]
        uE = universeBi
    m <- get
    let ns = [ n' | StCall _ _ (ExpValue _ _ (ValSubroutineName n')) _ <- uS pu ] ++
             [ n' | ExpFunctionCall _ _ (ExpValue _ _ (ValFunctionName n')) _ <- uE pu ]
    put $ M.insert n (S.fromList ns) m

--------------------------------------------------

type InOut t    = (t, t)
type InOutMap t = M.Map Node (InOut t)
type InF t      = Node -> t
type OutF t     = Node -> t

dataFlowSolver :: Ord t => BBGr a
                        -> (Node -> InOut t)
                        -> OrderF a
                        -> (OutF t -> InF t)
                        -> (InF t -> OutF t)
                        -> InOutMap t
dataFlowSolver gr initF order inF outF = converge (==) $ iterate step initM
  where
    ordNodes = order gr
    initM    = M.fromList [ (n, initF n) | n <- ordNodes ]
    step m   = M.fromList [ (n, (inF (snd . get m) n, outF (fst . get m) n)) | n <- ordNodes ]
    get m n  = fromJust $ M.lookup n m

--------------------------------------------------

-- set of names found in an AST node
allVars :: (Data a, Data (b a)) => b a -> [Name]
allVars b = [ v | ExpValue _ _ (ValArray _ v)    <- uniBi b ] ++
            [ v | ExpValue _ _ (ValVariable _ v) <- uniBi b ]
  where
    uniBi :: (Data a, Data (b a)) => b a -> [Expression a]
    uniBi = universeBi

-- set of names found in the parts of an AST that are the target of an
-- assignment statement
allLhsVars :: (Data a, Data (b a)) => b a -> [Name]
allLhsVars b = [ v | ExpValue _ _ (ValArray _ v)    <- lhsExprs b ] ++
               [ v | ExpValue _ _ (ValVariable _ v) <- lhsExprs b ] ++
               [ v | ExpSubscript _ _ (ExpValue _ _ (ValArray _ v)) _ <- lhsExprs b ]

-- set of names used -- not defined -- by an AST-block
blockVarUses :: Data a => Block a -> [Name]
blockVarUses (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars subs
  | otherwise                      = allVars rhs
blockVarUses (BlDo _ _ _ (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2) _)
  | ExpSubscript _ _ _ subs <- lhs = allVars (rhs, e1, e2) ++ allVars subs
  | otherwise                      = allVars (rhs, e1, e2)
blockVarUses (BlStatement _ _ _ (StDeclaration {})) = []
blockVarUses (BlDoWhile _ _ e1 e2 _)   = allVars (e1, e2)
blockVarUses (BlIf _ _ e1 e2 _)        = allVars (e1, e2)
blockVarUses b                         = allVars b

-- set of names defined by an AST-block
blockVarDefs :: Data a => Block a -> [Name]
blockVarDefs (BlStatement _ _ _ st) = allLhsVars st
blockVarDefs (BlDo _ _ _ doSpec _)  = allLhsVars doSpec
blockVarDefs _                      = []

--------------------------------------------------

-- Muchnick, p. 445: A variable is "live" at a particular program
-- point if there is a path to the exit along which its value may be
-- used before it is redefined. It is "dead" if there is no such path.
liveVariableAnalysis :: Data a => BBGr a -> InOutMap (S.Set Name)
liveVariableAnalysis gr = dataFlowSolver gr (const (S.empty, S.empty)) revPreOrder inn out
  where
    inn outF b = (outF b S.\\ kill b) `S.union` gen b
    out innF b = S.unions [ innF s | s <- suc gr b ]
    kill b     = bblockKill (fromJust $ lab gr b)
    gen b      = bblockGen (fromJust $ lab gr b)

bblockKill :: Data a => [Block a] -> S.Set Name
bblockKill = S.fromList . concatMap blockKill

bblockGen :: Data a => [Block a] -> S.Set Name
bblockGen bs = S.fromList . fst . foldl' f ([], []) $ zip (map blockGen bs) (map blockKill bs)
  where
    f (bbgen, bbkill) (gen, kill) = ((gen \\ bbkill) `union` bbgen, kill `union` bbkill)

blockGen :: Data a => Block a -> [Name]
blockGen = blockVarUses
blockKill :: Data a => Block a -> [Name]
blockKill = blockVarDefs

--------------------------------------------------

-- Reaching Definitions
-- forward flow analysis (revPostOrder)

-- GEN b@( definition of anything ) = {b}
-- KILL b@( definition of y ) = DEFS y    -- technically, except b, but it won't matter
-- DEFS y = { all definitions of y }

-- Within a basic block
-- GEN [] = KILL [] = {}
-- GEN [b_1 .. b_{n+1}] = GEN b_{n+1} `union` (GEN [b_1 .. b_n] `difference` KILL b_{n+1})
-- KILL [b_1 .. b_{n+1}] = KILL b_{n+1} `union` (KILL [b_1 .. b_n] `difference` GEN b_{n+1})

-- Between basic blocks
-- REACHin bb = unions [ REACHout bb | bb <- pred bb ]
-- REACHout bb = GEN bb `union` (REACHin bb `difference` KILL bb)

type BlockMap a = IM.IntMap (Block (Analysis a))
type DefMap = M.Map Name IS.IntSet

reachingDefinitions :: Data a => DefMap -> BBGr (Analysis a) -> InOutMap IS.IntSet
reachingDefinitions dm gr = dataFlowSolver gr (const (IS.empty, IS.empty)) revPostOrder inn out
  where
    inn outF b = IS.unions [ outF s | s <- pre gr b ]
    out innF b = gen `IS.union` (innF b IS.\\ kill)
      where (gen, kill) = rdBblockGenKill dm (fromJust $ lab gr b)

rdBblockGenKill :: Data a => DefMap -> [Block (Analysis a)] -> (IS.IntSet, IS.IntSet)
rdBblockGenKill dm bs = foldl' f (IS.empty, IS.empty) $ zip (map gen bs) (map kill bs)
  where
    gen b | null (allLhsVars b) = IS.empty
          | otherwise            = IS.singleton . fromJust . insLabel . getAnnotation $ b
    kill = rdDefs dm
    f (bbgen, bbkill) (gen, kill) =
      ((bbgen IS.\\ kill) `IS.union` gen, (bbkill IS.\\ gen) `IS.union` kill)

-- set of all instruction labels that also define variables defined by AST-block b
rdDefs :: Data a => DefMap -> Block a -> IS.IntSet
rdDefs dm b = IS.unions [ IS.empty `fromMaybe` M.lookup y dm | y <- allLhsVars b ]

genDefMap :: Data a => BlockMap a -> DefMap
genDefMap bm = M.fromListWith IS.union [
                 (y, IS.singleton i) | (i, b) <- IM.toList bm, y <- allLhsVars b
               ]

genBlockMap :: Data a => ProgramFile (Analysis a) -> BlockMap a
genBlockMap pf = IM.fromList [ (i, b) | b <- universeBi pf, let Just i = insLabel (getAnnotation b) ]

genKill dm gr = [ (n, rdBblockGenKill dm (fromJust $ lab gr n)) | n <- nodes gr ]

--------------------------------------------------

-- DUMap : insLabel -> { insLabel }
--       : definition -> uses
type DUMap = IM.IntMap IS.IntSet

-- def-use map: map instruction labels of defining AST-blocks to the
-- AST-blocks that may use the definition.
genDUMap :: Data a => BlockMap a -> DefMap -> BBGr (Analysis a) -> InOutMap IS.IntSet -> DUMap
genDUMap bm dm gr rdefs = IM.unionsWith IS.union duMaps
  where
    -- duMaps for each bblock
    duMaps = [ fst (foldl' inBBlock (IM.empty, is) bs) |
               (n, (is, _)) <- M.toList rdefs,
               let Just bs = lab gr n ]
    -- internal analysis within bblock; fold over list of AST-blocks
    inBBlock (duMap, inSet) b = (duMap', inSet')
      where
        Just i = insLabel (getAnnotation b)
        bduMap = IM.fromListWith IS.union [ (i', IS.singleton i) | i' <- IS.toList inSet, overlap i' ]
        -- asks: does AST-block at label i' define anything used by AST-block b?
        overlap i' = not . null . intersect uses $ blockVarDefs b'
          where Just b' = IM.lookup i' bm
        uses   = blockVarUses b
        duMap' = IM.unionWith IS.union duMap bduMap
        gen b | null (allLhsVars b) = IS.empty
              | otherwise           = IS.singleton . fromJust . insLabel . getAnnotation $ b
        kill   = rdDefs dm
        inSet' = (inSet IS.\\ (kill b)) `IS.union` (gen b)

-- UDMap : insLabel -> { insLabel }
--       : use -> definitions
type UDMap = IM.IntMap IS.IntSet

duMapToUdMap :: DUMap -> UDMap
duMapToUdMap duMap = IM.fromListWith IS.union [
    (use, IS.singleton def) | (def, uses) <- IM.toList duMap, use <- IS.toList uses
  ]

-- use-def map: map instruction labels of variable-using AST-blocks to the
-- AST-blocks that define those variables.
genUDMap :: Data a => BlockMap a -> DefMap -> BBGr (Analysis a) -> InOutMap IS.IntSet -> UDMap
genUDMap bm dm gr = duMapToUdMap . genDUMap bm dm gr

--------------------------------------------------

mapToGraph :: DynGraph gr => IM.IntMap a -> IM.IntMap IS.IntSet -> gr a ()
mapToGraph bm m = buildGr [
    ([], i, l, jAdj) | (i, js) <- IM.toList m
                     , let Just l = IM.lookup i bm
                     , let jAdj = map ((),) $ IS.toList js
  ]

type FlowsGraph a = Gr (Block (Analysis a)) ()

flows :: Data a => BlockMap a -> DefMap -> BBGr (Analysis a) -> InOutMap IS.IntSet -> FlowsGraph a
flows bm dm gr = trc . mapToGraph bm . genDUMap bm dm gr

--------------------------------------------------

-- Find the edges that 'loop back' in the graph; ones where the target
-- node dominates the source node.
type BackEdgeMap = IM.IntMap Node
backEdges :: Graph gr => IM.IntMap IS.IntSet -> gr a b -> BackEdgeMap
backEdges domMap = IM.filterWithKey isBackEdge . IM.fromList . edges
  where
    isBackEdge s t = t `IS.member` (fromJust $ s `IM.lookup` domMap)

-- For each loop, find out which nodes are in it by looking through
-- the backedges (m, n) where n is considered the 'loop-header',
-- delete n from the map, and then doing a reverse-depth-first
-- traversal starting from m to find all the nodes of
-- interest. Intersect this with the strongly-connected component
-- containing m, in case of 'improper' graphs with weird control
-- transfers.
loopNodes :: Graph gr => BackEdgeMap -> gr a b -> [IS.IntSet]
loopNodes bedges gr = [
    IS.fromList (n:intersect (scc' gr n) (rdfs [m] (delNode n gr))) | (m, n) <- IM.toList bedges
  ]

scc' :: (Graph gr) => gr a b -> Node -> [Node]
scc' g n = head . filter (n `elem`) $ scc g

--------------------------------------------------

showDataFlow :: (Data a, Out a, Show a) => ProgramFile (Analysis a) -> String
showDataFlow pf@(ProgramFile cm_pus _) = (perPU . snd) =<< cm_pus
  where
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ dfStr gr ++ "\n\n"
      where p = "| Program Unit " ++ show (getName pu) ++ " |"
            dashes = replicate (length p) '-'
            dfStr gr = (\ (l, x) -> '\n':l ++ ": " ++ x) =<< [
                         ("callGraph",    show cg)
                       , ("postOrder",    show (postOrder gr))
                       , ("revPostOrder", show (revPostOrder gr))
                       , ("revPreOrder",  show (revPreOrder gr))
                       , ("dominators",   show (dominators gr))
                       , ("iDominators",  show (iDominators gr))
                       , ("lva",          show (M.toList $ lva gr))
                       , ("rd",           show (M.toList $ rd gr))
                       , ("backEdges",    show bedges)
                       , ("topsort",      show (topsort gr))
                       , ("scc ",         show (scc gr))
                       , ("loopNodes",    show (loopNodes bedges gr))
                       , ("duMap",        show (genDUMap bm dm gr (rd gr)))
                       , ("udMap",        show (genUDMap bm dm gr (rd gr)))
                       , ("flows",        show (edges $ flows bm dm gr (rd gr)))
                       ] where
                           bedges = backEdges (dominators gr) gr
    perPU _ = ""
    lva = liveVariableAnalysis
    bm = genBlockMap pf
    dm = genDefMap bm
    rd = reachingDefinitions dm
    cg = makeCallGraph pf

--------------------------------------------------

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y     = y
  | otherwise = converge p ys
