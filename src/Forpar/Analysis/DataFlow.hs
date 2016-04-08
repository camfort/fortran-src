{-# LANGUAGE FlexibleContexts, PatternGuards, ScopedTypeVariables #-}
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
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree (Gr)
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

allVars :: (Data a, Data (b a)) => b a -> [Name]
allVars b = [ v | ExpValue _ _ (ValArray _ v)    <- uniBi b ] ++
            [ v | ExpValue _ _ (ValVariable _ v) <- uniBi b ]
  where
    uniBi :: (Data a, Data (b a)) => b a -> [Expression a]
    uniBi = universeBi

blockGen :: Data a => Block a -> [Name]
blockGen (BlStatement _ _ _ (StExpressionAssign _ _ lhs rhs))
  | ExpSubscript _ _ _ subs <- lhs = allVars rhs ++ allVars subs
  | otherwise                      = allVars rhs
blockGen (BlDo _ _ _ (DoSpecification _ _ (StExpressionAssign _ _ lhs rhs) e1 e2) _)
  | ExpSubscript _ _ _ subs <- lhs = allVars (rhs, e1, e2) ++ allVars subs
  | otherwise                      = allVars (rhs, e1, e2)
blockGen (BlStatement _ _ _ (StDeclaration {})) = []
blockGen (BlDoWhile _ _ e1 e2 _)   = allVars (e1, e2)
blockGen (BlIf _ _ e1 e2 _)        = allVars (e1, e2)
blockGen b                         = allVars b

blockKill :: Data a => Block a -> [Name]
blockKill (BlStatement _ _ _ st) = allLhsExprs st
blockKill (BlDo _ _ _ doSpec _)  = allLhsExprs doSpec
blockKill _                      = []

allLhsExprs b = [ v | ExpValue _ _ (ValArray _ v)    <- lhsExprs b ] ++
                [ v | ExpValue _ _ (ValVariable _ v) <- lhsExprs b ] ++
                [ v | ExpSubscript _ _ (ExpValue _ _ (ValArray _ v)) _ <- lhsExprs b ]

-- killGen gr = [ (n, (lvaKill n, lvaGen n)) | n <- nodes gr ]
--   where
--     lvaKill b = bblockKill (fromJust $ lab gr b)
--     lvaGen b  = bblockGen (fromJust $ lab gr b)

-- lhsExprsGr gr = [ (n, (concatMap lhsExprs (fromJust $ lab gr n))) | n <- nodes gr ]

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

rdBblockGenKill :: Data a => M.Map Name IS.IntSet -> [Block (Analysis a)] -> (IS.IntSet, IS.IntSet)
rdBblockGenKill dm bs = foldl' f (IS.empty, IS.empty) $ zip (map gen bs) (map kill bs)
  where
    gen b | null (allLhsExprs b) = IS.empty
          | otherwise            = IS.singleton . fromJust . insLabel . getAnnotation $ b
    kill = rdDefs dm
    f (bbgen, bbkill) (gen, kill) =
      ((bbgen IS.\\ kill) `IS.union` gen, (bbkill IS.\\ gen) `IS.union` kill)

rdDefs :: Data a => DefMap -> Block a -> IS.IntSet
rdDefs dm b = IS.unions [ IS.empty `fromMaybe` M.lookup y dm | y <- allLhsExprs b ]

genDefMap :: Data a => BlockMap a -> DefMap
genDefMap bm = M.fromListWith IS.union [
                 (y, IS.singleton i) | (i, b) <- IM.toList bm, y <- allLhsExprs b
               ]

genBlockMap :: Data a => ProgramFile (Analysis a) -> BlockMap a
genBlockMap pf = IM.fromList [ (i, b) | b <- universeBi pf, let Just i = insLabel (getAnnotation b) ]

genKill dm gr = [ (n, rdBblockGenKill dm (fromJust $ lab gr n)) | n <- nodes gr ]

--------------------------------------------------

backEdges domMap = filter isBackEdge . edges
  where
    isBackEdge (s, t) = t `IS.member` (fromJust $ s `IM.lookup` domMap)

-- For each loop, find out which nodes are in it by looking through
-- the backedges (m, n) where n is considered the 'loop-header',
-- delete n from the map, and then doing a reverse-depth-first
-- traversal starting from m to find all the nodes of
-- interest. Intersect this with the strongly-connected component
-- containing m, in case of 'improper' graphs with weird control
-- transfers.
loopNodes bedges gr = map (\ (m, n) -> n:intersect (scc' gr n) (rdfs [m] (delNode n gr))) bedges

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
                         ("postOrder",    show (postOrder gr))
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
                       -- , ("genKill",      show (genKill dm gr))
                       -- , ("lhsExprsGr", show (lhsExprsGr gr))
                       ] where
                           bedges = backEdges (dominators gr) gr
    perPU _ = ""
    lva = liveVariableAnalysis
    bm = genBlockMap pf
    dm = genDefMap bm
    rd = reachingDefinitions dm

--------------------------------------------------

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y     = y
  | otherwise = converge p ys
