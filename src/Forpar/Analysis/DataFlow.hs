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
import qualified Data.Set as S
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import Data.List (foldl', (\\))

--------------------------------------------------

dominators :: BBGr a -> [(Node, [Node])]
dominators = flip dom 0

iDominators :: BBGr a -> [(Node, Node)]
iDominators = flip iDom 0

type OrderF a = BBGr a -> [Node]

postOrder :: OrderF a
postOrder = postorder . head . dff [0]

revPostOrder :: OrderF a
revPostOrder = reverse . postOrder

--------------------------------------------------

type InOut t    = (S.Set t, S.Set t)
type InOutMap t = M.Map Node (InOut t)
type InF t      = Node -> S.Set t
type OutF t     = Node -> S.Set t

dataFlowSolver :: Ord t => BBGr a
                        -> (Node -> InOut t)
                        -> OrderF a
                        -> (OutF t -> InF t)
                        -> (InF t -> OutF t)
                        -> InOutMap t
dataFlowSolver gr initF order inF outF = converge (==) $ iterate step initM
  where
    ordNodes = order gr
    initM    = M.fromList [ (n, initF n) | n <- nodes gr ]
    step m   = M.fromList [ (n, (inF (snd . get m) n, outF (fst . get m) n)) | n <- ordNodes ]
    get m n  = fromJust $ M.lookup n m

--------------------------------------------------

liveVariableAnalysis :: Data a => BBGr a -> InOutMap Name
liveVariableAnalysis gr = dataFlowSolver gr (const (S.empty, S.empty)) postOrder inn out
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
    f (bbgen, bbkill) (gen, kill) = ((gen \\ bbkill) ++ bbgen, kill ++ bbkill)

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

killGen gr = [ (n, (lvaKill n, lvaGen n)) | n <- nodes gr ]
  where
    lvaKill b = bblockKill (fromJust $ lab gr b)
    lvaGen b  = bblockGen (fromJust $ lab gr b)

-- lhsExprsGr gr = [ (n, (concatMap lhsExprs (fromJust $ lab gr n))) | n <- nodes gr ]

--------------------------------------------------

showDataFlow :: (Data a, Out a, Show a) => ProgramFile (Analysis a) -> String
showDataFlow (ProgramFile cm_pus _) = (perPU . snd) =<< cm_pus
  where
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ dfStr gr ++ "\n\n"
      where p = "| Program Unit " ++ show (getName pu) ++ " |"
            dashes = replicate (length p) '-'
            dfStr gr = (\ (l, x) -> '\n':l ++ ": " ++ x) =<< [
                         ("postOrder",    show (postOrder gr))
                       , ("revPostOrder", show (revPostOrder gr))
                       , ("dominators",   show (dominators gr))
                       , ("iDominators",  show (iDominators gr))
                       , ("lva",          show (M.toList $ lva gr))
                       , ("killGen", show (killGen gr))
                       -- , ("lhsExprsGr", show (lhsExprsGr gr))
                       ]
    perPU _ = ""
    lva = liveVariableAnalysis

--------------------------------------------------

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y     = y
  | otherwise = converge p ys
