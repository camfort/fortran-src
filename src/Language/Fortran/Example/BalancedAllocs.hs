{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.Example.BalancedAllocs where

import qualified Language.Fortran.Parser as F
import qualified Language.Fortran.AST as F

import Data.Generics.Uniplate.Operations
import Data.Function ( on )
import qualified Data.List as L

data Report = Report
  { reportUnbalancedAllocs :: [()]
  , reportOutOfOrder :: [()]
  } deriving stock Show

checkAllocs :: F.ProgramFile a -> Report
checkAllocs pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf

  let checkPU :: F.ProgramUnit a -> Report
      checkPU pu = Report {..}
        where
          reportUnbalancedAllocs = filter (not . isDealloced . fst) allocs
          reportOutOfOrder       = concat $ zipWith (\ v1 v2 -> if fst v1 == fst v2 then [] else [v1, v2]) (L.nubBy ((==) `on` fst) allocs) (L.nubBy ((==) `on` fst) $ reverse deallocs)
          allocs =
            [ (v, (F.getName pu, atSpannedInFile file e))
            | F.StAllocate _ _ _ (F.AList _ _ es) _ <-
                universeBi (F.programUnitBody pu) :: [F.Statement a]
            , e <- es
            , v <- take 1 [ v | F.ExpValue _ _ (F.ValVariable v) <-
                universeBi e :: [F.Expression a] ]
            ]
          deallocs =
            [ (v, (F.getName pu, atSpannedInFile file e))
            | F.StDeallocate _ _ (F.AList _ _ es) _ <-
                universeBi (F.programUnitBody pu) :: [F.Statement a]
            , e <- es
            , v <- take 1 [ v | F.ExpValue _ _ (F.ValVariable v) <-
                universeBi e :: [F.Expression a] ]
            ]
          isDealloced v = not . null $ filter ((==v) . fst) deallocs

  let reports = map checkPU (universeBi pf)

  mconcat reports
