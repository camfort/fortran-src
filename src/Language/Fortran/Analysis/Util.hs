{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Language.Fortran.Analysis.Util where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Data.Generics.Uniplate.Operations

-- TODO fix the warning (this is bad somehow)
type UniFunc f g a =    Biplate (f (Analysis a)) (g (Analysis a))
                     => f (Analysis a)
                     -> [g (Analysis a)]

allProgramUnits :: UniFunc ProgramFile ProgramUnit a
allProgramUnits = universeBi

allDeclarators :: UniFunc ProgramFile Declarator a
allDeclarators = universeBi

allStatements :: UniFunc f Statement a
allStatements = universeBi

allExpressions :: UniFunc f Expression a
allExpressions = universeBi

isAttrDimension :: Attribute a -> Bool
isAttrDimension AttrDimension {} = True
isAttrDimension _                = False

isAttrParameter :: Attribute a -> Bool
isAttrParameter AttrParameter {} = True
isAttrParameter _                = False

isAttrExternal :: Attribute a -> Bool
isAttrExternal AttrExternal {} = True
isAttrExternal _               = False

isIxSingle :: Index a -> Bool
isIxSingle IxSingle {} = True
isIxSingle _           = False
