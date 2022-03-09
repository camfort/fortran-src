{-# LANGUAGE DefaultSignatures #-}

module Language.Fortran.AST.Annotated where

import Language.Fortran.Util.FirstParameter

-- Retrieving SrcSpan and Annotation from nodes
class Annotated f where
  getAnnotation :: f a -> a
  setAnnotation :: a -> f a -> f a
  modifyAnnotation :: (a -> a) -> f a -> f a
  default getAnnotation :: (FirstParameter (f a) a) => f a -> a
  getAnnotation = getFirstParameter

  default setAnnotation :: (FirstParameter (f a) a) => a -> f a -> f a
  setAnnotation = setFirstParameter

  modifyAnnotation f x = setAnnotation (f (getAnnotation x)) x
