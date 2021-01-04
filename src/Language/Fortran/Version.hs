{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran.Version
  ( FortranVersion(..)
  , fortranVersionAliases
  , selectFortranVersion
  ) where

import           Data.Char (toLower)
import           Data.List (isInfixOf, find)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data FortranVersion = Fortran66
                    | Fortran77
                    | Fortran77Extended
                    | Fortran77Legacy
                    | Fortran90
                    | Fortran95
                    | Fortran2003
                    | Fortran2008
                    deriving (Ord, Eq, Data, Typeable, Generic)

instance Show FortranVersion where
  show Fortran66         = "Fortran 66"
  show Fortran77         = "Fortran 77"
  show Fortran77Extended = "Fortran 77 Extended"
  show Fortran77Legacy   = "Fortran 77 Legacy"
  show Fortran90         = "Fortran 90"
  show Fortran95         = "Fortran 95"
  show Fortran2003       = "Fortran 2003"
  show Fortran2008       = "Fortran 2008"

fortranVersionAliases :: [(String, FortranVersion)]
fortranVersionAliases = [ ("66" , Fortran66)
                        , ("77e", Fortran77Extended)
                        , ("77l", Fortran77Legacy)
                        , ("77" , Fortran77)
                        , ("90" , Fortran90)
                        , ("95" , Fortran95)
                        , ("03" , Fortran2003)
                        , ("08" , Fortran2008) ]

selectFortranVersion :: String -> Maybe FortranVersion
selectFortranVersion alias = snd <$> find (\ entry -> fst entry `isInfixOf` map toLower alias) fortranVersionAliases
