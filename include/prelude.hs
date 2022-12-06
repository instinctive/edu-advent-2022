module Main where

import Control.Lens
import Control.Monad.State
import Linear.V2

import Data.List.Split ( chunksOf, splitOn )

import Data.IntMap     ( IntMap )
import Data.Map        ( Map    )
import Data.Set        ( Set    )

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]
