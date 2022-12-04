module Main where

import Control.Lens
import Linear.V2

import Data.List.Split ( chunksOf, splitOn )

import qualified Data.Set as S

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]
