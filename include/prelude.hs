module Main where

import Control.Lens
import Linear.V2

import Control.Arrow   ( (>>>) )
import Data.List.Split ( chunksOf, splitOn )

import qualified Data.IntSet as IS

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]
