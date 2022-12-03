module Main where

import Control.Lens
import Linear.V2

import Control.Arrow   ( (>>>)   )
import Data.List.Split ( splitOn )

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]
