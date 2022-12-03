module Main where

import Control.Lens
import Control.Arrow   ( (>>>)   )
import Data.List.Split ( splitOn )

printBoth :: Show a => (a,a) -> IO ()
printBoth = void . both print

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]
