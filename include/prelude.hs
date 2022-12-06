module Main where

import Control.Lens
import Control.Monad.Extra
import Control.Monad.State
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.List.Split
import Linear.V2

import Data.IntMap   ( IntMap   )
import Data.Map      ( Map      )
import Data.Set      ( Set      )

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = readArray a i >>= writeArray a i . f
