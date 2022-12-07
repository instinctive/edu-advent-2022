module Main where

import Prelude hiding ( readSigned )

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
import Data.Text     ( Text     )

import qualified Data.IntMap    as IM
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = readArray a i >>= writeArray a i . f

readSigned :: Integral a => Text -> a
readSigned t = case T.signed T.decimal t of Right (i,_) -> i
