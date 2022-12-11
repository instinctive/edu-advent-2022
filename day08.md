# [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8)

```haskell
{-# LANGUAGE OverloadedStrings #-}
#include "prelude.hs"

main :: IO ()
main = T.getContents >>= void . both print . solve . parse
  where solve = partOne &&& partTwo
```

## Parsing

We represent the woods as an array from coordinate points to heights.  The
parser reads the input and fills out the array. Each line of the input is a
_row_ and each character position is a _column_.

```haskell
type Pt = V2 Int -- V2 row column
type Woods = UArray Pt Char

parse :: Text -> Woods
parse = T.lines >>> listArray . mkBounds <*> mkValues
  where
    mkBounds tt = (V2 1 1, V2 nrows ncols) where
        nrows = length tt               -- number of rows
        ncols = T.length $ head tt      -- number of columns
    mkValues tt = concatMap T.unpack tt -- row-major order
```

## The Forest Interior

These problems involve values in the interior of the forest.

These values are computed by scanning each row and column in each direction.
That is, we scan over these lists of points:

- each row, west to east
- each row, east to west
- each column, north to south
- each column, south to north

```haskell
forestRanges :: Woods -> [[Pt]]
forestRanges w =
    let (V2 rlo clo, V2 rhi chi) = bounds w
        rows = [rlo..rhi]
        cols = [clo..chi]
        rowranges = [ (row `V2`) <$> cols | row <- rows ]
        colranges = [ (`V2` col) <$> rows | col <- cols ]
        rowranges' = reverse <$> rowranges
        colranges' = reverse <$> colranges
    in rowranges <> rowranges' <> colranges <> colranges'
```

Our scans will update an array that maps coordinate points to an integer.
The scan function takes the interior array and a list of points, and modifies
that interior array.

```haskell
type Ary s  = STUArray s Pt Int
type Scan s = forall s. Ary s -> [Pt] -> ST s ()
```

We initialize the interior array, do the scan, and return the array
limited to the interior points.

```haskell
getInterior :: Woods -> Int -> Scan s -> UArray Pt Int
getInterior w initial scanfn = limit $ runSTUArray do
    ary <- newArray (lo,hi) initial :: ST s (STUArray s Pt Int)
    traverse_ (scanfn ary) (forestRanges w)
    pure ary
  where
    (lo,hi) = bounds w
    limit = ixmap (lo+1,hi-1) id
```

## Part One

The answer to part one is the size of the border (which is always visible)
plus the visible trees of the interior.

A tree is visible if its height is greater than all the trees that preceed it
in the list. For every tree in the list of points, we ``check`` its height
against the ``shade`` (preceeding heights) up to that point.
$$\hbox{\tt shade [3,2,5,1]} \Rightarrow \hbox{\tt [3,3,5,5]}$$

We initialize the working array to 0, and if we can see the tree from any
direction, we set it to 1. When we're done, we sum the interior values to get
the total number of visible trees.

```haskell
partOne :: Woods -> Int
partOne w =
    border + interior
  where
    (V2 rlo clo, V2 rhi chi) = bounds w
    border = 2 * (rhi - rlo) + 2 * (chi - clo)
    height = (w!)                   -- height function
    shade = scanl1 max . map height -- preceeding heights
    interior = sum . elems $ getInterior w 0 \ary vv -> do
        let check v h = when (height v > h) $ writeArray ary v 1
        sequence_ $ zipWith check (tail vv) (shade vv)
```

## Part Two

For part two we determine the visibility from each tree in each direction.
Instead of doing each tree in sequence, we incrementally adjust the values
for each tree (by multiplying) as we scan across the forest.

As we scan each list of points, we track the closest tree of every height.
Initially, the edge of the forest serves as the closest, so we mark that
using the _first_ point in the list, which is on the border of the forest.

For each following point, we find the minimum distance to the closest
tree greater than or equal in height to the current tree.

```haskell
partTwo :: Woods -> Int
partTwo w = maximum . elems $ getInterior w 1 \ary (v0:vv) -> do
    pos <- newArray ('0','9') v0 :: ST s (STArray s Char Pt)        
    for_ vv \v -> do
        let h = w ! v
        let closest d h = do
                v' <- readArray pos h
                let d' = abs . sum $ v - v'
                pure $ min d d'
        d <- foldM closest maxBound [h..'9']
        modifyArray ary v (* d)
        writeArray pos h v
```
