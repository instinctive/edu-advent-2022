# [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

In part one, the tail is ``1`` knot. In part two, it is ``9`` knots.

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= void . both print . (solve 1 &&& solve 9) . parse
```

We represent both knot positions and moves as a 2-dimensional point.

```haskell
type Pt = V2 Int
```

The input is parsed into a list of single moves.

```haskell
parse :: String -> [Pt]
parse = lines >>> concatMap \(words -> [dir,count]) ->
    replicate (read count) $ case dir of
        "R" -> V2   0    1  -- right
        "L" -> V2   0  (-1) -- left
        "D" -> V2   1    0  -- down
        "U" -> V2 (-1)   0  -- up
```

All knots of the rope begin at the zero point. We update the position of the
rope at every move, using the state monad to keep track of all the points
that the last tail knot has visited. The size of that set is the answer.

```haskell
solve :: Int -> [Pt] -> Int
solve n = let rope = replicate (n+1) 0 in -- the inital rope
    foldM update rope                     -- move the rope
    >>> flip execState S.empty            -- visited points
    >>> S.size                            -- the answer
```

To update the rope for a single move, we move the first knot, then every
successive knot ``follow``s the _moved_ previous knot, using the
self-referential ``zipWith``.

The position of the last knot is inserted into the set of visited points.

```haskell
update (first:rest) m = do
    modify (S.insert $ last rope) -- mark the tail as seen
    pure rope                     -- the moved rope
  where
    rope = first + m : zipWith follow rope rest
    follow h t
        | sum ((h - t)^2) < 4 = t           -- close: don't move
        | otherwise = t + fmap signum (h-t) -- far: move toward head
```
