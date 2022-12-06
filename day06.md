# [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

[Here](https://github.com/instinctive/edu-advent-2022/blob/tryhard/day06.md)
is a try-hard solution using the ST monad.

```haskell
#include "prelude.hs"
```

Parts one and two differ only in the window size: 4 and 14, respectively.

```haskell
main :: IO ()
main = getContents >>= void . both print . (solve 4 &&& solve 14)
```

The solution to the problem is the position of the first valid packet.

```haskell
solve w s =
    packets w s      -- find the packet status across the input
    & zip [1..]      -- label with their positions
    & find snd       -- first valid packet
    & fst . fromJust -- return the position
```

## Represention

Candidate message packets appear in a moving window as we scan the input.  As
the window moves new characters enter and old characters leave.  We'll
represent the sliding window as a map from characters to character counts.

```haskell
type Window = Map Char Int
```

## Sliding Window

When we slide the window over, we add the new character and remove the old. We
do this by updating the character counts.  To make this cleaner, we'll keep the
window in the state monad.

When a window has no repeated characters, we've found a valid message packet.

```haskell
slide :: (Char,Char) -> State Window Bool
slide (new,old) = do       -- return the previous count
    ix old %= pred         -- remove the old character
    ix new %= succ         -- add the new character
    get <&> not . any (>1) -- is this a valid packet?

```

## Initial Conditions

We will use spaces as a dummy character for the initial window. The first
characters we remove as the window slides along (the _prefix_) will be these
dummy spaces.

We begin with _n_+1 spaces in the window, so that once we remove _n_ spaces the
packet detection will work correctly.

_(Haskell note: We also initialize each alphabetic character to a 0-count, so
that the use of the ``ix`` lens in ``slide`` will work correctly.)_

```haskell
makeInitial :: Int -> (Window, String)
makeInitial n = (window,prefix) where
    prefix = replicate n ' '       -- prefix of n blanks
    window = blanks <> alphas      -- initial window
    blanks = M.singleton ' ' (n+1) -- n+1 blanks for window
    alphas = M.fromAscList         -- 0-count alphas
        $ zip ['a'..'z'] (repeat 0)
```

## Get Packet Status

To get the packet status, we slide the window over the input.  At each
position, ``slide`` will return ``True`` iff this packet is valid.

```haskell
packets :: Int -> String -> [Bool]
packets w s = 
    flip evalState window          -- update over the window
    $ traverse slide (zip new old) -- do each character
  where
    new = s           -- new characters entering
    old = prefix <> s -- old characters leaving
    (window,prefix) = makeInitial w
```
