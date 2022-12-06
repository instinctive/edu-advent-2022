# [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

[Here](https://github.com/instinctive/edu-advent-2022/blob/main/day06.md)
is a nice solution without the ST monad.

```haskell
#include "prelude.hs"
```

Parts one and two differ only in the window size: 4 and 14, respectively.

```haskell
main :: IO ()
main = getContents >>= void . both print
    . (solve 4 &&& solve 14)
    . filter isAlpha
```

Candidate message packets appear in a moving window as we scan the input.  As
the window moves new characters enter and old characters leave.  We'll
represent the sliding window as an array from characters to character counts.

```haskell
type Window s = STUArray s Char Int
```

When we slide the window over, we add the new character and remove the old.
When a window has no repeated characters, we've found a valid message packet.

```haskell
slide :: Window s -> Char -> Char -> ST s Bool
slide ary new old = do
    modifyArray ary old pred
    modifyArray ary new succ
    valid <$> unsafeFreeze ary
  where
    valid :: UArray Char Int -> Bool
    valid = allOf each (<=1)
```

We will fill the initial window with a dummy character. The first characters we
remove as the window slides along (the _prefix_) will be these dummy
characters.

We begin with _w_+1 dummies in the window, so that once we remove _w_ dummies
the packet detection will work correctly.

```haskell
solve :: Int -> String -> Int
solve w s = 
    runST $ do
        ary <- newArray (q,'z') 0 :: ST s (STUArray s Char Int)
        writeArray ary q (w+1)
        let loop ans (n:nn) (o:oo) = slide ary n o >>= bool
                (loop (ans+1) nn oo) -- not valid, continue
                (pure ans)           -- return the answer
        loop 1 new old
  where
    q = pred 'a'           -- dummy character
    new = s                -- new characters entering
    old = prefix <> s      -- old characters leaving
    prefix = replicate w q -- dummy characters to remove
```
