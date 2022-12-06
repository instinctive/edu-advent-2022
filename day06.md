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

To solve the problem, we slide the initial window over the incoming "new"
characters and the outgoing "old" characters.

```haskell
solve w s = runST $ do
    (window,prefix) <- makeInitial w -- initial window
    slideAll window s (prefix <> s)  -- slide window over input
```

## Representation

Candidate message packets appear in a moving window as we scan the input.  As
the window moves new characters enter and old characters leave.  We'll
represent the sliding window as an array from characters to character counts.

```haskell
type Window s = STUArray s Char Int
```

### Initial Conditions

We will fill the initial window with a dummy character. The first characters we
remove as the window slides along (the _prefix_) will be these dummy
characters.

We begin with _w_+1 dummies in the window, so that once we remove _w_ dummies
the packet detection will work correctly.

```haskell
makeInitial :: Int -> ST s (Window s, String)
makeInitial w = do
    ary <- newArray (q,'z') 0 :: ST s (STUArray s Char Int)
    writeArray ary q (w+1) -- start with w+1 dummy characters
    pure (ary,prefix)
  where
    q = pred 'a'           -- dummy character
    prefix = replicate w q -- dummy characters to remove
```

## Sliding Window

When we slide the window over, we add the new character and remove the old.
When a window has no repeated characters, we've found a valid message packet.

```haskell
slide :: Window s -> Char -> Char -> ST s Bool
slide ary new old = do
    modifyArray ary old pred   -- remove old
    modifyArray ary new succ   -- add new
    valid <$> unsafeFreeze ary -- check for valid window
  where
    valid :: UArray Char Int -> Bool
    valid = allOf each (<=1)
```

We slide the window until we find a valid packet, which we return.

```haskell
slideAll :: Window s -> String -> String -> ST s Int
slideAll w new old = loopM go (1,new,old) where
    go (ans,n:nn,o:oo) = 
        slide w n o <&> bool next done
      where
        next = Left (ans+1,nn,oo) -- continue to next input
        done = Right ans          -- return the answer
```
