# [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

```haskell
#include "include/prelude.hs"
```

This is a standard two-part problem:

``` haskell
main :: IO ()
main = getContents >>= printBoth . solve

solve :: String -> (Int,Int)
solve = lines                       -- split into lines
    >>> paragraphs                  -- group by paragraph
    >>> map (sum . map read)        -- read and sum values
    >>> sortBy (comparing Down)     -- sort descending
    >>> (,) . head <*> sum . take 3 -- return parts one and two
```
