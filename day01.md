# [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

This is a standard two-part problem:

``` haskell
#include "include/prelude.hs"
main :: IO ()
main = getContents >>= printBoth . solve
```

I ran into input data issues... something in my workflow left each line with an
extra ``\r``. Hence the "cleanup" filter, which is not a bad idea anyway.

``` haskell
solve :: String -> (Int,Int)
solve = lines                       -- split into lines
    >>> map (filter isDigit)        -- cleanup input data
    >>> paragraphs                  -- group by paragraph
    >>> map (sum . map read)        -- read and sum values
    >>> sortBy (comparing Down)     -- sort descending
    >>> (,) . head <*> sum . take 3 -- return parts one and two
```
