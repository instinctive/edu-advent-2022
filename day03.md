# [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= print . solve

solve = lines
    >>> map score
    >>> sum
```

Item priorities:

```haskell
priority :: Char -> Int
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
priority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
```

The score for each rucksack is the priority of the item that appears in the
intersection of the sets of items in each compartment.

```haskell
score :: String -> Int
score raw = answer where
    [answer] = IS.elems $ IS.intersection one two
    one = IS.fromList $ take half cooked
    two = IS.fromList $ drop half cooked
    cooked = map priority raw
    half = length raw `div` 2
```
