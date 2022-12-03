# [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= void . each print . solve

solve :: String -> V2 Int
solve (lines -> ss) = V2 one two where        -- contains parts one and two
    one = sum $ map partOne ss                -- sum for each line
    two = sum $ map partTwo $ chunksOf 3 $ ss -- sum for each group of three
```

Item priorities:

```haskell
priority :: Char -> Int
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
priority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
```

## Part One

The score for each rucksack is the priority of the item that appears in the
intersection of the sets of items in each compartment.

```haskell
partOne :: String -> Int
partOne sack = priority answer where
    [answer] = S.elems $ S.intersection one two
    one = S.fromList $ take half sack
    two = S.fromList $ drop half sack
    half = length sack `div` 2
```

## Part Two

The score for each three-elf group is the priority of the item that appears
in the intersection of each elf's rucksack.

```haskell
partTwo :: [String] -> Int
partTwo sacks = priority answer where
    [answer] = S.elems
        $ foldl1' S.intersection 
        $ map S.fromList sacks
