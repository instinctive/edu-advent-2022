# [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= void . both print . solve

solve :: String -> (Int,Int)
solve = lines
    >>> map ((partOne &&& partTwo) . parse)
    >>> bimap sum sum . unzip
```

## Parsing

```haskell
data Range = Range { _lo :: Int, _hi :: Int } deriving Show
type Pairing = (Range,Range)

parseRange :: String -> Range
parseRange s = Range lo hi where
    [lo,hi] = read <$> splitOn "-" s

parse :: String -> Pairing
parse s = (one,two) where
    [one,two] = parseRange <$> splitOn "," s
```

## Part One

Return ``1`` if one range is fully contained in the other, and ``0`` otherwise.

```haskell
partOne :: Pairing -> Int
partOne = bool 0 1 . test where
    test (a,b) = isWithin a b || isWithin b a
    isWithin a b =
        _lo a >= _lo b &&
        _hi a <= _hi b
```

## Part Two

Return ``1`` if one range overlaps with the other, and ``0`` otherwise.

```haskell
partTwo :: Pairing -> Int
partTwo = bool 0 1 . overlaps where
    overlaps (a,b) =
        _lo a <= _hi b && _hi a >= _lo b
```
