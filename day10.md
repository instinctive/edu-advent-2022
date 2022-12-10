# [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)

Parts one and two have different output styles.

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= out . parse where
    out p = one p >> two p
    one = print    . partOne -- print the single value
    two = putStrLn . partTwo -- print the CRT display
```

## Parsing

The input is a sequence of commands that provide a _Δx_ value.
The ``noop`` takes one cycle and the ``addx`` two, so we turn
the ``addx`` into two _Δx_ values: ``0`` and the provided value.

We return as the parsing result the cumulative sums of these _Δx_ values.

_Note: The problem specifies that cycle-counting is 1-based. We add an extra
``0`` value at the front so we are working with a 0-based list of values._

```haskell
parse = lines
    >>> concatMap (go . words) -- Δx for every cycle
    >>> scanl (+) 1            -- cumulative sum over cycles
    >>> (0:)                   -- add a 0th-cycle element
  where
    go = \case
        ["noop"]   -> [0]        -- no change
        ["addx",v] -> [0,read v] -- two-cycles to change
```

## Part One

For part one we look up the cycle values at sample points.
$$\hbox{\it partOne} = \sum_{s\in samples} s*value(s)$$

```haskell
partOne :: [Int] -> Int
partOne values = sum [ s * (values !! s) | s <- samples ]

samples :: [Int]
samples = [ 20, 60, 100, 140, 180, 220 ]
```

## Part Two

The pixel position is a repeating scanline from ``0`` to ``39``.

```haskell
pixels :: [Int]
pixels = cycle [0..39] -- repeating scanline
```

The _x_ position determined by the input is a sprite that extends one position
to the left and right. The current pixel rendering depends on whether it falls
within the 3-pixel area covered by the sprite.

```haskell
render :: Int -> Int -> Char
render pixel sprite
    | abs (pixel - sprite) <= 1 = '#' -- within sprite
    | otherwise                 = '.' -- not within
```

For part two we render the first (1-based) 240 cycles. Each chunk of 40 pixels
is a scanline. We join the scanlines with ``\newline``s.

```haskell
partTwo :: [Int] -> String
partTwo = tail                -- drop 0th-cycle element
    >>> zipWith render pixels -- render all pixels
    >>> take 240              -- only keep the first 240
    >>> chunksOf 40           -- 40 pixels per scanline
    >>> unlines               -- insert \newline characters
```
