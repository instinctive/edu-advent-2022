# [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= putStrLn . run . parse
```

## Representation

We'll represent the ship's cargo hold as a ``Ship``: a map from integers to
lists of characters, where an integer is the stack id and a character is a
crate.

A ``Move`` will represent moving a _single_ crate from one stack to another.

```haskell
type Ship = IntMap String
type Move = (Int,Int)      -- (from,to)
```

Actually doing a move means taking a crate off one stack and putting it on
another. We'll put the ship itself behind the scenes in the ``State`` monad
and use the lens operators to work with it.

```haskell
move :: Move -> State Ship ()
move (from, to) =
    ix from <<%= tail >>= \(c:_) -> ix to %= (c:)
```

## Run the crane

To get the answer, we do every move and then return the crate at
the top of each stack.

```haskell
run :: (Ship,[Move]) -> String
run (ship,moves) = answer final where
    final = flip execState ship $ traverse_ move moves

answer = map top . IM.elems where
    top (c:_) = c
    top []    = ' '
```

## Parsing

This is a fun parsing problem, because it is layout-dependent.

We parse the input into a ``Ship`` and a list of ``Move``s.

```haskell
parse :: String -> (Ship,[Move])
parse = lines
    >>> paragraphs -- split at the blank line
    >>> assemble   -- parse each part
  where
    assemble [ship,moves] = (parseShip ship, parseMoves moves)
```

### Parsing the Ship

We'll build the ship's stacks from the bottom up.
A quick check of the input shows that there are some nice spaces inserted to
the end of each line, so if we add a space at the front of each line,
there will be a consistent pattern:

- ``␣[``_c_``]`` → crate _c_
- ``␣␣␣␣`` → no crate

The stack ids are in sorted order, so we can just fill them in after.

```haskell
parseShip :: [String] -> Ship
parseShip = reverse                -- build stacks from the bottom-up
    >>> tail                       -- discard the stack ids
    >>> map (chunksOf 4 . (' ':))  -- use four character chunks
    >>> transpose                  -- each "line" is now a complete stack
    >>> map (mapMaybe parseCargo)  -- only keep the crates
    >>> map reverse                -- make the stacks top-down
    >>> IM.fromAscList . zip [1..] -- label with the stack ids
  where
    parseCargo :: String -> Maybe Char
    parseCargo "    "          = Nothing
    parseCargo [' ','[',c,']'] = Just c
```

### Parsing the moves

Since the moves happen one at a time, an order like

    move 3 from 4 to 5

is the same as

    move 1 from 4 to 5
    move 1 from 4 to 5
    move 1 from 4 to 5

So we will expand each such move.

```haskell
parseMoves :: [String] -> [Move]
parseMoves = concatMap (go . words) where
    go [_,n,_,from,_,to] =
        replicate (read n) (read from, read to)
```
