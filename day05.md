# [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= void . both putStrLn . solve
```

Parts one and two differ in how they handle more than one crate being moved at
a time. In part one, they move individually, reversing their order, and in part
two they are moved all at once, keeping the same order.

```haskell
solve :: String -> (String,String)
solve = (run reverse &&& run id) . parse
```

## Representation

We'll represent the ship's cargo hold as a ``Ship``: a map from integers to
lists of characters, where an integer is the stack id and a character is a
crate.

A ``(Move`` _n from to_``)`` represents moving _n_ crates _from_ one stack _to_
another.

```haskell
type Ship = IntMap String
data Move = Move Int Int Int deriving Show
```

The crates are handled differently in parts one and two.

```haskell
type CrateHandler = String -> String
```

Actually doing a move means taking crates off one stack and putting them on
another. We'll put the ship itself behind the scenes in the ``State`` monad
and use the lens operators to work with it.

```haskell
move :: CrateHandler -> Move -> State Ship ()
move handler (Move n from to) = do
    cc <- ix from <<%= drop n
    ix to %= mappend (handler $ take n cc)
```

## Run the crane

To get the answer, we do every move and then return the crate at
the top of each stack.

```haskell
run :: CrateHandler -> (Ship,[Move]) -> String
run handler (ship,moves) =
    map head                         -- get the top crate
    $ IM.elems                       -- of each stack
    $ flip execState ship            -- from the ship
    $ traverse_ (move handler) moves -- after doing all the moves
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

```haskell
parseMoves :: [String] -> [Move]
parseMoves = map (go . words) where
    go [_,n,_,from,_,to] =
        Move (read n) (read from) (read to)
```
