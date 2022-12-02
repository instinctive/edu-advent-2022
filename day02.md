# [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

```haskell
#include "prelude.hs"
```

This is a standard two-part problem:

```haskell
main :: IO ()
main = getContents >>= printBoth . solve

solve :: String -> (Int,Int)
solve = lines                       -- split into lines
    >>> map (uncurry score . parse) -- score each round
    >>> sum                         -- add it all up
    >>> (,) <*> const 0             -- return parts one and two
```

Representing Rock Paper Scissors. We're going to go the extra mile and tag each
move with who the player is. We'll also tag the outcomes with the players.
We'll use the type system for this: each move and outcome will carry the
evidence in the type.

```haskell
data Player      = Us   | Them
data Move    a   = Rock | Paper | Scissors deriving (Eq,Ord,Show)
data Outcome a b = Loss | Tie   | Win      deriving (Eq,Ord,Show)
```

The result of ``play a b`` is the ``Outcome`` of ``a`` with respect to ``b``.
For example,

$${\tt play Paper Rock}\Rightarrow{\tt Win}$$

indicating that ``Paper`` wins over ``Rock``.

```haskell
outcome :: Move a -> Move b -> Outcome a b
outcome Rock     Scissors = Win
outcome Rock     Paper    = Loss
outcome Scissors Paper    = Win
outcome Scissors Rock     = Loss
outcome Paper    Rock     = Win
outcome Paper    Scissors = Loss
outcome _ _ = Tie
```

We need to parse the player moves from the input.

```haskell
parse :: String -> (Move 'Us, Move 'Them)
parse s = (parseMove us, parseMove them) where
    [them,us] = words s

parseMove :: String -> Move a
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors
parseMove e = error $ "invalid move: " <> show e
```

Our score is based on what we chose and the outcome of "us" playing "them".

```haskell
score :: Move 'Us -> Move 'Them -> Int
score us them = moveValue us + outcomeValue (outcome us them)

moveValue :: Move 'Us -> Int
moveValue Rock     = 1
moveValue Paper    = 2
moveValue Scissors = 3

outcomeValue :: Outcome 'Us 'Them -> Int
outcomeValue Loss = 0
outcomeValue Tie  = 3
outcomeValue Win  = 6
```
