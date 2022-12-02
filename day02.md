# [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

```haskell
#include "prelude.hs"
```

This is a standard two-part problem:

```haskell
main :: IO ()
main = getContents >>= printBoth . solve

solve :: String -> (Int,Int)
solve = lines               -- split into lines
    >>> map (score . parse) -- score each round
    >>> sum                 -- add it all up
    >>> (,) <*> const 0     -- return parts one and two
```

## Representing Rock Paper Scissors

We're going to go the extra mile and tag each move with the player. We'll also
tag the outcomes with the players.  We'll use the type system for this: each
move and outcome will carry the evidence in the type.

A ``Round`` is two moves played against each other.

```haskell
data Player      = Us   | Them
data Move    a   = Rock | Paper | Scissors deriving (Eq,Ord,Show)
data Outcome a b = Loss | Tie   | Win      deriving (Eq,Ord,Show)
type Round   a b = (Move a, Move b)
```

The outcome of a ``Round a b`` is an ``Outcome a b``, which is a loss, tie, or
win from ``a``'s point of view. For example,
$$\leftline{$\displaystyle\hbox{\strut\hskip 2em\tt outcome (Paper, Rock)}
  \Rightarrow
  \hbox{\tt Win}$}$$
indicates that ``Paper`` wins over ``Rock``.

```haskell
outcome :: Round a b -> Outcome a b
outcome = \case
    ( Rock     , Scissors ) -> Win
    ( Rock     , Paper    ) -> Loss
    ( Scissors , Paper    ) -> Win
    ( Scissors , Rock     ) -> Loss
    ( Paper    , Rock     ) -> Win
    ( Paper    , Scissors ) -> Loss
    _ -> Tie
```

## Scoring

Our score is based on what we chose and the outcome of the round.

```haskell
score :: Round 'Us 'Them -> Int
score round@(us,_) = moveValue us + outcomeValue (outcome round)

moveValue :: Move 'Us -> Int
moveValue Rock     = 1
moveValue Paper    = 2
moveValue Scissors = 3

outcomeValue :: Outcome 'Us 'Them -> Int
outcomeValue Loss = 0
outcomeValue Tie  = 3
outcomeValue Win  = 6
```

## Parsing

We need to parse the rounds from the input. Note that the input has the
opponent's move first. Two identical types with an arbitrary ordering can be a
nice source of bugs, hence the tagging of moves and outcomes with player type
information.

```haskell
parse :: String -> Round 'Us 'Them
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
