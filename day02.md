# [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= void . each print . solve

solve :: String -> V2 Int   -- contains parts one and two
solve = lines               -- split into lines
    >>> map (score . parse) -- score each round
    >>> sum                 -- add it all up
```

## Representing Rock Paper Scissors

Here ``Player`` is used to supply the phantom types ``'Us`` and ``'Them`` (via
the ``DataKinds`` extension).  We'll tag each move with the player, and the
outcomes with both players, using these
[phantom types](https://wiki.haskell.org/Phantom_type).

```haskell
data Player      = Us   | Them
data Move    a   = Rock | Paper | Scissors deriving (Eq,Ord,Show)
data Outcome a b = Loss | Tie   | Win      deriving (Eq,Ord,Show)
```

### Part One

An ``Outcome a b`` is the outcome from ``a``'s point of view.
For example,
$$\hbox{\tt getOutcome Paper Rock}\Rightarrow\hbox{\tt Win}$$
indicates that when paper fights rock, it wins.

This is the "truth table" for Rock Paper Scissors:

```haskell
getOutcome :: Move a -> Move b -> Outcome a b
getOutcome Paper    Paper    = Tie
getOutcome Paper    Rock     = Win
getOutcome Paper    Scissors = Loss
getOutcome Rock     Paper    = Loss
getOutcome Rock     Rock     = Tie
getOutcome Rock     Scissors = Win
getOutcome Scissors Paper    = Win
getOutcome Scissors Rock     = Loss
getOutcome Scissors Scissors = Tie
```

### Part Two

The move we choose will depend on the desired outcome and the move our opponent
chose. For example,
$$\hbox{\tt chooseMove Loss Scissors}\Rightarrow\hbox{\tt Paper}$$
indicates that to lose against scissors we must choose paper.

This is the same truth table, "inverted":

```haskell
chooseMove :: Outcome a b -> Move b -> Move a
chooseMove Loss Paper    = Rock
chooseMove Loss Rock     = Scissors
chooseMove Loss Scissors = Paper
chooseMove Tie  Paper    = Paper
chooseMove Tie  Rock     = Rock
chooseMove Tie  Scissors = Scissors
chooseMove Win  Paper    = Scissors
chooseMove Win  Rock     = Paper
chooseMove Win  Scissors = Rock
```

## Parsing and Scoring a Round

Parts one and two interpret the second letter of each round's input differently.
We capture each interpretation in the elements of the pair.

The ``Round`` type is the output of ``parse`` and the input to ``score``.
It is annotated with the player type information.

```haskell
type Round = (Move 'Them, (Move 'Us, Outcome 'Us 'Them))
```

### Scoring

Our score is based on the move we chose and the outcome of the round.  For each
of parts one and two, we are given only one of our two scoring features and
must compute the other.

```haskell
score :: Round -> V2 Int
score (them, (us1, out2)) = V2 one two where
    one  = moveValue us1 + outcomeValue out1
    two  = moveValue us2 + outcomeValue out2
    us2  = chooseMove out2 them
    out1 = getOutcome us1 them

moveValue :: Move 'Us -> Int
moveValue Rock     = 1
moveValue Paper    = 2
moveValue Scissors = 3

outcomeValue :: Outcome 'Us 'Them -> Int
outcomeValue Loss = 0
outcomeValue Tie  = 3
outcomeValue Win  = 6
```

### Parsing

For each round, the first letter of the input is the opponent's move. The
second letter is interpreted differently for each part: part one interprets it
as a move, and part two interprets it as the desired outcome from our point of
view.

```haskell
parse :: String -> Round
parse (words -> [first,second]) = (them, (us, out))
  where
    them = parseMove first
    us   = parseMove second
    out  = parseOutcome second

parseMove :: String -> Move a
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors

parseOutcome :: String -> Outcome a b
parseOutcome "X" = Loss
parseOutcome "Y" = Tie
parseOutcome "Z" = Win
```
