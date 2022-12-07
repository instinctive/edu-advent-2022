# [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

```haskell
{-# LANGUAGE OverloadedStrings #-}
#include "prelude.hs"

main :: IO ()
main = T.getContents >>= void . both print . solve

solve :: Text -> (Int,Int)
solve = parse               -- parse the input
    >>> total               -- get the directory totals
    >>> sort                -- sort them in increasing order
    >>> partOne &&& partTwo -- answer both parts
```

For part one, we want the sum of all the directory sizes
that are at most ``100000``.

```haskell
partOne :: [Int] -> Int
partOne = sum . takeWhile (<=100000)
```

For part two, we want the smallest directory size which will bring the free
space to at least ``30000000``. The total storage is ``70000000``:
$$70000000 - \hbox{\it total in use} + \hbox{\it freed directory} \ge 30000000$$
$$\hbox{\it freed directory} \ge 30000000 - (70000000 - \hbox{\it total in use})$$
$$\hbox{\it freed directory} \ge \hbox{\it total in use} - 40000000$$
Note that the last element of the sorted sizes is the root, which is the _total
in use_.

```haskell
partTwo :: [Int] -> Int
partTwo sizes = fromJust $ find (>=needed) sizes where
    needed = last sizes - 40000000
```

## Parsing

We parse each line into a direct representation of its contents.
However, we don't actually need the directory or file names from
the listings, so we ignore them.

```haskell
data Line
    = CdTop       -- $ cd /
    | CdUp        -- $ cd ..
    | CdDown Text -- $ cd <name>
    | Ls          -- $ ls
    | LsDir       -- dir <name>
    | LsFile Int  -- <name> <sizes>
    deriving (Eq,Show)

parseLine :: Text -> Line
parseLine s = case T.words s of
    ["$","cd","/"]  -> CdTop
    ["$","cd",".."] -> CdUp
    ["$","cd",name] -> CdDown name
    ["$","ls"]      -> Ls
    ["dir",_]       -> LsDir
    [size,_]        -> LsFile $ readSigned size
```

The parser does some cleanup. We don't need the directory entries from the
input, and after throwing them away, we want to sum all the adjacent file sizes
to get a single value for that listing. Finally, we can throw away the ``ls``
commands, which were useful to separate sequential listings in the same
directory.

```haskell
parse :: Text -> [Line]
parse = T.lines           -- split into lines
    >>> map parseLine     -- parse each line
    >>> filter (/= LsDir) -- discard directory entries
    >>> sumFiles          -- sum sequential file sizes
    >>> filter (/= Ls)    -- discard ls commands

sumFiles :: [Line] -> [Line]
sumFiles = \case
    LsFile a : LsFile b : more -> sumFiles $ LsFile (a+b) : more
    line : more -> line : sumFiles more
    [] -> []
```

## Directories

We track directory sizes in a map from directory _paths_. I had a bug where I
used directory _names_, but the sly problem setter has repeated names in the
directory structure. Using paths from the root ensures uniqueness.

```haskell
type DirMap = Map [Text] Dir
```

Each directory tracks whether a listing has been seen, and its total size.

```haskell
data Dir = Dir
    { _dirSeen :: !Bool -- have we seen a directory listing yet?
    , _dirSize :: !Int  -- total size of this directory
    } deriving (Eq,Show)

zDir = Dir False 0      -- a new directory
```

For some reason, ``$(makeLenses ''Dir)`` doesn't work with my setup,
maybe because of the ``markdown-unlit`` pre-processor? So we create the lenses
by hand:

```haskell
dirSeen :: Lens' Dir Bool
dirSeen f (Dir b i) = f b <&> \b' -> Dir b' i

dirSize :: Lens' Dir Int
dirSize f (Dir b i) = f i <&> \i' -> Dir b i'
```

## Totaling the Directory Sizes

We process the input line-by-line, keeping track of the directory map in the
state monad. When we're done, we sort directory sizes in increasing order.

```haskell
total :: [Line] -> [Int]
total = foldM_ doLine []              -- do each line
    >>> flip execState M.empty        -- in the state monad
    >>> sort . map _dirSize . M.elems -- return the sizes
```

Finally, each line either moves us within the stack of directories,
or is the sum of file sizes for the current directory.

If the latter, and we haven't seen a listing from this directory yet, we add
the sizes to all directories in the stack. In either case, we mark this
directory as seen.

```haskell
doLine :: [Text] -> Line -> State DirMap [Text]
doLine stack = \case
    CdTop       -> pure $ ["/"]      -- go to the top level
    CdUp        -> pure $ tail stack -- pop the stack
    CdDown next -> pure $ next:stack -- push the next directory
    LsFile size -> do
        -- mark seen and return the prior "seen" status
        seen <- at stack . non zDir . dirSeen <<.= True
        -- if not previously seen, add size to stack directories
        when (not seen) $ for_ (tails stack) \path ->
            when (not . null $ path) $
                at path . non zDir . dirSize += size
        pure stack
```
