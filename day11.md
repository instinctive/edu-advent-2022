# [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)

Parts one and two have different output styles.

```haskell
#include "prelude.hs"

main :: IO ()
main = getContents >>= print . const 0
```
