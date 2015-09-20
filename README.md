# WaterFlow

## Problem 

Given a n x n grid of cells, with (possibly) differing heights, calculate from which of those cells
water can flow to each of the four edges of the grid. In this problem, water can only flow
horizontally or vertically from a cell with equal or greater height to the cell that the water
flows to.

## Examples

```
1 2  =>  0 1
2 1      1 0
```


```
1 2 3      0 0 0
8 9 4  =>  1 1 0
7 6 5      1 1 1
```

## Solution

Haskell; using a memoisation map to avoid repeated work - for each cell, recursively visit all
neighbours that can be visited (due to the height limitation) and determine which edges they can
reach. Any given coordinate can reach the edges its (valid) neighbours can reach, and the edges it
is on (i.e. each cell on the top row can immediately reach the top row).

## Running

Using the [stack](stack) tool, `stack build && stack exec WaterFlow` will build and run a bunch of
tests.

[stack]: https://github.com/commercialhaskell/stack
