{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/11>

-}
module Main (main) where

import           Advent        (getParsedLines, number)
import           Data.Ord      (comparing)
import           Data.Foldable (maximumBy)
import qualified Data.Array.Unboxed as A

type Grid = A.Array (Int,Int) Int

-- | Print the answers to day 11
main :: IO ()
main =
  do [serial] <- getParsedLines 11 number
     let cells = mkCells serial
     print (part1 cells)
     print (part2 cells)

part1 :: Grid -> (Int, Int)
part1 cells = best $ A.assocs $ growSquares cells $ growSquares cells cells

part2 :: Grid -> (Int, Int, Int)
part2 cells = best [ ((x,y,n),e)
                   | (n,vs) <- zip [1..300] (iterate (growSquares cells) cells)
                   , ((x,y),e) <- A.assocs vs ]

best :: Ord b => [(a,b)] -> a
best = fst . maximumBy (comparing snd)

powerLevel :: Int -> Int -> Int -> Int
powerLevel serial x y = (rackid * y + serial) * rackid `div` 100 `mod` 10 - 5
  where rackid = x+10

mkCells :: Int -> Grid
mkCells serial =
  A.array ((1,1),(300,300))
  [ ((x,y), powerLevel serial x y) | x <- [1..300], y <- [1..300] ]

growSquares :: Grid -> Grid -> Grid
growSquares cells prev =
  A.array ((1,1),(n',n'))
  [ ((x,y), prev  A.! (x+1,y+1)
          + cells A.! (x,y)
          + sum [ cells A.! (x,y+z)
                + cells A.! (x+z,y)
                | z <- [1 .. h]
                ]
    )
  | x <- [1..n']
  , y <- [1..n'] ]

  where
    gridSize = fst . snd . A.bounds
    n' = gridSize prev  - 1
    h =  gridSize cells - gridSize prev + 1
