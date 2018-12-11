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

type Grid = A.UArray (Int,Int) Int

dim :: Int
dim = 300

-- | Print the answers to day 11
main :: IO ()
main =
  do [serial] <- getParsedLines 11 number
     let cells = mkCells serial
     let areas = summedArea cells
     print (part1 areas)
     print (part2 areas)

part1 :: Grid -> (Int, Int)
part1 areas = best (squaresOfSize areas 3)

part2 :: Grid -> (Int, Int, Int)
part2 areas = best [ ((x,y,sz),e) | sz <- [1..dim], ((x,y),e) <- squaresOfSize areas sz]

best :: Ord b => [(a,b)] -> a
best = fst . maximumBy (comparing snd)

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) = (rackid * y + serial) * rackid `div` 100 `mod` 10 - 5
  where rackid = x+10

mkCells :: Int -> Grid
mkCells serial = generate (1,1) (dim,dim) (powerLevel serial)

squaresOfSize :: Grid -> Int -> [((Int,Int),Int)]
squaresOfSize areas sz =
  [ ((x,y), squareSum areas x y sz)
  | x <- [1..dim-sz+1]
  , y <- [1..dim-sz+1]
  ]

squareSum :: Grid -> Int -> Int -> Int -> Int
squareSum areas x y sz
  = areas A.! (x+sz-1,y+sz-1)
  - areas A.! (x   -1,y+sz-1)
  - areas A.! (x+sz-1,y   -1)
  + areas A.! (x   -1,y   -1)

summedArea :: Grid -> Grid
summedArea elts = convert table -- convert to unboxed array
  where
  -- table is boxed to allow self-reference in definition
  table = A.array ((0,0),(dim,dim))
        $ concat [ [ ((0,z),0), ((z,0),0)] | z <- [0..dim]]
        ++
        [ ( (x,y)
          , elts  A.! (x,y)
          + table A.! (x-1,y)
          + table A.! (x,y-1)
          - table A.! (x-1,y-1)
          )
        | x <- [1..dim]
        , y <- [1..dim]
        ]

-- Array helpers

generate :: (A.IArray a e, A.Ix i) => i -> i -> (i -> e) -> a i e
generate lo hi f = A.array (lo,hi) [ (i, f i) | i <- A.range (lo, hi) ]

convert :: (A.IArray a e, A.Ix i) => A.Array i e -> a i e
convert a = A.array (A.bounds a) (A.assocs a)

