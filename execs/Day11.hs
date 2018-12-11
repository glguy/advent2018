{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/11>

This solution uses dynamic programming to efficiently compute
the area of an arbitrary square on the power-cell grid.

This efficiency is achieved using a summed-area table.
<https://en.wikipedia.org/wiki/Summed-area_table>

Once we've computed this table we're able compute the sum of
the elements contained in an arbitrary rectangle via addition
and subtraction of 4 elements.

-}
module Main (main) where

import           Advent        (getParsedLines, number)
import           Data.Ord      (comparing)
import           Data.List     (intercalate)
import           Data.Foldable (maximumBy)
import qualified Data.Array.Unboxed as A

type Coord = (Int, Int)
type Grid = A.UArray Coord Int

dim :: Int
dim = 300

-- | Print the answers to day 11
main :: IO ()
main =
  do [serial] <- getParsedLines 11 number
     let table = summedAreaTable (powerLevel serial)
     putStrLn (part1 table)
     putStrLn (part2 table)

-- | Compute the position of the largest-valued 3-square
part1 :: Grid -> String
part1 areas = intercalate "," (map show [x,y])
  where
    ((x,y),_) = maximumSquare areas [3]

-- | Compute the position and size of the largest-valued square on the grid
part2 :: Grid -> String
part2 areas = intercalate "," (map show [x,y,s])
  where
    ((x,y),s) = maximumSquare areas [1..dim]

-- | Compute power level of power cell given the cells coordinate
-- and the player's serial number.
powerLevel ::
  Int   {- ^ serial number   -} ->
  Coord {- ^ grid coordinate -} ->
  Int   {- ^ power level     -}
powerLevel serial (x, y) = (rackid * y + serial) * rackid `div` 100 `mod` 10 - 5
  where
    rackid = x+10


-- | Compute the coordinates and size of the square on the
-- given Grid that maximizes the sum of the contained power levels.
maximumSquare ::
  Grid         {- ^ summed area grid               -} ->
  [Int]        {- ^ candidate sizes                -} ->
  (Coord, Int) {- ^ coordinates and size of square -}
maximumSquare areas sizes =
  fst $ maximumBy (comparing snd)
  [ (((x, y), size), rectangleSum areas (x, y) size size)
  | size <- sizes
  , let u = dim-size+1, x <- [1..u], y <- [1..u]
  ]

-- | Compute the sum of the power levels contained in a rectangle
-- specified by its top-left corner coordinate, width, and height.
rectangleSum ::
  Grid  {- ^ summed area grid              -} ->
  Coord {- ^ top-left corner of rectangle  -} ->
  Int   {- ^ rectangle width               -} ->
  Int   {- ^ rectangle height              -} ->
  Int   {- ^ sum of power levels in region -}
rectangleSum areas (x, y) w h
  = areas A.! (x+w-1,y+h-1)
  - areas A.! (x  -1,y+h-1)
  - areas A.! (x+w-1,y  -1)
  + areas A.! (x  -1,y  -1)

summedAreaTable :: (Coord -> Int) -> Grid
summedAreaTable f = convert table -- convert to unboxed array
  where
  -- table is boxed to allow self-reference in definition
  table :: A.Array Coord Int
  table = A.array ((0,0),(dim,dim))
        $ concat [ [ ((0,z),0), ((z,0),0)] | z <- [0..dim]]
        ++
        [ ( (x,y)
          , f (x,y)
          + table A.! (x-1,y  )
          + table A.! (x  ,y-1)
          - table A.! (x-1,y-1)
          )
        | x <- [1..dim]
        , y <- [1..dim]
        ]

-- | Convert between different array representations.
convert :: (A.IArray a e, A.IArray a' e, A.Ix i) => a i e -> a' i e
convert a = A.array (A.bounds a) (A.assocs a)
