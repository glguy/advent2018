{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/6>

-}
module Main (main) where

import Advent
import Data.List
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)

far :: Int
far = 10000

-- | Print the answers to day 6
main :: IO ()
main =
  do input <- parseInput <$> getInput 6

     -- Compute the minimum and maximum values of the x and y coordinates
     let minx = minimum (map fst input)
         miny = minimum (map snd input)
         maxx = maximum (map fst input)
         maxy = maximum (map snd input)

     -- Compute all the coordinates within the min/max bounds as well as a
     -- box that is one larger all the way around
     let box0 = region (minx, miny) (maxx, maxy)
         box1 = region (minx+1, miny+1) (maxx+1, maxy+1)

     print (part1 input box0 box1)
     print (part2 input box0)


-- | Parse the input file as lines of comma delimited coordinates
parseInput :: [String] -> [Coord]
parseInput = map parseCoord

-- | Parse a comma delimited coordinate
--
-- >>> parseCoord "1, 2"
-- (1,2)
parseCoord :: String -> Coord
parseCoord str =
  case reads str of
    [(x, ',':ys)] -> (x,read ys)
    _ -> error ("parseCoord: " ++ str)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (x,y) (u,v) = abs (x-u) + abs (y-v)

-- | Compute the cardinal neighbors of a coordinate: north, south, east, west
neighbors :: Coord -> [Coord]
neighbors (x,y) = [ (x+1,y) , (x-1,y) , (x,y-1), (x,y+1)]

-- | Given two corners of a rectangle, generate all of the coordinates contained
-- in that rectangle.
region :: Coord -> Coord -> [Coord]
region (xlo, ylo) (xhi, yhi) = [(x, y) | x <- [xlo .. xhi], y <- [ylo .. yhi]]


-- | Part 1 looks for the largest completely closed region of coordinates
-- that are nearest to one of the input coordinates. We determine that a
-- region is closed by growing the considered region and eliminating
-- any regions that continue to grow. These still growing regions would
-- only grow larger and larger!
part1 ::
  [Coord] {- ^ input coordinates      -} ->
  [Coord] {- ^ enclosing region       -} ->
  [Coord] {- ^ slightly larger region -} ->
  Int     {- ^ solution               -}
part1 input box0 box1
  = maximum
  $ Map.mapMaybe id -- eliminate growing regions
  $ Map.intersectionWith match (regionSizes box0) (regionSizes box1)
  where
    regionSizes = cardinality . concatMap toRegion

    match x y
      | x == y    = Just x
      | otherwise = Nothing

    toRegion c
      | snd (choices !! 0) == snd (choices !! 1) = []
      | otherwise = map fst (take 1 choices)
      where
        choices =
          sortBy (comparing snd)
            [ (coord, manhattan c coord) | coord <- input ]


-- | Part 2 finds the size of the region with sum of distances less than 10,000
-- by knowing that the region must overlap at least partially with the bounding
-- rectangle. Because the region is defined by Manhattan distance the region
-- must be connected, so we search the bounding rectangle for a starting point.
-- Next we'll grow the region considering cardinal neighbors for any point that
-- is in bounds. Once we're unable to grow the region any further we return its
-- size.
part2 ::
  [Coord] {- ^ input coordinates -} ->
  [Coord] {- ^ enclosing region  -} ->
  Int
part2 input box0 = search Set.empty (Set.singleton startingPoint)

  where
    distances :: Coord -> Int
    distances (x,y) = sum (map (manhattan (x,y)) input)

    startingPoint = head [b | b <- box0, distances b < far]

    search seen next =
      case Set.minView next of
        Nothing -> Set.size seen
        Just (i, next')
          | Set.notMember i seen, distances i < far ->
              search (Set.insert i seen)
                     (foldl (flip Set.insert) next' (neighbors i))
          | otherwise -> search seen next'
