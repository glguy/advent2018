{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/6>

-}
module Main (main) where

import Advent (getInput, cardinality)
import Data.List (groupBy, foldl', sort, sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)

-- | Magic number used in part 2 when computing the region that
-- isn't too far from all the points.
far :: Int
far = 10000

-- | Print the answers to day 6
main :: IO ()
main =
  do input <- parseInput <$> getInput 6
     print (part1 input)
     print (part2 input)


-- | Parse the input file as lines of comma delimited coordinates
parseInput :: [String] -> [Coord]
parseInput = map parseCoord

-- | Parse a comma delimited coordinate
--
-- >>> parseCoord "1, 2"
-- (1,2)
parseCoord :: String -> Coord
parseCoord str =
  case break (','==) str of
    (x,',':y) -> (read x,read y)
    _         -> error ("parseCoord: " ++ str)

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
  Int     {- ^ solution               -}
part1 input
  = maximum
  $ Map.mapMaybe id -- eliminate growing regions
  $ Map.intersectionWith match (regionSizes box0) (regionSizes box1)
  where
    regionSizes = cardinality . concatMap toRegion

    match x y
      | x == y    = Just x
      | otherwise = Nothing

    toRegion c =
      case choices of
        [(r,_)]:_ -> [r]
        _         -> []
      where
        choices = groupBy ((==)    `on` snd)
                $ sortBy  (compare `on` snd)
                  [ (coord, manhattan c coord) | coord <- input ]

    -- Compute the minimum and maximum values of the x and y coordinates
    minx = minimum (map fst input)
    miny = minimum (map snd input)
    maxx = maximum (map fst input)
    maxy = maximum (map snd input)

    -- Compute all the coordinates within the min/max bounds as well as a
    -- box that is one larger all the way around
    box0 = region (minx, miny) (maxx, maxy)
    box1 = region (minx+1, miny+1) (maxx+1, maxy+1)


-- | Part 2 finds the size of the region with sum of distances less than 10,000
-- by knowing that this region must contain the point found at the median of
-- all x and y coordinates (which is where the distance will be minimized.
-- Because the region is defined by Manhattan distance the region
-- must be connected, so we can find it by expanding this starting point.
-- Next we'll grow the region considering cardinal neighbors for any point that
-- is in bounds. Once we're unable to grow the region any further we return its
-- size.
part2 :: [Coord] -> Int
part2 input = search Set.empty (Set.singleton startingPoint)

  where
    distances :: Coord -> Int
    distances (x,y) = sum (map (manhattan (x,y)) input)

    startingPoint = (median (map fst input), median (map snd input))

    search seen next =
      case Set.minView next of
        Nothing -> Set.size seen
        Just (i, next')
          | Set.notMember i seen, distances i < far ->
              search (Set.insert i seen)
                     (foldl' (flip Set.insert) next' (neighbors i))
          | otherwise -> search seen next'

-- | Return the median element of a list. For even lists return the second
-- of the two middle elements.
--
-- >>> median [10,1,5]
-- 5
-- >>> median [1,3,4,5]
-- 4
-- >>> median [1,3,9,10,4,5]
-- 5
median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `quot` 2)
