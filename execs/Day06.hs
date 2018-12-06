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
  do inp <- parseInput <$> getInput 6

     let minx = minimum (map fst inp)
         miny = minimum (map snd inp)
         maxx = maximum (map fst inp)
         maxy = maximum (map snd inp)
         box0 = region (minx, miny) (maxx, maxy)
         box1 = region (minx+1, miny+1) (maxx+1, maxy+1)

     print (part1 inp box0 box1)
     print (part2 inp box0)


parseInput :: [String] -> [Coord]
parseInput = map parseCoord

parseCoord :: String -> Coord
parseCoord str =
  case reads str of
    [(x, ',':ys)] -> (x,read ys)
    _ -> error ("parseCoord: " ++ str)

manhattan :: Coord -> Coord -> Int
manhattan (x,y) (u,v) = abs (x-u) + abs (y-v)

neighbors :: Coord -> [Coord]
neighbors (x,y) = [ (x+1,y) , (x-1,y) , (x,y-1), (x,y+1)]

region :: Coord -> Coord -> [Coord]
region (xlo, ylo) (xhi, yhi) = [(x, y) | x <- [xlo .. xhi], y <- [ylo .. yhi]]

part1 :: [Coord] -> [Coord] -> [Coord] -> Int
part1 input box0 box1 =
    maximum $
    Map.mapMaybe id $
    Map.intersectionWith match (regionSizes box0) (regionSizes box1)
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


part2 :: [Coord] -> [Coord] -> Int
part2 inp box0 =
  search Set.empty startingPoints

  where
    distances :: Coord -> Int
    distances (x,y) = sum (map (manhattan (x,y)) inp)

    startingPoints = Set.fromList [b | b <- box0, distances b < far]

    search seen next =
      case Set.minView next of
        Nothing -> Set.size seen
        Just (i, next')
          | Set.notMember i seen && distances i < 10000 ->
              search (Set.insert i seen) (foldl (flip Set.insert) next' (neighbors i))
          | otherwise -> search seen next'
