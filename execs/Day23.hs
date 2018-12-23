{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/23>

I don't believe my part 2 solution solves the general problem.
It just happens that it solves the problem for my input. I'm
just committing this because it's what I have right now.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent
import Data.Ord
import Data.List

data Coord = C !Int !Int !Int
  deriving (Eq, Ord, Show)

data Bot = Bot !Coord !Int
  deriving (Eq, Ord, Show)

-- | Print the answers to day 23
main :: IO ()
main =
  do bots <- getParsedLines 23 parseLine

     print (part1 bots)
     print (part2 bots)

part1 :: [Bot] -> Int
part1 bots = count (\(Bot bot _) -> manhattan bot here <= r) bots
  where
    Bot here r = maximumBy (comparing (\(Bot _ r) -> r)) bots

part2 :: [Bot] -> Int
part2 bots = manhattan point (C 0 0 0)
  where
    candidates = concatMap octohedron bots

    visibleFrom pos = count (\(Bot botPos botRad) -> manhattan pos botPos <= botRad)
                            bots
    out = [ (p, visibleFrom p) | p <- candidates ]
    a = maximumBy (comparing (\(pos, n) -> n)) out
    point = minimize 10000 visibleFrom (fst a)

manhattan :: Coord -> Coord -> Int
manhattan (C x y z) (C u v w) = abs (x - u) + abs (y - v) + abs (z - w)

octohedron :: Bot -> [Coord]
octohedron (Bot (C x y z) r) =
  [ C (x+r) (y  ) (z  )
  , C (x-r) (y  ) (z  )
  , C (x  ) (y+r) (z  )
  , C (x  ) (y-r) (z  )
  , C (x  ) (y  ) (z+r)
  , C (x  ) (y  ) (z-r)
  ]

minimize :: Int -> (Coord -> Int) -> Coord -> Coord
minimize d f p@(C x y z) =
  case [ p' | p' <- candidates, f p <= f p' ] of
    p' : _         -> minimize d f p'
    [] | d > 1     -> minimize (d`quot`2) f p
       | otherwise -> p
  where
    candidates =
      [ C (x-d) (y-d) (z-d)

      , C (x-d) (y-d) z
      , C x (y-d) (z-d)
      , C (x-d) y (z-d)

      , C (x-d) y z
      , C x (y-d) z
      , C x y (z-d)
      ]




parseLine :: Parser Bot
parseLine = Bot <$ "pos=" <*> parseCoord <* ", r=" <*> number

parseCoord :: Parser Coord
parseCoord = C <$ "<" <*> number <* "," <*> number <* "," <*> number <* ">"
