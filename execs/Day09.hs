{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/9>

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent (Parser, getParsedLines, number)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Print the answers to day 9
main :: IO ()
main =
  do [(players, marbles)] <- getParsedLines 9 parseInput
     print (game players marbles)
     print (game players (100*marbles))

parseInput :: Parser (Int, Int)
parseInput = (,) <$> number <* " players; last marble is worth " <*> number <* " points"

-- | Player the marble game, find maximum score
--
-- >>> game 10 1618
-- 8317
-- >>> game 13 7999
-- 146373
-- >>> game 17 1104
-- 2764
-- >>> game 21 6111
-- 54718
-- >>> game 30 5807
-- 37305
game :: Int {- ^ players -} -> Int {- ^ max marble -} -> Int {- ^ max score -}
game players marbles = go IntMap.empty (Seq.singleton 0) 0 1
  where
    go scores circle player marble

      -- game over, find winning score
      | marble > marbles = maximum scores

      -- scoring marble, update current elf's score
      | isScoreMarble marble =
          case Seq.splitAt (Seq.length circle - 7) circle of
            (l, picked Seq.:<| r) ->
               go (IntMap.insertWith (+) player (marble + picked) scores)
                  (r Seq.>< l)
                  player' marble'

      -- normal turn, just add the marble
      | otherwise =
          case Seq.splitAt (wrap (Seq.length circle)) circle of
            (l,r) -> go scores
                        (marble Seq.<| r Seq.>< l)
                        player' marble'
      where
        player' = (player + 1) `rem` players
        marble' = marble + 1

-- | Predicate for marbles that trigger a score event
isScoreMarble :: Int -> Bool
isScoreMarble i = i `rem` 23 == 0

-- | Figure out what index to split the circle at on a normal turn
wrap :: Int -> Int
wrap 0 = 0
wrap 1 = 1
wrap 2 = 0
wrap n = 2

--405 players; last marble is worth 70953 points
