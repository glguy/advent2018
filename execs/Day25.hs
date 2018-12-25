{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/25>

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent (Parser, getParsedLines, number)
import Text.Megaparsec (sepBy)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

-- | Print the answers to day 25
main :: IO ()
main =
  do input <- getParsedLines 25 parseLine
     print (noComponents (starGraph input))

starGraph :: [[Int]] -> UGr
starGraph stars =
  mkUGraph [ 1 .. length stars ]
           [ (i,j) | (i,x) <- zip [1..] stars
                   , (j,y) <- zip [1..] stars
                   , manhattan x y <= 3 ]

parseLine :: Parser [Int]
parseLine = number `sepBy` ","

manhattan :: [Int] -> [Int] -> Int
manhattan x y = sum (zipWith (\a b -> abs (a-b)) x y)
