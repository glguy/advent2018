{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/12>

Probably like most people I approached this program
by looking at the output and noticing that it eventually
stabilized.

This program works for my particular input rules, I don't
know whether or not it works for other people's inputs.

-}
{-# Language NumDecimals #-}
module Main (main) where

import Advent
import Data.List (tails)
import Data.Maybe (fromMaybe)

magicBuffer :: Int
magicBuffer = 25

-- | Print the answers to day 12
main :: IO ()
main =
  do input <- map parseLine <$> getInput 12
     let xs = iterate (update input) initial
     print (part1 xs)
     print (part2 xs)

part1 :: [[Bool]] -> Int
part1 xs = eval 0 (xs !! 20)

part2 :: [[Bool]] -> Int
part2 xs = eval (50e9 - i) row
  where
    (i, row) = loopIteration (take 200 xs)

loopIteration :: [[Bool]] -> (Int, [Bool])
loopIteration = go . zip [0..]
  where
    go ( (i,x) : rest@((_,y) : _))
      | False:x == y++[False] = (i, x)
      | otherwise = go rest

eval :: Int -> [Bool] -> Int
eval offset xs = sum [ offset + i | (i, True) <- zip [-magicBuffer ..] xs ]

update :: [([Bool],Bool)] -> [Bool] -> [Bool]
update input xs
  = map (\i -> fromMaybe False (lookup i input))
  $ takeWhile (\x -> length x == 5)
  $ map (take 5)
  $ tails
  $ False : False : xs ++ [False, False]

parseLine :: String -> ([Bool], Bool)
parseLine str =
  case words str of
    [a,"=>",[c]] -> (map ('#'==) a, '#'==c)
    _ -> error ("parseLine: " ++ str)

initial :: [Bool]
initial
  = map ('#' ==)
  $ (replicate magicBuffer '.')
  ++ real ++ replicate 1000 '.'

real :: String
real = ".##..##..####..#.#.#.###....#...#..#.#.#..#...\
       \#....##.#.#.#.#.#..######.##....##.###....##..#.####.#"
