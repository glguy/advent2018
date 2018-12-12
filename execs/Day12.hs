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
import Data.List (tails, dropWhileEnd)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

-- | Print the answers to day 12
main :: IO ()
main =
  do (initial, rules) <- parseInput <$> getInput 12
     let xs = iterate (update rules) initial
     print (part1 xs)
     print (part2 xs)

parseInput :: [String] -> (Garden, Rule)
parseInput (initialStateLine : "" : ruleLines)
  | ["initial", "state:", garden] <- words initialStateLine =
     ( mkGarden 0 (map isPlant garden)
     , toRule (map parseRuleLine ruleLines))
parseInput _ = error "parseInput failed"

parseRuleLine :: String -> ([Bool], Bool)
parseRuleLine str =
  case words str of
    [a,"=>",[c]] -> (map isPlant a, isPlant c)
    _            -> error ("parseLine: " ++ str)

part1 :: [Garden] -> Int
part1 xs = eval 0 (xs !! 20)

part2 :: [Garden] -> Int
part2 xs = eval (step * (50e9 - i)) row
  where
    (i, step, row) = loopIteration xs

loopIteration :: [Garden] -> (Int, Int, Garden)
loopIteration = go . zip [0..]
  where
    go ( (i,Garden n xs) : rest@((_,Garden m ys) : _))
      | xs == ys  = (i, m-n, Garden n xs)
      | otherwise = go rest

eval :: Int -> Garden -> Int
eval offset (Garden n xs) = sum [ offset + i | (i, True) <- zip [n ..] xs ]

update :: Rule -> Garden -> Garden
update input (Garden n xs)
  = Garden (n + length leading - 2) xs'
  where
    (leading,xs') = break id
        $ dropWhileEnd not
        $ map (matchRule input)
        $ tails
        $ False : False : False : False : xs

isPlant :: Char -> Bool
isPlant x = '#' == x

data Garden = Garden !Int [Bool]
  deriving (Read, Show)

mkGarden :: Int -> [Bool] -> Garden
mkGarden n xs = Garden (n + length a) b
  where
    (a,b) = break id xs

data Rule
  = Branch Rule Rule
  | Leaf Bool
  deriving (Read, Show)

matchRule :: Rule -> [Bool] -> Bool
matchRule (Leaf x) _ = x
matchRule (Branch f t) (x:xs) = matchRule (if x then t else f) xs
matchRule (Branch f _) [] = matchRule f []

toRule :: [([Bool], Bool)] -> Rule
toRule [([], x)] = Leaf x
toRule ys = Branch (toRule a) (toRule b)
  where
    (a,b) = partitionEithers
          $ map (\ (x:xs, y) -> if x then Right (xs, y) else Left (xs, y)) ys
