{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/2>

Day 2 has us count up the occurrences of characters in words and
find pairs of characters that match except for a single position.

-}
module Main where

import           Advent (cardinality, count, getInput)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List (tails)

-- | Print the answers to part 1 and 2 of day 2's task.
main :: IO ()
main =
  do inp <- lines <$> getInput 2
     print (part1 inp)
     putStrLn (part2 inp)

-- | Compute the number of elements in the list that have exactly 2 of
-- a particular element with the number of elements in the list that have
-- exactly 3 of a particular number of elements.
--
-- >>> part1 ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
-- 12
part1 :: Ord a => [[a]] -> Int
part1 inp = product (map exact [2,3])
  where
    cards = map cardinality inp
    exact n = count (elem n) cards

-- | Return the common elements between the two entries in the given list
-- where all but one of the elements are equal.
--
-- >>> part2 ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
-- "fgij"
part2 :: Eq a => [[a]] -> [a]
part2 inp = head [r | x:xs <- tails inp, y <- xs, Just r <- [offbyone x y]]

-- | Find the common elements between two lists that differ in exactly one
-- position.
--
-- >>> offbyone "abcde" "axcye"
-- Nothing
-- >>> offbyone "fghij" "fguij"
-- Just "fgij"
offbyone :: Eq a => [a] -> [a] -> Maybe [a]
offbyone (x:xs) (y:ys)
  | x  == y  = (x :) <$> offbyone xs ys
  | xs == ys = Just xs
offbyone _ _ = Nothing
