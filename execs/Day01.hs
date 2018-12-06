{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/1>

Day 1 gives us a list of differences to sum up. We compute the sum
of these differences, and we search for duplicates in the partial
sums of the differences list.

-}
module Main (main) where

import Advent (getInput)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

-- | Print the answers to the problem.
main :: IO ()
main =
  do inp <- parseInput <$> getInput 1
     print (sum inp)
     print (part2 inp)

-- | Parse input a lines of integers prefixed by @+@ and @-@.
parseInput :: [String] -> [Integer]
parseInput = map readInt

-- | Parse an integer that is prefixed with a @+@ or @-@.
--
-- >>> readInt "+10"
-- 10
-- >>> readInt "-42"
-- -42
readInt :: String -> Integer
readInt ('+':xs) = read xs
readInt xs       = read xs

-- | Compute the partial sums of a list.
--
-- >>> partialSums [1,1,1]
-- [0,1,2,3]
-- >>> partialSums [1,-2,3]
-- [0,1,-1,2]
-- >>> partialSums []
-- [0]
partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

-- | Given the list of differences, find the first partial sum
-- that repeats when we cycle the list of differences.
--
-- >>> part2 [1,-1]
-- 0
-- >>> part2 [3,3,4,-2,-4]
-- 10
-- >>> part2 [-6,3,8,5,-6]
-- 5
-- >>> part2 [7,7,-2,-7,-4]
-- 14
part2 :: [Integer] -> Integer
part2 = fromJust . firstDuplicate . partialSums . cycle

-- | Find the first duplicate element in a list.
firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs

