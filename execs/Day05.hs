{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/5>

-}
module Main (main) where

import Advent (getInput)
import Data.Char (toLower, toUpper)
import Data.List (nub)

-- | Print the answers to day 5
main :: IO ()
main =
  do inp <- head . words <$> getInput 5
     print (part1 inp)
     print (part2 inp)


-- | Reduce a string by eliminating matched pairs of upper and lowercase
-- letters.
part1 :: String -> Int
part1 = length . foldr step ""
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys                                        = x : ys


-- | Find the minimum length a string can reduce to when we're allowed
-- to remove any one pair of characters.
part2 :: String -> Int
part2 str = minimum lengths
  where
    candidates = nub (map toLower str)
    isOk bad x = x /= bad && x /= toUpper bad
    lengths    = [part1 (filter (isOk bad) str) | bad <- ['a'..'z']]
