{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/14>
-}
module Main (main) where

import           Advent    (getInput)
import           Data.List (isPrefixOf, tails, findIndex)
import           Data.Char (digitToInt)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

-- | Print the answers to day 14
main :: IO ()
main =
  do print (take 10 (drop 320851 (3:7:generate initialCooks)))
     print (findIndex ([3,2,0,8,5,1] `isPrefixOf`) (tails (3:7:generate initialCooks)))

data Cooks = Cooks Int Int (Seq Int)
  deriving Show

initialCooks :: Cooks
initialCooks = Cooks 0 1 (Seq.fromList [3,7])

generate :: Cooks -> [Int]
generate (Cooks elf1 elf2 recipies) =
  news ++ generate (Cooks elf1' elf2' recipies')
  where
    elf1r = Seq.index recipies elf1
    elf2r = Seq.index recipies elf2

    news = map digitToInt (show (elf1r + elf2r))
    recipies' = recipies <> Seq.fromList news

    elf1' = (elf1 + elf1r + 1) `rem` Seq.length recipies'
    elf2' = (elf2 + elf2r + 1) `rem` Seq.length recipies'
