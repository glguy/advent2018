{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/8>

-}
{-# Language DeriveTraversable #-}
module Main (main) where

import Advent                    (getInput)
import Control.Monad             (replicateM)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Maybe                (fromMaybe)

-- | Print the answers to day 8
main :: IO ()
main =
  do input <- parseInput . head <$> getInput 8
     let tree = evalState getTree input
     print (part1 tree)
     print (part2 tree)

-- | Parse input as a space-delimited list of integers
parseInput :: String -> [Int]
parseInput = map read . words

-- | A tree can have children and metadata entries.
data Tree a = Tree [Tree a] [a] -- ^ children and metadata
  deriving (Functor, Foldable, Traversable)

-- | Sum of all metadata entries
part1 :: Tree Int -> Int
part1 = sum

-- | Sum of metadata entries on leaf nodes and recursive call on
-- child nodes identified by indexes stored in metadata.
part2 :: Tree Int -> Int
part2 (Tree xs ys)
  | null xs   = sum ys
  | otherwise = sum [ fromMaybe 0 (index i (map part2 xs)) | i <- ys ]

-- | 1-based list index returning Nothing on failure.
index :: Int -> [a] -> Maybe a
index n xs
  | n >= 1, a:_ <- drop (n-1) xs = Just a
  | otherwise                    = Nothing

-- | Parse a tree from a list of integers
getTree :: State [Int] (Tree Int)
getTree =
  do n <- get1
     m <- get1
     a <- replicateM n getTree
     b <- replicateM m get1
     pure (Tree a b)

-- | Extract the next element from the source list.
get1 :: State [a] a
get1 =
  do xxs <- get
     case xxs of
       x:xs -> x <$ put xs
       [] -> error "get1: empty list"
