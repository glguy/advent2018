{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/7>

-}
module Main (main) where

import Advent
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- | Print the answers to day 7
main :: IO ()
main =
  do input <- parseInput <$> getInput 7
     putStrLn (part1 input)
     print (part2 input)

parseInput :: [String] -> [(Char,Char)]
parseInput = map $ \str ->
  case words str of
    xs-> (head (xs!!1),head (xs!!7))

part1 :: [(Char, Char)] -> String
part1 = unfoldr go . newWorkQueue
  where
    go q = do (x, q') <- nextTask q
              return (x, finishTasks [x] q')

part2 :: [(Char, Char)] -> Int
part2 = part2' 0 Map.empty . newWorkQueue

part2' :: Int -> Map Char Int -> WorkQueue -> Int
part2' time work queue
  | isComplete queue, Map.null work = time
  | Map.size work < 5, Just (next, queue') <- nextTask queue =
      part2' time (addWork next work) queue'
  | otherwise =
      part2' (time+step) (Map.filter (>0) work') queue'
  where
    step          = minimum work
    (done, work') = Map.partition (0==) (subtract step <$> work)
    queue'        = finishTasks (Map.keys done) queue


addWork :: Char -> Map Char Int -> Map Char Int
addWork c m = Map.insert c (timeRequired c) m

timeRequired :: Char -> Int
timeRequired c = ord c - ord 'A' + 61

data WorkQueue = WorkQueue [Char] [(Char,Char)]

newWorkQueue :: [(Char, Char)] -> WorkQueue
newWorkQueue deps = WorkQueue (nub [z | (x,y) <- deps, z <- [x,y]]) deps

nextTask :: WorkQueue -> Maybe (Char, WorkQueue)
nextTask (WorkQueue remaining deps)
  | null candidates = Nothing
  | otherwise       = Just (next, WorkQueue (delete next remaining) deps)
  where
    candidates = remaining \\ map snd deps
    next = minimum candidates

finishTasks :: [Char] -> WorkQueue -> WorkQueue
finishTasks tasks (WorkQueue remaining deps) = WorkQueue remaining deps'
  where
    deps' = filter (\x -> not (fst x `elem` tasks)) deps

isComplete :: WorkQueue -> Bool
isComplete (WorkQueue remaining _) = null remaining
