{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/4>

-}
module Main (main) where

import           Advent (cardinality, getInput)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List (maximumBy, sortBy)
import           Data.Ord (comparing)
import           Data.Time (LocalTime, readSTime, defaultTimeLocale, todMin, localTimeOfDay)

-- | Print solutions to part 1 and part 2 of day 4
main :: IO ()
main =
  do input <- loadFile <$> getInput 4
     let sleepMins = toSleepMinutes input
     print (part1 sleepMins)
     print (part2 sleepMins)

-- | Log entries
data Entry
  = Start Int -- ^ Guard begins shift
  | Wake      -- ^ Current guard wakes up
  | Sleep     -- ^ Current guard falls asleep
  deriving Show

-- | Parse a file into lines of entries and sort them by timestamp.
loadFile :: String -> [(LocalTime, Entry)]
loadFile = sortBy (comparing fst) . map parseLine . lines

-- | Parse one of the log entries
parseLine :: String -> (LocalTime, Entry)
parseLine str =
  case readSTime True defaultTimeLocale "[%Y-%m-%d %H:%M]" str of
    [(time, descr)] -> (time, entry)
      where
        entry =
          case words descr of
            ["wakes", "up"] -> Wake
            ["falls", "asleep"] -> Sleep
            ["Guard", '#':num, "begins", "shift"] -> Start (read num)
            _ -> error descr
    _ -> error str

-- | Generate a list of Guard ID and minute pairs for each minute that
-- a particular guard is sleeping.
toSleepMinutes :: [(LocalTime, Entry)] -> [(Int, Int)]
toSleepMinutes = go (error "no start")
  where
    go _ ((_, Start who) : xs) = go who xs
    go who ((t1, Sleep) : (t2, Wake) : xs) =
      [ (who, s) | s <- [getMinute t1 .. getMinute t2 - 1] ] ++ go who xs
    go _ _ = []

-- | Extract the minute from a local time.
getMinute :: LocalTime -> Int
getMinute = todMin . localTimeOfDay

-- | Given a list of guard/minute pairs, find the product of the number
-- of the sleepiest guard multiplied by the minute that guard is sleepiest.
part1 :: [(Int, Int)] -> Int
part1 sleepMins = sleepyNum * sleepyMin
  where
    minutesSlept = cardinality (map fst sleepMins)
    sleepyNum    = bigKey minutesSlept
    sleepyMins   = cardinality [ m | (n, m) <- sleepMins, n == sleepyNum ]
    sleepyMin    = bigKey sleepyMins

-- | Give a list of guard/minute pairs, find the product of the
-- guard that sleeps the most in a particular minute and that minute.
part2 :: [(Int, Int)] -> Int
part2 sleepMins = num * minute
  where
    (num, minute) = bigKey (cardinality sleepMins)

-- | Find the key associated with the largest value in a 'Map'.
bigKey :: Ord a => Map k a -> k
bigKey = fst . maximumBy (comparing snd) . Map.toList
