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

This program works for inputs that eventually reach a point
where they repeatedly just shift in one direction or another.

-}
{-# Language OverloadedStrings, NumDecimals #-}
module Main (main) where

import Advent               (Parser, getParsedInput)
import Control.Monad        (replicateM)
import Data.List            (dropWhileEnd, tails)
import Data.Either          (partitionEithers)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec      ((<|>), endBy, eof, many)

-- | Print the answers to day 12
main :: IO ()
main =
  do (initial, rules) <- getParsedInput 12 parseInput
     let xs = iterate (update rules) initial
     print (part1 xs)
     print (part2 xs)

-- parsing -------------------------------------------------------------

-- | Parse the input file returning the initial garden and the rules.
parseInput :: Parser (Garden, Rule)
parseInput =
  do start   <- "initial state: " *> many parsePlant <* newline <* newline
     entries <- parseRule `endBy` newline <* eof
     pure (mkGarden start, toRule entries)

-- | Parse a single plant as a @#@ or empty spae @.@
parsePlant :: Parser Bool
parsePlant = True <$ "#" <|> False <$ "."

-- | Parse a whole rule of the form @.#.#. => #@
parseRule :: Parser ([Bool], Bool)
parseRule = (,) <$> replicateM 5 parsePlant <* " => " <*> parsePlant

------------------------------------------------------------------------

-- | Evaluate the 20th iteration of the garden
part1 :: [Garden] -> Int
part1 xs = eval (xs !! 20)

-- | Evaluate the 50 billionth iteration of the garden.
-- This implementation relies on the state eventually repeating
-- modulo a shift to the left or right.
part2 :: [Garden] -> Int
part2 xs = eval (shiftGarden (step * (50e9 - i)) row)
  where
    (i, step, row) = loopIteration xs

-- | Compute the sum of the pot locations which have a plant
eval :: Garden -> Int
eval (Garden n xs) = sum [ i | (i, True) <- zip [n ..] xs ]

-- iteration logic -----------------------------------------------------

-- | Iterate a garden until it advances to a state that is identical
-- to the previous state except that it might be shifted in one direction.
-- Return the generation at which the pattern is stable, how much the garden
-- shifts when it repeats, and the garden at that iteration.
loopIteration :: [Garden] -> (Int, Int, Garden)
loopIteration = go . zip [0..]
  where
    go ( (i,Garden n xs) : rest@((_,Garden m ys) : _))
      | xs == ys  = (i, m-n, Garden n xs)
      | otherwise = go rest
    go _ = error "loopIteration: bad stream"

-- | Apply the update rule to a garden producing the next generation
update :: Rule -> Garden -> Garden
update input (Garden n xs) = shiftGarden (n-2) (mkGarden xs')
  where
    -- the -2 shift accounts for the first rule match placing
    -- a plant 2 before xs because we have 4 empty pots prefixed
    -- onto the garden and we assume that all rules match 5 pots
    xs' = map (matchRule input)
        $ tails
        $ False : False : False : False : xs

-- normalized garden representation ------------------------------------

-- | Gardens are pot locations kept in a normalized form
-- that eliminates leading and trailing empty locations
data Garden = Garden !Int [Bool] -- ^ first index and pots
  deriving (Read, Show)

-- | Make a new garden value given a list of plants starting
-- with the zero pot.
mkGarden :: [Bool] -> Garden
mkGarden xs = Garden (length a) (dropWhileEnd not b)
  where
    (a,b) = break id xs

-- | Move all pot locations in a garden by a given offset.
shiftGarden :: Int -> Garden -> Garden
shiftGarden offset (Garden n xs) = Garden (offset + n) xs

-- rule matching -------------------------------------------------------

-- | Binary tree for more efficient rule matching
data Rule
  = Branch Rule Rule -- ^ branch left for empty, right for plant
  | Leaf Bool        -- ^ end of rule, replace with this value
  deriving (Read, Show)

-- | Match the prefix of a garden against a rule returning the
-- new plant to place at that location.
matchRule :: Rule -> [Bool] -> Bool
matchRule (Leaf x) _ = x
matchRule (Branch f t) (x:xs) = matchRule (if x then t else f) xs
matchRule (Branch f _) [] = matchRule f []

-- | Construct a binary rule tree from a list of rules.
toRule :: [([Bool], Bool)] -> Rule
toRule [] = Leaf False
toRule [([], x)] = Leaf x
toRule ys = Branch (toRule a) (toRule b)
  where
    (a,b) = partitionEithers
          $ map (\ (x:xs, y) -> if x then Right (xs, y) else Left (xs, y)) ys

-- $example
--
-- >>> import Text.Megaparsec (parseMaybe)
-- >>> :{
-- let exampleInput =
--       "initial state: #..#.#..##......###...###\n\
--       \\n\
--       \...## => #\n\
--       \..#.. => #\n\
--       \.#... => #\n\
--       \.#.#. => #\n\
--       \.#.## => #\n\
--       \.##.. => #\n\
--       \.#### => #\n\
--       \#.#.# => #\n\
--       \#.### => #\n\
--       \##.#. => #\n\
--       \##.## => #\n\
--       \###.. => #\n\
--       \###.# => #\n\
--       \####. => #\n"
-- :}
--
-- >>> let Just (garden, rule) = parseMaybe parseInput exampleInput
-- >>> part1 (iterate (update rule) garden)
-- 325
