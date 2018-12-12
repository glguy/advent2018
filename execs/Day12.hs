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

import           Advent               (Parser, getParsedInput)
import           Control.Monad        (replicateM)
import           Data.List            (dropWhileEnd, foldl', tails)
import           Text.Megaparsec.Char (newline)
import           Text.Megaparsec      ((<|>), endBy, eof, many)
import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed as Vector

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
     entries <- many parseRule <* eof
     pure (mkGarden start, toRule entries)

-- | Parse a single plant as a @#@ or empty spae @.@
parsePlant :: Parser Bool
parsePlant = True  <$ "#"
         <|> False <$ "."

-- | Parse a whole rule production of the form @.#.#. => #@
parseRule :: Parser ([Bool], Bool)
parseRule = (,) <$> replicateM 5 parsePlant <* " => " <*> parsePlant <* newline

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
update input (Garden n xs) = shiftGarden (n-3) (mkGarden xs')
  where
    -- the -3 shift accounts for the first rule match placing
    -- a plant 3 before xs because we first match the empty plot
    -- before feeding pots in one at a time.
    xs' = map (matchRule input)
        $ scanl pushBit 0
        $ xs ++ replicate 4 False -- flush window with 4 more empties

-- normalized garden representation ------------------------------------

-- | Gardens are pot locations kept in a normalized form
-- that eliminates leading and trailing empty locations
data Garden = Garden !Int [Bool] -- ^ first index and pots
  deriving (Read, Show)

-- | Make a new garden value given a list of plants starting
-- with the zero pot.
--
-- >>> mkGarden [False, True, True, False]
-- Garden 1 [True,True]
mkGarden :: [Bool] -> Garden
mkGarden xs = Garden (length a) (dropWhileEnd not b)
  where
    (a,b) = break id xs

-- | Move all pot locations in a garden by a given offset.
--
-- >>> shiftGarden 4 (Garden 3 [True, False, True])
-- Garden 7 [True,False,True]
shiftGarden :: Int -> Garden -> Garden
shiftGarden offset (Garden n xs) = Garden (offset + n) xs

-- rule matching -------------------------------------------------------

-- | Rules take a window of 5 pots as bits forming an index between 0 and 31.
-- The vector has a True for windows where a plant should be emitted.
newtype Rule = Rule (Vector Bool)

-- | Match the prefix of a garden against a rule returning the
-- new plant to place at that location.
matchRule :: Rule -> Int {- ^ 5-bit window -} -> Bool
matchRule (Rule v) i = v Vector.! i

-- | Construct a efficient rule lookup mechanism given a list of
-- rule productions. Each of the rule productions will correspond to
-- an element in the vector that will be True if a plant is produced.
toRule :: [([Bool], Bool)] -> Rule
toRule entries = Rule (Vector.generate 32 (`elem` plants))
  where
    plants = [ foldl' pushBit 0 bs | (bs, True) <- entries ]

-- | Update the 5-bit window with a new low-endian bit, shifting
-- all the other bits up by one.
--
-- >>> :set -XBinaryLiterals
-- >>> pushBit 0b10011 False == 0b00110
-- True
-- >>> pushBit 0b10011 True == 0b00111
-- True
pushBit ::
  Int  {- ^ five-bit window -} ->
  Bool {- ^ new bit         -} ->
  Int  {- ^ five-bit window -}
pushBit acc x = (acc * 2 + fromEnum x) `rem` 32

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
