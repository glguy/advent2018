{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/10>

In my original attempt I considered messages with a window size of 20.
Now that I know the message is only 10 high, I've narrowed my predicate.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent               (Parser, getParsedLines, number)
import Data.Maybe           (fromJust, isJust, isNothing)
import Text.Megaparsec.Char (space)
import qualified Data.Set as Set

data Particle = P { px, py, dx, dy :: !Int }

-- | Print the answers to day 10
main :: IO ()
main =
  do input <- getParsedLines 10 parseParticle
     let msgs = findWindow
              $ zipWith draw [0..]
              $ iterate (map update) input
     mapM_ putStr msgs

-- | Drop until we get the first Just, and then take until we get the first
-- Nothing. The idea here is that once that particles get close together they'll
-- start to spread back out and never return.
findWindow :: [Maybe a] -> [a]
findWindow = map fromJust . takeWhile isJust . dropWhile isNothing

-- | Given a number of seconds and the current list of particles,
-- render the particles to a string when they are close enough together
-- to be considered interesting to look at.
draw :: Int -> [Particle] -> Maybe String
draw i ps
  | hirow - lorow <= 10 = Just (unlines (show i : picture))
  | otherwise           = Nothing
  where
    s     = Set.fromList [ (px p, py p) | p <- ps ]
    locol = minimum (map px ps)
    lorow = minimum (map py ps)
    hicol = maximum (map px ps)
    hirow = maximum (map py ps)
    picture = [ [ if Set.member (col,row) s then '#' else ' '
                | col <- [locol..hicol]]
              | row <- [lorow..hirow]]

parseParticle :: Parser Particle
parseParticle =
  P <$ "position=<"   <* space <*> number
    <* ","            <* space <*> number
    <* "> velocity=<" <* space <*> number
    <* ","            <* space <*> number
    <* ">"

-- | Update a particle to move one time step.
update :: Particle -> Particle
update (P x y dx dy) = P (x+dx) (y+dy) dx dy
