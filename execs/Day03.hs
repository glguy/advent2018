{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

-}
{-# Language OverloadedStrings #-} -- for parser
module Main where

import           Advent
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- | Print the answers to part 1 and 2 of day 3's task.
main :: IO ()
main =
  do inp <- getParsedLines 3 parsePatch
     print (part1 inp)
     print (part2 inp)

-- | Description of a quilt patch.
data Patch = Patch { patchId, offsetX, offsetY, sizeX, sizeY :: !Int }
  deriving (Read, Show)

-- | Parse a patch description.
--
-- >>> parseTest parsePatch "#3 @ 5,5: 2x2"
-- Patch {patchId = 3, offsetX = 5, offsetY = 5, sizeX = 2, sizeY = 2}
parsePatch :: Parser Patch
parsePatch = Patch <$ "#"   <*> number
                   <* " @ " <*> number
                   <* ","   <*> number
                   <* ": "  <*> number
                   <* "x"   <*> number

-- | Compute the number of coordinates that are covered by more than one patch
part1 :: [Patch] -> Int
part1
  = Map.size                    -- compute the number of coords remaining
  . Map.filter (> (1::Int))     -- keep all coords with an overlap
  . Map.unionsWith (+)          -- combine all the coord counts
  . map (Map.fromSet (const 1)) -- turn coords into counts at each coord
  . map patchCoords             -- get coords for each patch

-- | Find the ID of the patch that overlaps with no others.
part2 :: [Patch] -> Int
part2 patches =
  head [ i | let zs = [ (patchId p, patchCoords p) | p <- patches ]
           , ((i,x),xs) <- pickOne zs
           , all (disjoint x) (map snd xs)
           ]

-- | Make a set of the coordinates covered by a patch.
patchCoords :: Patch -> Set (Int, Int)
patchCoords patch =
  Set.fromList
  [ (offsetX patch + x, offsetY patch + y)
  | x <- [0 .. sizeX patch - 1]
  , y <- [0 .. sizeY patch - 1]
  ]

-- | Check that two sets are disjoint
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint p1 p2 = Set.null (Set.intersection p1 p2)
