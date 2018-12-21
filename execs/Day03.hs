{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

-}
{-# Language OverloadedStrings #-} -- for parser
module Main (main) where

import           Advent
import           Advent.Coord
import           Advent.Visualize
import           Data.Map (Map)
import qualified Data.Map as Map

-- | Print the answers to part 1 and 2 of day 3's task.
--
-- >>> :main
-- 115304
-- 275
main :: IO ()
main =
  do patches <- getParsedLines 3 parsePatch
     let fabric = cutFabric patches

     writePng "output.png" (draw fabric)

     print (part1 fabric)
     print (part2 fabric patches)

-- | Description of a quilt patch.
data Patch = Patch { patchId, offsetX, offsetY, sizeX, sizeY :: !Int }
  deriving (Read, Show)

-- | Parse a patch description.
parsePatch :: Parser Patch
parsePatch = Patch <$ "#"   <*> number
                   <* " @ " <*> number
                   <* ","   <*> number
                   <* ": "  <*> number
                   <* "x"   <*> number

-- | Given a list of patches, compute the number of patches covering
-- each coordinate.
--
-- >>> cutFabric [Patch {patchId = 3, offsetX = 5, offsetY = 5, sizeX = 2, sizeY = 2}]
-- fromList [(C 5 5,1),(C 5 6,1),(C 6 5,1),(C 6 6,1)]
cutFabric :: [Patch] -> Map Coord Int
cutFabric = cardinality . concatMap patchCoords

-- | Compute the number of coordinates that are covered by more than one patch
-- given the number of patches that cover each coordinate.
part1 :: Map Coord Int -> Int
part1 = count (> 1)

-- | Find the ID of the patch that overlaps with no others given the number
-- of patches that overlap each coordinate and the list of patches.
part2 :: Map Coord Int -> [Patch] -> Int
part2 fabric patches =
  head [ patchId patch
       | patch <- patches
       , all (1 ==) (Map.intersection fabric (cutFabric [patch]))
       ]

-- | Make a set of the coordinates covered by a patch.
patchCoords :: Patch -> [Coord]
patchCoords patch =
  [ C y x
  | x <- [offsetX patch .. offsetX patch + sizeX patch - 1]
  , y <- [offsetY patch .. offsetY patch + sizeY patch - 1]
  ]

-- | Render the patches
draw :: Map Coord Int -> Image PixelRGB8
draw coords =
  drawCoords bnds $ \i ->
    case Map.lookup i coords of
      Nothing -> PixelRGB8 0 0 0
      Just 1  -> PixelRGB8 0 255 0
      Just i  -> PixelRGB8 (fromIntegral (min 255 (128+30*i))) 0 0
  where
    Just bnds = boundingBox (Map.keys coords)
