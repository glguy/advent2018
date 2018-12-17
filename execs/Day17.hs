{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/17>
-}
{-# Language OverloadedStrings #-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Control.Applicative
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Array.Unboxed as A
import           Codec.Picture

type Walls = A.UArray Coord Bool

-- | Print the answers to day 17
main :: IO ()
main =
  do input <- getParsedLines 17 parseLine
     let walls = toArray (concat input)
         frames = solver walls
         (walls', water) = last frames

     writePng "output.png" (draw walls walls' water)
     -- sequence_ $ writeGifAnimation "output.gif"
     --    10 LoopingForever
     --     [ draw walls walls' water | (walls', water) <- frames ]

     let flowingN  = Set.size water
         standingN = count id (zipWith (/=) (A.elems walls) (A.elems walls'))

     print (flowingN + standingN)
     print standingN

data Mode = LookLeft | LookRight | LookDown
  deriving (Eq, Ord, Show)

-- water flow logic ----------------------------------------------------

solver :: Walls -> [(Walls, Set Coord)]
solver walls
  | null fills = [(walls, water)]
  | otherwise  = (walls, water) : solver walls'
  where
    startY = coordRow (fst (A.bounds walls))
    search = dfs (waterflow walls) (LookDown, (C startY 500))
    water  = Set.fromList (map snd search)

    walls' = walls A.// fills

    fills  = [(C ly x, True)
               | (LookDown, c) <- search        -- downward-flowing water
               , inArray walls (below c)        -- not on the bottom
               , walls A.! below c              -- supported by a wall
               , C ly lx <- isContained left  c -- look to the left
               , C _  rx <- isContained right c -- look to the right
               , x       <- [lx + 1 .. rx - 1]  -- between the walls
               ]

    isContained f c
      | walls A.! c       = [c]
      | walls A.! below c = isContained f (f c)
      | otherwise         = []

-- water flow logic ----------------------------------------------------

waterflow :: Walls -> (Mode, Coord) -> [(Mode, Coord)]
waterflow walls (mode, c)
  | not (inArray walls (below c)) = []
  | not (walls A.! below c) = [ (LookDown, below c) ]
  | otherwise = filter (isOpen . snd)
              $ [ (LookLeft , left  c) | mode /= LookRight ]
             ++ [ (LookRight, right c) | mode /= LookLeft  ]
  where
    isOpen c' = inArray walls c' && not (walls A.! c')

-- input format --------------------------------------------------------

asHoriz, asVert :: Int -> Int -> Int -> [Coord]
asHoriz y xlo xhi = [ C y x | x <- [xlo..xhi] ]
asVert  x ylo yhi = [ C y x | y <- [ylo..yhi] ]

parseLine :: Parser [Coord]
parseLine =
  asHoriz <$ "y=" <*> number <* ", x=" <*> number <* ".." <*> number <|>
  asVert  <$ "x=" <*> number <* ", y=" <*> number <* ".." <*> number

-- rendering -----------------------------------------------------------

draw :: Walls -> Walls -> Set Coord -> Image PixelRGB8
draw walls walls' water =
  generateImage toPixel (hicol-locol+1) (hirow-lorow+1)
  where
    standing = PixelRGB8 0 0 255
    flowing  = PixelRGB8 0 200 255
    sand     = PixelRGB8 170 121 66
    clay     = PixelRGB8 100 71 39

    (C lorow locol, C hirow hicol) = A.bounds walls

    toPixel col row =
        if Set.member (C (lorow+row) (locol+col)) water then flowing  else
        if walls A.!  (C (lorow+row) (locol+col))       then clay     else
        if walls' A.! (C (lorow+row) (locol+col))       then standing else
        sand

-- searching -----------------------------------------------------------

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = aux start (const []) Set.empty
  where
    aux x rest seen
      | Set.member x seen = rest seen
      | otherwise = x : foldr aux rest (next x) (Set.insert x seen)

-- array helpers -------------------------------------------------------

inArray :: (Ix i, IArray a e) => a i e -> i -> Bool
inArray a c = A.inRange (A.bounds a) c

toArray :: [Coord] -> A.UArray Coord Bool
toArray xs = A.accumArray (\_ e -> e) False (C miny minx, C maxy maxx)
                        [ (xy, True) | xy <- xs ]
  where
    miny = minimum (map coordRow xs)
    maxy = maximum (map coordRow xs)
    minx = minimum (map coordCol xs) - 1
    maxx = maximum (map coordCol xs) + 1
