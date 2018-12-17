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
import           Control.Applicative
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Array.Unboxed as A

type Coord = (Int, Int)
type Walls = A.UArray Coord Bool

-- | Print the answers to day 17
main :: IO ()
main =
  do input <- getParsedLines 17 parseLine
     let walls = expandInput input
         (walls', water) = solver walls

     putStrLn (draw walls walls' water)

     let flowingN  = Set.size water
         standingN = count id (zipWith (/=) (A.elems walls) (A.elems walls'))

     print (flowingN + standingN)
     print standingN

data Mode = LookLeft | LookRight | LookDown
  deriving (Eq, Ord, Show)

-- water flow logic ----------------------------------------------------

solver :: Walls -> (Walls, Set Coord)
solver walls
  | null fills = (walls, water)
  | otherwise  = solver walls'
  where
    y      = snd (fst (A.bounds walls))
    search = dfs (neighbors walls) (LookDown, (500,y))
    water  = Set.fromList (map snd search)

    walls' = walls A.// [ ((x,ly), True)  | ((lx,ly),(rx,_ry)) <- fills, x <- [lx .. rx] ]

    fills = [ (l,r) | (LookDown, c) <- search, Just (l,r) <- [isContained c] ]

    isContained c
      | inArray walls (below c) && walls A.! below c = liftA2 (,) (isContained1 left c) (isContained1 right c)
      | otherwise = Nothing

    isContained1 f c
      | walls A.! f c         = Just c
      | walls A.! below (f c) = isContained1 f (f c)
      | otherwise             = Nothing

-- water flow logic ----------------------------------------------------

neighbors :: Walls -> (Mode, Coord) -> [(Mode, Coord)]
neighbors walls (_, c)
  | not (inArray walls (below c)) = []
  | not (walls A.! below c) = [ (LookDown, below c) ]

neighbors walls (LookDown, c) =
  [ (LookLeft , c') | let c' = left c , inArray walls c', not (walls A.! c') ] ++
  [ (LookRight, c') | let c' = right c, inArray walls c', not (walls A.! c') ]

neighbors walls (LookLeft, c) =
  [ (LookLeft , c') | let c' = left c , inArray walls c', not (walls A.! c') ]

neighbors walls (LookRight, c) =
  [ (LookRight, c') | let c' = right c, inArray walls c', not (walls A.! c') ]

-- input format --------------------------------------------------------

data Entry = Horiz Int Int Int | Vert Int Int Int
  deriving Show

expandInput :: [Entry] -> Walls
expandInput input =
  toArray [ (x,y) | entry <- input
                  , x <- case entry of
                           Vert x _ _ -> [x]
                           Horiz _ x1 x2 -> [x1..x2]
                  , y <- case entry of
                           Vert _ y1 y2 -> [y1..y2]
                           Horiz y _ _ -> [y]
                  ]

parseLine :: Parser Entry
parseLine = parseHoriz <|> parseVert

parseHoriz, parseVert :: Parser Entry
parseHoriz = Horiz <$ "y=" <*> number <* ", x=" <*> number <* ".." <*> number
parseVert  = Vert <$ "x=" <*> number <* ", y=" <*> number <* ".." <*> number

-- rendering -----------------------------------------------------------

draw :: Walls -> Walls -> Set (Int,Int) -> String
draw walls walls' water = unlines picture
  where
    ((locol,lorow),(hicol,hirow)) = A.bounds walls
    picture = [ [ if Set.member (col,row) water then '|' else
                  if walls A.! (col,row) then '▓' else
                  if walls' A.! (col,row) then '~' else '░'
                | col <- [locol..hicol]]
              | row <- [lorow..hirow]]

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

toArray :: [(Int, Int)] -> A.UArray (Int,Int) Bool
toArray xs = A.accumArray (\_ e -> e) False ((minx,miny),(maxx,maxy))
                        [ (xy, True) | xy <- xs ]
  where
    miny = minimum (map snd xs)
    maxy = maximum (map snd xs)
    minx = minimum (map fst xs) - 1
    maxx = maximum (map fst xs) + 1

below, left, right :: Coord -> Coord
below (x,y) = (x,y+1)
left  (x,y) = (x-1,y)
right (x,y) = (x+1,y)

