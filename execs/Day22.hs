{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/22>

This solution uses an A* graph search to find the shortest path through
the cave to the target.

I picked an arbitrary cave size to memoize that was big enough to avoid
indexing errors. I use a boxed array so that I can lazily compute the
geologic indexes of the various locations in the cave. This also allows
me to recursively define 'geologic' and 'erosion'.

My A* heuristic is manhattan distance to the target plus a penalty for
not holding the torch. (Accounting for the torch saves a small, but
positive amount of time.)

Instead of modelling the tool being held directly I simply keep track of
the risk number of the area I'm not allowed to enter.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import           Advent (Parser, getParsedInput, number)
import           Advent.Coord (Coord(C), above, left, boundingBox, cardinal, manhattan, origin)
import           Advent.Visualize (writePng, colorWheel, drawCoords, Image, PixelRGB8(..))
import qualified Advent.PQueue as PQueue
import qualified Data.Array as A
import           Data.List (delete, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Print the answers to day 22
main :: IO ()
main =
  do (depth, target) <- getParsedInput 22 parseInput
     -- (a,b:_) = break (goal==) (astar steps start)
     -- writePng "output.png" (draw (b:a))
     let risk = mkRisk depth target
     print (part1 risk target)
     print (part2 risk target)

-- | Sum of risk values in rectangle defined by origin and target
part1 :: (Coord -> Tool) -> Coord -> Int
part1 risk target = sum [ toolId (risk c) | c <- A.range (origin, target) ]

-- | Minimum cost of traveling to the target from the origin
part2 :: (Coord -> Tool) -> Coord -> Int
part2 risk target = n
  where
    Just n = lookup goal (astar (steps risk target) start)
    start = Node origin torch
    goal  = Node target torch

-- tool representation -------------------------------------------------

-- | Tools track the risk index that they are incompatible with.
newtype Tool = Tool { toolId :: Int } deriving (Show, Eq, Ord)

torch :: Tool
torch = Tool 1 -- torch is excluded from wet (1) squares

tools :: [Tool]
tools = [Tool 0, Tool 1, Tool 2]

-- movement rules ------------------------------------------------------

-- | Graph search node. There will be a lot of these and this
-- representation is much more compact than a tuple.
data Node = Node {-# Unpack #-}!Coord {-# Unpack #-}!Tool deriving (Eq, Ord)

-- | Compute the states reachable from the given state. Cost is the
-- incremental cost of choosing that state. Heuristic is lower-bound
-- on the distance remaining until the target. This lower-bound is an
-- admissible heuristic that enables A* to find the optimal path.
steps ::
  (Coord -> Tool)    {- ^ location to banned tool         -} ->
  Coord              {- ^ target location                 -} ->
  Node               {- ^ location, tool                  -} ->
  [(Node, Int, Int)] {- ^ location, tool, cost, heuristic -}
steps risk target (Node here tool) =
  [ (Node dest tool', cost, heuristic)
     | (Node dest tool', cost) <- changeTool ++ move
     , risk dest /= tool'
     , let heuristic = manhattan dest target
                     + if tool' == torch then 0 else 7
     ]
  where
    changeTool = [ (Node here tool', 7) | tool' <- delete tool tools ]
    move = [ (Node dst tool, 1) | dst@(C y x) <- cardinal here, y >= 0, x >= 0 ]

-- cave characterization -----------------------------------------------

-- | Computes a function that can query the risk index at a given query
-- coordinate. The query function is backed by an array to efficiently
-- compute risks for a given depth and target value.
mkRisk ::
  Int   {- ^ layer depth                    -} ->
  Coord {- ^ target coordinate              -} ->
  Coord {- ^ query coordinate               -} ->
  Tool  {- ^ risk index at query coordinate -}
mkRisk depth target = risk
  where
    bnds = (C 0 0, C 1000 200) -- hardcoded magic bounds

    -- array used for memoization
    geologic :: A.Array Coord Int
    geologic = A.listArray bnds
      [(if y == 0      then x * 16807 else
        if x == 0      then y * 48271 else
        if c == target then 0         else
        erosion (above c) * erosion (left c)
       ) `rem` 20183
       | c@(C y x) <- A.range bnds ]

    erosion i = (geologic A.! i + depth) `rem` 20183
    risk    i = Tool (erosion i `rem` 3)


-- graph search --------------------------------------------------------

-- | A* graph search. Returns a list of states in order of cost starting
-- from the given start state and using the given step function to
-- compute successive states.
astar ::
  Ord a =>
  (a -> [(a, Int, Int)]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                      {- ^ starting state                                           -} ->
  [(a, Int)]             {- ^ list of states visited with associated cost              -}
astar nexts start = go Set.empty (PQueue.singleton 0 (0, start))
  where
    go _ PQueue.Empty = []
    go seen ((cost, x) PQueue.:<| work)
      | Set.member x seen = go seen work
      | otherwise         = (x, cost) : go seen' work'
      where
        seen' = Set.insert x seen
        work' = foldl' addWork work (nexts x)
        addWork w (x', stepcost, heuristic) =
          let cost' = cost + stepcost
          in PQueue.insert (cost' + heuristic) (cost', x') w

-- input parsing -------------------------------------------------------

-- | Parse depth and target coordinate
parseInput :: Parser (Int, Coord)
parseInput =
  (,) <$ "depth: "  <*> number     <* "\n"
      <* "target: " <*> parseCoord <* "\n"

-- | Parse a coordinate in @X,Y@ form.
parseCoord :: Parser Coord
parseCoord = flip C <$> number <* "," <*> number

-- visualization -------------------------------------------------------

-- | Render the search states visited on the way to finding the shortest
-- route to the target. Use a rainbow gradient to show the relative
-- costs of visiting each location.
draw :: (Coord -> Tool) -> [((Coord, Tool), Int)] -> Image PixelRGB8
draw risk visited = drawCoords box $ \i ->
  case Map.lookup i v of
    Nothing -> let r = fromIntegral (255 - 20 * toolId (risk i))
               in PixelRGB8 r r r
    Just d  -> colorWheel (fromIntegral (255 * d `quot` maxD))
  where
    maxD = maximum v
    v = Map.fromList [ (c, d) | ((c, _), d) <- visited ]
    Just box = boundingBox (map (fst . fst) visited)
