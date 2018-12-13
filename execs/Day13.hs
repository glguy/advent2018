{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/13>
-}
module Main (main) where

import           Advent (getInput)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tuple (swap)

-- | Directions determine where a cart will move to on its
-- next tick.
data Dir = North | South | East | West
  deriving Show

-- | Turns determine the behavior at an intersection
data Turn = NextL | NextR | NextS
  deriving Show

-- | Cart state includes the current direction of travel as well
-- as the next turn when an intersection is reached.
data Cart = Cart Dir Turn
  deriving Show

-- | Coordinates are stored row then column in order to get
-- the correct ordering for carts in the simulation.
type Coord = (Int, Int)

-- | Road is a random-accessible representation of the track.
newtype Road = Road (Vector (Vector Char))

-- | Carts are stored in a where they will naturally be ordered
-- in the way that the simulation calls for.
type CartQueue = Map Coord Cart


-- | Print the answers to day 13
main :: IO ()
main =
  do road <- parseInput <$> getInput 13
     let carts = findCarts road
     print (part1 road carts)
     print (part2 road carts)

-- | Run the simulation and report the location of the first collision.
part1 :: Road -> CartQueue -> Coord
part1 road carts = simulate (\pos _ _ -> swap pos) road carts

-- | Run the simulation and report the position of the final car.
part2 :: Road -> CartQueue -> Coord
part2 road carts = simulate onWreck road carts
  where
    -- when a car wrecks, clear that location and resume the simulation
    onWreck pos ready done = tick onWreck road (Map.delete pos ready) (Map.delete pos done)

-- | Parse the input file as a 'Road'
parseInput :: [String] -> Road
parseInput = Road . V.fromList . map V.fromList

-- | Look up the road element at a particular coordinate
indexRoad :: Road -> Coord -> Char
indexRoad (Road v) (i, j) = v V.! i V.! j

-- | Find all the initial locations and directions of the carts.
findCarts :: Road -> Map Coord Cart
findCarts (Road xs) =
  Map.fromList
  [ cart
  | (i,x) <- zip [0..] (V.toList xs)
  , (j,y) <- zip [0..] (V.toList x)
  , cart <- aux i j y
  ]
  where
    aux i j '^' = [((i,j), Cart North NextL)]
    aux i j 'v' = [((i,j), Cart South NextL)]
    aux i j '<' = [((i,j), Cart West  NextL)]
    aux i j '>' = [((i,j), Cart East  NextL)]
    aux _ _ _ = []

-- | Run the simulation to completion. Take the wreck behavior
-- as a parameter to allow part1 and part2 to share the same
-- simulation.
simulate ::
  (Coord -> CartQueue -> CartQueue -> Coord) ->
  Road ->
  CartQueue ->
  Coord
simulate onWreck road carts
  | [(i,j)] <- Map.keys carts = (j,i)
  | otherwise                 = tick onWreck road carts Map.empty

-- | Run a single tick of the simulation.
tick ::
  (Coord -> CartQueue -> CartQueue -> Coord)
            {- ^ wreck behavior          -} ->
  Road      {- ^ road                    -} ->
  CartQueue {- ^ carts ready to  move    -} ->
  CartQueue {- ^ carts moved this tick   -} ->
  Coord     {- ^ final coordinate answer -}
tick onWreck road carts done =
  case Map.minViewWithKey carts of
    Nothing -> simulate onWreck road done
    Just ((pos, cart), carts')
      | collision -> onWreck pos' carts' done
      | otherwise -> tick onWreck road carts' (Map.insert pos' cart' done)
      where
        collision = Map.member pos' done || Map.member pos' carts'
        (pos', cart') = drive road pos cart

-- | Compute the next state of a cart when it is its turn to move
drive :: Road -> Coord -> Cart -> (Coord, Cart)
drive road (i,j) (Cart dir next) = (pos', Cart dir' next')
  where
    seg = indexRoad road pos'

    pos' =
      case dir of
        North -> (i-1,j)
        South -> (i+1,j)
        West  -> (i,j-1)
        East  -> (i,j+1)

    next' =
      case seg of
        '+' -> nextTurn next
        _   -> next

    dir' =
      case (seg, dir) of
        ('\\', North) -> West
        ('\\', South) -> East
        ('\\', East ) -> South
        ('\\', West ) -> North
        ('/' , North) -> East
        ('/' , South) -> West
        ('/' , East ) -> North
        ('/' , West ) -> South
        ('+' , _    ) -> turn next dir
        _             -> dir

-- | Apply a turn to a direction.
turn :: Turn -> Dir -> Dir
turn NextL = turnLeft
turn NextR = turnRight
turn NextS = id

-- | Change a direction counter-clockwise
turnLeft :: Dir -> Dir
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- | Change a direction clockwise
turnRight :: Dir -> Dir
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- | Advance a turn direction to the next one in sequence.
nextTurn :: Turn -> Turn
nextTurn NextL = NextS
nextTurn NextS = NextR
nextTurn NextR = NextL
