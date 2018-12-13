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
import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import qualified Data.Vector as V
import           Data.Vector (Vector)

-- | Turns determine the behavior at an intersection
data Turn = NextL | NextR | NextS deriving Show

-- | Cart state includes the current direction of travel as well
-- as the next turn when an intersection is reached.
data Cart = Cart !Velocity !Turn deriving Show

-- | Coordinates are stored row then column in order to get
-- the correct ordering for carts in the simulation.
-- The y axis grows down as specified in the problem!
data Coord = C !Int !Int deriving (Eq, Ord, Show)

-- | Velocities are stored row then column to match coordinates
data Velocity = V !Int !Int deriving (Eq, Show)

-- | Road is a random-accessible representation of the track.
newtype Road = Road (Vector (Vector Char))

-- | Carts are stored in a where they will naturally be ordered
-- in the way that the simulation calls for.
type CartQueue = Map Coord Cart


-- | Print the answers to day 13
--
-- >>> :main
-- 50,54
-- 50,100
main :: IO ()
main =
  do road <- parseRoad <$> getInput 13
     let carts = findCarts road
     putStrLn (part1 road carts)
     putStrLn (part2 road carts)

-- | Format a coordinate into X,Y notation.
--
-- >>> format (C 10 20)
-- "20,10"
format :: Coord -> String
format (C y x) = show x ++ "," ++ show y

-- | Run the simulation and report the location of the first collision.
--
-- >>> let road = parseRoad ["><"] in part1 road (findCarts road)
-- "1,0"
-- >>> :{
-- let road = parseRoad
--       ["/->-\\        "
--       ,"|   |  /----\\"
--       ,"| /-+--+-\\  |"
--       ,"| | |  | v  |"
--       ,"\\-+-/  \\-+--/"
--       ,"  \\------/   "]
-- :}
--
-- >>> let carts = findCarts road
-- >>> part1 road carts
-- "7,3"
part1 :: Road -> CartQueue -> String
part1 road carts = format (simulate (\pos _ _ -> pos) road carts)

-- | Run the simulation and report the position of the final car.
--
-- >>> let road = parseRoad ["><>-"] in part2 road (findCarts road)
-- "3,0"
-- >>> :{
-- let road = parseRoad
--       ["/>-<\\  "
--       ,"|   |  "
--       ,"| /<+-\\"
--       ,"| | | v"
--       ,"\\>+</ |"
--       ,"  |   ^"
--       ,"  \\<->/"]
-- :}
--
-- >>> let carts = findCarts road
-- >>> part2 road carts
-- "6,4"
part2 :: Road -> CartQueue -> String
part2 road carts = format (simulate onWreck road carts)
  where
    -- when a car wrecks, clear that location and resume the simulation
    onWreck pos ready done =
      tick onWreck road (Map.delete pos ready) (Map.delete pos done)

-- | Parse the input file as a 'Road'
parseRoad :: [String] -> Road
parseRoad = Road . V.fromList . map V.fromList

-- | Look up the road element at a particular coordinate
indexRoad :: Road -> Coord -> Char
indexRoad (Road v) (C i j) = v V.! i V.! j

-- | Find all the initial locations and directions of the carts.
findCarts :: Road -> CartQueue
findCarts (Road rs) =
  Map.fromList
    [ (C y x, Cart dir NextL)
    | (y,r) <- zip [0..] (V.toList rs)
    , (x,c) <- zip [0..] (V.toList r)
    , dir   <- case c of
                 '^' -> [north]
                 'v' -> [south]
                 '>' -> [east ]
                 '<' -> [west ]
                 _   -> []
    ]

-- | Run the simulation to completion. Take the wreck behavior
-- as a parameter to allow part1 and part2 to share the same
-- simulation. When a cart collides with another control of
-- the simulation will switch to the wreck parameter.
simulate ::
  (Coord -> CartQueue -> CartQueue -> Coord)
            {- ^ wreck behavior: position, ready queue, done queue -} ->
  Road      {- ^ road                                              -} ->
  CartQueue {- ^ starting cart states                              -} ->
  Coord     {- ^ final cart position                               -}
simulate onWreck road carts
  | [pos] <- Map.keys carts = pos
  | otherwise               = tick onWreck road carts Map.empty

-- | Run a single tick of the simulation.
tick ::
  (Coord -> CartQueue -> CartQueue -> Coord)
            {- ^ wreck behavior: position, ready queue, done queue -} ->
  Road      {- ^ road                                              -} ->
  CartQueue {- ^ carts ready to  move                              -} ->
  CartQueue {- ^ carts moved this tick                             -} ->
  Coord     {- ^ final coordinate answer                           -}
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
drive road pos (Cart dir next) = (pos', Cart dir' next')
  where
    pos' = pos `addVelocity` dir

    (dir', next') =
      case indexRoad road pos' of
        '\\' -> (invert    dir, next         )
        '/'  -> (invert'   dir, next         )
        '+'  -> (turn next dir, nextTurn next)
        _    -> (dir          , next         )

-- | Apply a turn to a direction.
turn :: Turn -> Velocity -> Velocity
turn NextL = turnLeft
turn NextR = turnRight
turn NextS = id

-- | Invert a velocity along a line y=x. (remember y grows down)
--
-- >>> map invert [north, south, east, west] == [west, east, south, north]
-- True
invert :: Velocity -> Velocity
invert (V dy dx) = V dx dy

-- | Invert a velocity along a line y = -x (remember y grows down)
-- >>> map invert' [north, south, east, west] == [east, west, north, south]
-- True
invert' :: Velocity -> Velocity
invert' (V dy dx) = V (-dx) (-dy)

-- | Change a direction counter-clockwise
--
-- >>> take 4 (iterate turnLeft north) == [north, west, south, east]
-- True
turnLeft :: Velocity -> Velocity
turnLeft (V dy dx) = V (-dx) dy

-- | Change a direction clockwise
--
-- >>> take 4 (iterate turnRight north) == [north, east, south, west]
-- True
turnRight :: Velocity -> Velocity
turnRight (V dy dx) = V dx (-dy)

-- | Advance a turn direction to the next one in sequence.
nextTurn :: Turn -> Turn
nextTurn NextL = NextS
nextTurn NextS = NextR
nextTurn NextR = NextL

-- | Add a velocity to a coordinate.
--
-- >>> addVelocity (C 10 20) north
-- C 9 20
addVelocity :: Coord -> Velocity -> Coord
addVelocity (C y x) (V dy dx) = C (y + dy) (x + dx)

-- | Unit vectors in cardinal directions.
north, east, south, west :: Velocity
north = V (-1) 0
south = V 1 0
east  = V 0 1
west  = V 0 (-1)
