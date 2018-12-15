
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/15>
-}
module Main (main) where

import           Advent
import           Data.Maybe
import           Data.Ord
import           Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Map (Map)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

data Team = Elf | Goblin
  deriving (Eq, Ord, Show)

data Unit = Unit { attack, hp :: !Int, team :: !Team }
  deriving (Show)

data Tile = Open | Wall
  deriving (Eq, Ord, Show)

type Coord = (Int, Int)

type Dungeon = Vector (Vector Tile)

-- | Print the answers to day 15
main :: IO ()
main =
  do input <- Vector.fromList . map Vector.fromList <$> getInput 15
     let dungeon = parseMap input
         units = parseUnits input

     answer1 <- part1 dungeon units
     answer2 <- part2 dungeon units
     print answer1
     print answer2

part1 :: Dungeon -> Map Coord Team -> IO Int
part1 dungeon units =
  do let units1 = fmap (Unit 3 200) units
     (units1, turns1) <- animate dungeon $ simulate dungeon units1 0
     return (outcome units1 turns1)

part2 :: Dungeon -> Map Coord Team -> IO Int
part2 dungeon units =
  do let elfCount = Map.size (Map.filter (Elf==) units)
     search elfCount 3
  where
    search elfCount atk =
      do let toUnit t = case t of Elf -> Unit atk 200 t; Goblin -> Unit 3 200 t
             units2 = fmap toUnit units
             allElves (us,_) = Map.size (Map.filter (\u -> team u == Elf) us)
                               == elfCount
             (ticks, trimmed) = span allElves (simulate dungeon units2 0)
         (units2, turns2) <- animate dungeon ticks
         if null trimmed
           then return (outcome units2 turns2)
           else search elfCount (atk+1)

outcome :: Map Coord Unit -> Int -> Int
outcome units turns = turns * sum (fmap hp units)

animate :: Dungeon -> [(Map Coord Unit, Int)] -> IO (Map Coord Unit, Int)
animate d [(u,t)]    = putStrLn (draw d u) >> return (u,t)
animate d ((u,t):xs) = putStrLn (draw d u) >> animate d xs

draw :: Dungeon -> Map (Int,Int) Unit -> String
draw dungeon units =
  unlines
    [[ case fmap team (Map.lookup (y,x) units) of
        Just Elf -> 'E'
        Just Goblin -> 'G'
        Nothing -> if c == Wall then '#' else '.'
       | (x,c) <- Vector.toList (Vector.indexed r) ]
       ++ drawStats y
    | (y,r) <- Vector.toList (Vector.indexed dungeon)
    ]
  where
    drawStats y = intercalate "," [ drawU u | ((y',x'),u) <- Map.toList units, y == y' ]
    drawU u = ' '
            : (case team u of Elf -> 'E'; Goblin -> 'G')
            : "(" ++ show (hp u) ++ ")"


parseMap :: Vector (Vector Char) -> Dungeon
parseMap = fmap (fmap (\x -> if x == '#' then Wall else Open))

parseUnits :: Vector (Vector Char) -> Map (Int,Int) Team
parseUnits rs = Map.fromList
  [ ((y,x),unit)
    | (y,r) <- Vector.toList (Vector.indexed rs)
    , (x,c) <- Vector.toList (Vector.indexed r)
    , unit <- case c of
                'G' -> [Goblin]
                'E' -> [Elf]
                _ -> [] ]

simulate :: Dungeon -> Map Coord Unit -> Int -> [(Map Coord Unit, Int)]
simulate dungeon units turns =
  (units, turns) : tick dungeon (Map.keysSet units) units turns

tick :: Dungeon -> Set Coord -> Map Coord Unit -> Int -> [(Map Coord Unit, Int)]
tick dungeon schedule units turns =
  case Set.minView schedule of
    Nothing -> simulate dungeon units (turns+1)
    _ | same (fmap team units) -> [(units,turns)]
    Just (pos, schedule) ->
      let unit = units Map.! pos
          pos' = fromMaybe pos (route pos unit (Map.delete pos units) dungeon)
          units' = Map.insert pos' unit (Map.delete pos units)
      in case target pos' unit units of
            Just tgt ->
               let units'' = melee (attack unit) tgt units'
                   schedule' = Set.intersection schedule (Map.keysSet units'')
               in tick dungeon schedule' units'' turns
            Nothing ->
               tick dungeon schedule units' turns

-- | Attack the unit at a coordinate for a given amount of damage
melee :: Int -> Coord -> Map Coord Unit -> Map Coord Unit
melee atk pos units = Map.alter aux pos units
  where
    aux Nothing = Nothing
    aux (Just tgt)
      | hp tgt <= atk = Nothing
      | otherwise = Just tgt { hp = hp tgt - atk }

-- | Figure out what neighboring unit this unit wants to attack
target :: Coord -> Unit -> Map Coord Unit -> Maybe Coord
target pos unit units
  | null possible = Nothing
  | otherwise     = Just $! minimumBy ordering possible
  where
    ordering = comparing (\i -> (hp (units Map.! i), i))
    possible = filter isEnemy (cardinal pos)
    isEnemy loc =
      case Map.lookup loc units of
        Just u -> team u /= team unit
        Nothing -> False

layers :: Coord -> (Coord -> Bool) -> [Set Coord]
layers start valid = go (Set.singleton start)
  where
    go seen
      | null nextLayer = []
      | otherwise = nextLayer : go (Set.union nextLayer seen)
      where
        nextLayer =
          Set.fromList
            [ p1 | p  <- Set.toList seen
                 , p1 <- cardinal p
                 , valid p1 ]
          `Set.difference` seen

-- | Figure out where, if anywhere, this unit wants to move
route :: Coord -> Unit -> Map (Int, Int) Unit -> Dungeon -> Maybe Coord
route pos unit units dungeon
  | any isEnemy (cardinal pos) || isNothing destination = Nothing
  | otherwise = Just nextMove

  where
    -- best next step to make progress to the target square
    nextMove = head [next | next <- cardinal pos
                      , isOpen next
                      , distance (fromJust destination) next == distToTarget - 1 ]

    -- Distance to the square the unit chose to move to
    distToTarget = distance (fromJust destination) pos

    -- Compute the distance to a particular starting from a given start
    distance tgt start = distance' tgt Set.empty [(start,0)] []

    distance' tgt seen [] others = distance' tgt seen (reverse others) []
    distance' tgt seen ((here,dist):q) others
            | tgt == here = dist
            | isOpen here, Set.notMember here seen
                        = distance' tgt (Set.insert here seen) q
                                        ([(c,dist+1) | c <- cardinal here ]
                                         ++ others)
            | otherwise = distance' tgt seen q others

    -- Square that the unit wants to move to
    destination =
         find (any isEnemy . cardinal)
       $ concatMap Set.toList
       $ layers pos isOpen

    isOpen loc = inDungeon dungeon loc && not (Map.member loc units)

    isEnemy loc =
      case Map.lookup loc units of
            Just u -> team u /= team unit
            Nothing -> False


cardinal :: (Int, Int) -> [(Int, Int)]
cardinal (y,x) = [ (y-1,x), (y,x-1), (y,x+1), (y+1,x) ]

inDungeon :: Dungeon -> Coord -> Bool
inDungeon dungeon (y,x) =
  fromMaybe undefined $
    do row <- dungeon Vector.!? y
       col <- row Vector.!? x
       pure (col == Open)
