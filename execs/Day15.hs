{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/15>
-}
{-# Language MultiWayIf #-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Data.Maybe
import           Data.Ord
import           Data.Function
import           Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Map (Map)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Concurrent

data Team = Elf | Goblin
  deriving (Eq, Ord, Show)

data Unit = Unit { attack, hp :: !Int, team :: !Team }
  deriving (Show)

data Tile = Open | Wall
  deriving (Eq, Ord, Show)

type Dungeon = Vector (Vector Tile)

-- | Print the answers to day 15
main :: IO ()
main =
  do input <- Vector.fromList . map Vector.fromList <$> getInput 15
     let dungeon = parseMap input
         units = parseUnits input

     print (part1 dungeon units)
     print (part2 dungeon units)

part1 :: Dungeon -> Map Coord Team -> Int
part1 dungeon units = outcome units2 turns
  where
    units1 = fmap (Unit 3 200) units
    (units2, turns) = last (simulate dungeon units1 0)

part2 :: Dungeon -> Map Coord Team -> Int
part2 dungeon units = search elfCount 3 Nothing
  where
    elfCount = Map.size (Map.filter (Elf==) units)

    search elfCount atkLo (Just (atkHi, answer))
      | atkLo + 1 == atkHi = answer

    search elfCount atkLo mbAtkHi
      | null trimmed -- perfect elf victory
      = search elfCount atkLo (Just (atk, outcome units2 turns))
      | otherwise = search elfCount atk mbAtkHi
      where
        atk = case mbAtkHi of
                Nothing       -> atkLo * 2
                Just (atkHi, _) -> (atkLo + atkHi) `quot` 2
        toUnit t = case t of Elf -> Unit atk 200 t; Goblin -> Unit 3 200 t
        units1 = fmap toUnit units
        allElves (us,_) = Map.size (Map.filter (\u -> team u == Elf) us)
                          == elfCount
        (ticks, trimmed) = span allElves (simulate dungeon units1 0)

        (units2, turns) = last ticks


outcome :: Map Coord Unit -> Int -> Int
outcome units turns = turns * sum (fmap hp units)

parseMap :: Vector (Vector Char) -> Dungeon
parseMap = fmap (fmap (\x -> if x == '#' then Wall else Open))

parseUnits :: Vector (Vector Char) -> Map Coord Team
parseUnits rs = Map.fromList
  [ (C y x, unit)
    | (y,r) <- Vector.toList (Vector.indexed rs)
    , (x,c) <- Vector.toList (Vector.indexed r)
    , unit <- case c of
                'G' -> [Goblin]
                'E' -> [Elf   ]
                _   -> [      ]
    ]

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

-- | Attack the unit at a coordinate for a given amount of damage.
-- Once a unit no longer has positive hit points it is removed from
-- the map of units.
melee ::
  Int            {- ^ damage              -} ->
  Coord          {- ^ target's coordinate -} ->
  Map Coord Unit {- ^ all units           -} ->
  Map Coord Unit {- ^ updated units       -}
melee atk = Map.update $ \tgt ->
  if hp tgt <= atk
    then Nothing
    else Just $! tgt { hp = hp tgt - atk }

-- | Figure out what neighboring unit this unit wants to attack
target ::
  Coord {- ^ coordinate of attacking unit -} ->
  Unit -> Map Coord Unit -> Maybe Coord
target pos unit units
  | null possible = Nothing
  | otherwise     = Just $! minimumBy ordering possible
  where
    ordering = comparing (\i -> (hp (units Map.! i), i))
    possible = filter (isEnemy unit units) (cardinal pos)

-- | Determine if a given coordinate contains an enemy of the given unit.
isEnemy ::
  Unit           {- ^ focused unit            -} ->
  Map Coord Unit {- ^ all units               -} ->
  Coord          {- ^ possible enemy location -} ->
  Bool           {- ^ enemy at location       -}
isEnemy unit units loc =
  case Map.lookup loc units of
    Just u  -> team u /= team unit
    Nothing -> False

-- | Figure out where, if anywhere, this unit wants to move
route ::
  Coord          {- ^ unit's position -} ->
  Unit           {- ^ unit stats      -} ->
  Map Coord Unit {- ^ all units       -} ->
  Dungeon        {- ^ dungeon map     -} ->
  Maybe Coord    {- ^ next location   -}
route pos unit units dungeon
  | isNear pos = Nothing
  | otherwise  = search Set.empty candidates
  where
    candidates = Set.fromList [ (0, start, start) | start <- cardinal pos ]
    isOpen loc = inDungeon dungeon loc && Map.notMember loc units
    isNear = any (isEnemy unit units) . cardinal

    search seen q =
      do ((dist, dest, start), q) <- Set.minView q
         if | Set.member dest seen || not (isOpen dest) -> search seen q
            | isNear dest -> Just start
            | otherwise -> search (Set.insert dest seen)
                                 (foldl' (flip Set.insert) q
                                     [ (dist+1, dest, start) | dest <- cardinal dest ])

inDungeon :: Dungeon -> Coord -> Bool
inDungeon dungeon (C y x) = col == Open
  where
    row = dungeon Vector.! y
    col = row     Vector.! x
