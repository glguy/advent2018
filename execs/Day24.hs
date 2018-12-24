{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/24>
-}
{-# Language RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Advent
import Data.Maybe
import Data.Ord
import Data.List
import Control.Applicative
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import           Text.Megaparsec
import           Text.Megaparsec.Char (newline)
import Debug.Trace

data Group = Group
  { size          :: !Int
  , hp            :: !Int
  , special       :: [(Element, Effect)]
  , attack        :: !Int
  , attackElement :: !Element
  , initiative    :: !Int
  , team          :: !Team
  }
  deriving (Show, Eq)

data Team = Good | Bad deriving (Eq, Ord, Show)

data Effect = Weak | Immune deriving (Eq, Ord, Show)

data Element = Bludgeoning | Fire | Slashing | Radiation | Cold
  deriving (Eq, Ord, Show)

-- | Print the answers to day 24
--
-- >>> :main
-- 16747
-- 5923
main :: IO ()
main =
  do groups <- getParsedInput 24 parseInput
     print (evaluate (simulate groups))
     print (evaluate (search (attemptBoost groups) 1 Nothing))

-- | Find the sum of the sizes of all the groups.
evaluate :: [Group] -> Int
evaluate = sum . map size

-- | Determine the lowest value that satisfies the given
-- predicate and return the result.
search ::
  (Int -> Maybe a) {- ^ satisfication condition -} ->
  Int              {- ^ known too low bound     -} ->
  Maybe (Int, a)   {- ^ known satisfying int    -} ->
  a                {- satisfying result         -}
search f tooLo (Just (hi, best)) | tooLo + 1 == hi = best
search f tooLo mbHi =
  case f i of
    Nothing   -> search f i mbHi
    Just best -> search f tooLo (Just (i, best))
  where
    i = case mbHi of
          Nothing     -> tooLo*2
          Just (hi,_) -> (tooLo+1 + hi) `quot` 2

-- | Test if a group is on the reindeer team.
isGood :: Group -> Bool
isGood g = Good == team g

-- | Determine if a boost is enough to allow the reindeer to
-- win. If it is return the final group of reindeer.
attemptBoost :: [Group] -> Int -> Maybe [Group]
attemptBoost groups boost
  | all isGood outcome = Just outcome
  | otherwise          = Nothing
  where
    boostGood g
      | isGood g  = g { attack = attack g + boost }
      | otherwise = g

    outcome = simulate (map boostGood groups)

-- | Run a battle until it stops making progress due to immunities
-- or due to a team being wiped out.
simulate :: [Group] -> [Group]
simulate groups
  | groups == groups' = groups
  | otherwise = simulate groups'
  where
    groups' = combat groups

-- | Determine the effectiveness multiplier of an attack element against
-- a particular group.
effectiveness :: Group -> Group -> Int
effectiveness atk def =
  case lookup (attackElement atk) (special def) of
    Just Immune -> 0
    Nothing     -> 1
    Just Weak   -> 2

-- | Compute effective power of a group
effectivePower :: Group -> Int
effectivePower grp = size grp * attack grp

-- | Order a list of groups by the order they get to chose their
-- targets.
targetSelectionOrder :: [Group] -> [Group]
targetSelectionOrder groups = sortOn prj groups
  where
    prj grp = (negate (effectivePower grp), negate (initiative grp))

-- | Given a list of groups generate a targetting assignment.
targetSelection ::
  [Group]      {- ^ unordered groups        -} ->
  [(Int, Int)] {- ^ attacker / defender IDs -}
targetSelection groups =
  foldr aux (\_ -> []) (targetSelectionOrder groups) groups
  where
  aux atk next groups =
    case targetChoice atk groups of
      Nothing -> next groups
      Just def -> (initiative atk, initiative def)
                : next (delete def groups)

-- | Given a group and the list of remaining target choices, determine
-- the chosen group, if any.
targetChoice :: Group -> [Group] -> Maybe Group
targetChoice atk def
  | null def' = Nothing
  | otherwise = Just $! choice
  where
    prj def = (effectiveness atk def, effectivePower def, initiative def)
    choice = maximumBy (comparing prj) def'
    def' = filter (\g -> effectiveness atk g > 0
                      && team g /= team atk) def

-- | Given an unordered list of groups, compute the result of combat
combat :: [Group] -> [Group]
combat groups =
  foldr aux IntMap.elems combatOrder (toIntMap groups)
  where
    selection = targetSelection groups

    combatOrder = sortBy (flip compare) (map initiative groups)

    toIntMap xs = IntMap.fromList [ (initiative g, g) | g <- xs ]

    aux atkid next groups =
      fromMaybe (next groups) $
        do defid <- lookup atkid selection
           atk <- IntMap.lookup atkid groups
           def <- IntMap.lookup defid groups
           let dmg = effectiveness atk def * effectivePower atk
               killed = min (size def) (dmg `quot` hp def)
               size' = size def - killed
           if size' > 0
               then return (next (IntMap.insert defid def { size = size' } groups))
               else return (next (IntMap.delete defid groups))

-- input parsing -------------------------------------------------------

parseInput :: Parser [Group]
parseInput =
  do immuneSys <- "Immune System:\n" *> endBy (parseUnitLine Good) newline
     newline
     infection <- "Infection:\n"     *> endBy (parseUnitLine Bad ) newline
     eof
     return (immuneSys ++ infection)

parseUnitLine :: Team -> Parser Group
parseUnitLine team =
  do size          <- number       <* " units each with "
     hp            <- number       <* " hit points "
     special       <- parseSpecial <* "with an attack that does "
     attack        <- number       <* " "
     attackElement <- parseElement <* " damage at initiative "
     initiative    <- number
     return Group{..}

parseSpecial :: Parser [(Element, Effect)]
parseSpecial = option [] (between "(" ") " (concat <$> sepBy1 parseSpecial1 "; "))

parseSpecial1 :: Parser [(Element, Effect)]
parseSpecial1 =
  do effect   <- parseEffect <* " to "
     elements <- parseElement `sepBy1` ", "
     return [ (element, effect) | element <- elements ]

parseElement :: Parser Element
parseElement =
  Bludgeoning <$ "bludgeoning" <|>
  Fire        <$ "fire"        <|>
  Slashing    <$ "slashing"    <|>
  Radiation   <$ "radiation"   <|>
  Cold        <$ "cold"

parseEffect :: Parser Effect
parseEffect = Weak <$ "weak" <|> Immune <$ "immune"
