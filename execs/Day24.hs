
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
  }
  deriving (Show, Eq)

data Team = Good | Bad deriving (Eq, Ord, Show)

data Effect = Weak | Immune deriving (Eq, Ord, Show)

data Element
  = Bludgeoning
  | Fire
  | Slashing
  | Radiation
  | Cold
  deriving (Eq, Ord, Show)

-- answer 45 or 46

-- | Print the answers to day 24
main :: IO ()
main =
  do (good, bad) <- getParsedInput 24 parseInput
     -- input <- getInput 24

     print (search (attempt good bad) 1 Nothing)

search f tooLo mbHi | traceShow (tooLo, mbHi) False = undefined
search f tooLo (Just hi)
  | tooLo + 1 == hi = f hi

search f tooLo mbHi =
  case f i of
    Nothing -> search f i mbHi
    Just _  -> search f tooLo (Just i)
  where
    i = case mbHi of
          Nothing -> tooLo*2
          Just hi -> (tooLo+1 + hi) `quot` 2


attempt good bad boost = run good' bad
  where
    good' = map (\g -> g { attack = attack g + boost }) good

    run good bad
      | null good = Nothing
      | null bad  = Just (sum (map size good))
      | otherwise =
           case combat good bad of
             (a,b) | a == good && b == bad -> Nothing
                   | otherwise -> (run a b)

parseInput :: Parser ([Group], [Group])
parseInput =
  do "Immune System:" *> newline
     immune <- endBy parseUnitLine newline
     newline
     "Infection:" *> newline
     infection <- endBy parseUnitLine newline
     eof
     return (immune, infection)

parseUnitLine :: Parser Group
parseUnitLine =
  do size <- number <* " units each with "
     hp <- number <* " hit points "
     special <- concat <$> option [] (between "(" ") " (sepBy1 parseSpecials "; "))
              <* "with an attack that does "
     attack <- number <* " "
     attackElement <- parseElement <* " damage at initiative "
     initiative <- number
     return Group{..}

parseElement :: Parser Element
parseElement =
  Bludgeoning <$ "bludgeoning" <|>
  Fire        <$ "fire"        <|>
  Slashing    <$ "slashing"    <|>
  Radiation   <$ "radiation"   <|>
  Cold        <$ "cold"

parseEffect :: Parser Effect
parseEffect = Weak <$ "weak" <|> Immune <$ "immune"

parseSpecials :: Parser [(Element, Effect)]
parseSpecials =
  do effect   <- parseEffect <* " to "
     elements <- parseElement `sepBy1` ", "
     return [ (element, effect) | element <- elements ]

effectiveness :: Element -> Group -> Int
effectiveness elt grp =
  case lookup elt (special grp) of
    Just Immune -> 0
    Nothing     -> 1
    Just Weak   -> 2

effectivePower :: Group -> Int
effectivePower grp = size grp * attack grp

targetSelectionOrder :: [Group] -> [Group] -> [(Team, Group)]
targetSelectionOrder good bad =
  sortOn (prj . snd) $
    [(Good, grp) | grp <- good] ++
    [(Bad,  grp) | grp <- bad ]
  where
    prj grp = (negate (effectivePower grp), negate (initiative grp))

targetSelection :: [Group] -> [Group] -> [(Team, Int, Int)]
targetSelection good bad =
  foldr aux (\_ _ -> []) (targetSelectionOrder good bad) good bad
  where
  aux (Good, atk) next good bad =
    case targetChoice atk bad of
      Nothing -> next good bad
      Just def -> (Good, initiative atk, initiative def)
                : next good (delete def bad)
  aux (Bad, atk) next good bad =
    case targetChoice atk good of
      Nothing -> next good bad
      Just def -> (Bad, initiative atk, initiative def)
                : next (delete def good) bad

targetChoice :: Group -> [Group] -> Maybe Group
targetChoice atk def
  | null def' = Nothing
  | otherwise = Just $! choice
  where
    prj grp = (effectiveness (attackElement atk) grp, effectivePower grp, initiative grp)
    choice = maximumBy (comparing prj) def'
    def' = filter (\g -> effectiveness (attackElement atk) g > 0) def

combat :: [Group] -> [Group] -> ([Group], [Group])
combat good bad =
  foldr aux (\good bad -> (IntMap.elems good, IntMap.elems bad)) combatOrder
        (toIntMap good) (toIntMap bad)
  where
    selection = targetSelection good bad

    combatOrder =
      map (\(t,grp) -> (t, initiative grp)) $
      sortOn (\(_,grp) -> negate (initiative grp)) $
      [ (Good, grp) | grp <- good ] ++
      [ (Bad, grp) | grp <- bad ]

    toIntMap :: [Group] -> IntMap Group
    toIntMap xs = IntMap.fromList [ (initiative g, g) | g <- xs ]

    getTarget (team, atkid) = listToMaybe [ defid | (team, atkid', defid) <- selection
                                                  , atkid == atkid' ]

    aux (Good, atkid) next good bad =
      fromMaybe (next good bad) $
        do defid <- getTarget (Good, atkid)
           atk <- IntMap.lookup atkid good
           def <- IntMap.lookup defid bad
           let dmg = effectiveness (attackElement atk) def
                   * effectivePower atk
               killed = min (size def) (dmg `quot` hp def)
               size' = size def - killed
           if size' > 0
               then return (next good (IntMap.insert defid def { size = size' } bad))
               else return (next good (IntMap.delete defid bad))

    aux (Bad, atkid) next good bad =
      fromMaybe (next good bad) $
        do defid <- getTarget (Bad, atkid)
           atk <- IntMap.lookup atkid bad
           def <- IntMap.lookup defid good
           let dmg = effectiveness (attackElement atk) def
                   * effectivePower atk
               killed = min (size def) (dmg `quot` hp def)
               size' = size def - killed
           if size' > 0
               then return (next (IntMap.insert defid def { size = size' } good) bad)
               else return (next (IntMap.delete defid good) bad)
