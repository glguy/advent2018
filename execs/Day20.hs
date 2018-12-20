{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/20>
-}
{-# Language OverloadedStrings #-}
module Main (main) where

import Advent
import Advent.Coord
import Control.Monad
import Control.Applicative
import Text.Megaparsec.Char (newline)
import Text.Megaparsec (sepBy, sepBy1, between , eof)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Map (Map)
import qualified MonadLib as M

data Dir = N | S | E | W
  deriving (Eq, Ord, Show)

data Regexp a
  = One a
  | Alt (Regexp a) (Regexp a)
  | Seq (Regexp a) (Regexp a)
  | Empty
  deriving (Eq, Ord, Show)

-- | Print the answers to day 20
main :: IO ()
main =
  do input <- getParsedInput 20 parseRe0
     let (_, (_, doors)) = (M.runLift (M.runStateT mempty (M.findAll (wander input (C 0 0)))))
     let ds = distances (neighbor doors) (C 0 0)
     print (maximum ds)
     print (Map.size (Map.filter (>= 1000) ds))

parseRe0, parseRe1, parseRe2, parseRe3 :: Parser (Regexp Dir)
parseRe0 = "^" *> parseRe1 <* "$"
parseRe1 = foldr1 Alt <$> sepBy1 parseRe2 "|"
parseRe2 = foldr Seq Empty <$> many parseRe3
parseRe3 = between "(" ")" parseRe1 <|> One <$> parseDir

parseDir :: Parser Dir
parseDir = N <$ "N" <|> S <$ "S" <|> E <$ "E" <|> W <$ "W"

move :: Dir -> Coord -> Coord
move N = above
move S = below
move W = left
move E = right

type M = M.ChoiceT (M.StateT (Set (Coord, Regexp Dir), Set Coord) M.Lift)

addDoor :: Coord -> M ()
addDoor door =
  do (seen, doors) <- M.get
     M.set (seen, Set.insert door doors)

visit :: Coord -> Regexp Dir -> M ()
visit here re =
  do (seen,doors) <- M.get
     guard (Set.notMember (here,re) seen)
     M.set (Set.insert (here,re) seen, doors)

wander :: Regexp Dir -> Coord -> M Coord
wander re here =
  do visit here re
     case re of
       Empty   -> pure here
       One d   -> move d (move d here) <$ addDoor (move d here)
       Alt a b -> wander a here <|> wander b here
       Seq a b -> wander b =<< wander a here

neighbor :: Set Coord -> Coord -> [Coord]
neighbor doors here =
  [ move dir (move dir here)
    | dir <- [N,E,S,W]
    , Set.member (move dir here) doors
    ]

distances :: Ord a => (a -> [a]) -> a -> Map a Int
distances next start = go Map.empty [(start,0)] []
  where
    go seen [] [] = seen
    go seen [] others = go seen (reverse others) []
    go seen ((x,d):xs) others
      | Map.member x seen = go seen xs others
      | otherwise = d `seq`
          go (Map.insert x d seen) xs ([(n,d+1) | n <- next x] ++ others)
