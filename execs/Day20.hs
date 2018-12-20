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

import           Advent (Parser, getParsedInput)
import           Advent.Coord (Coord(C), above, below, left, right, origin, addCoord, boundingBox, cardinal)
import           Advent.Visualize (writePng, generateImage, Image, PixelRGB8(..))
import           Advent.Queue (Queue)
import qualified Advent.Queue as Queue
import           Data.Map (Map)
import           Data.Foldable (foldl')
import           Data.Set (Set)
import           Data.Word (Word8)
import           Control.Monad (foldM)
import           Text.Megaparsec ((<|>), many, sepBy1, between , eof)
import           Text.Megaparsec.Char (newline)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Cardinal directions: north south east west
data Dir = N | S | E | W
  deriving (Eq, Ord, Show)

-- | Regular expressions parameterized by the underlying elements
newtype Regexp a = RE [[Either a (Regexp a)]]
  deriving (Eq, Ord, Show)

-- | Print the answers to day 20
--
-- >>> :main
-- 4121
-- 8636
main :: IO ()
main =
  do input <- getParsedInput 20 parseRe0
     let (doors, _) = route (Set.singleton origin) input
     let ds = distances (neighbor doors) (C 0 0)

     writePng "output.png" (draw doors ds)

     print (maximum ds)
     print (Map.size (Map.filter (>= 1000) ds))

-- Regular expression parsing for each level of precedence
parseRe0 :: Parser (Regexp Dir)
parseRe0 = "^" *> parseRe1 <* "$"
parseRe1 = RE <$> many parseRe2 `sepBy1` "|"
parseRe2 = Right <$> between "(" ")" parseRe1 <|> Left <$> parseDir

-- | Parse a cardinal direction
parseDir :: Parser Dir
parseDir = N <$ "N" <|> S <$ "S" <|> E <$ "E" <|> W <$ "W"

-- | Move one space giving a direction and a starting coordinate
move :: Dir -> Coord -> Coord
move N = above
move S = below
move W = left
move E = right

-- | Given the set of doors, generate a list of rooms reachable from a
-- given room
neighbor :: Set Coord -> Coord -> [Coord]
neighbor doors here =
  [ move dir (move dir here)
    | dir <- [N,E,S,W]
    , Set.member (move dir here) doors ]

-- | Given a neighbors generating function compute the minimum distances
-- to all reachable locations.
distances :: Ord a => (a -> [a]) -> a -> Map a Int
distances next start = go Map.empty (Queue.singleton (start,0))
  where
    go seen q =
      case Queue.pop q of
        Nothing -> seen
        Just ((x,d),q)
          | Map.member x seen -> go seen q
          | otherwise -> d `seq`
              go (Map.insert x d seen) (foldl' (flip Queue.snoc) q [(n,d+1) | n <- next x])

-- | Given a regular expression, compute a set of generated doors and end points
-- generated from the regular expression when starting at the origin.
route :: Set Coord -> Regexp Dir -> (Set Coord, Set Coord)
route starts (RE alts) = foldMap (foldM routeFrom starts) alts

-- Given a set of starting points and a new direction or sub-expression
-- compute the reachable doors and the ending coordinates
routeFrom :: Set Coord -> Either Dir (Regexp Dir) -> (Set Coord, Set Coord)
routeFrom starts = either (dirStep starts) (route starts)

-- | Generate the door passed thorugh and the end point when taking a step from the origin
-- in the given direction.
dirStep :: Set Coord -> Dir -> (Set Coord, Set Coord)
dirStep starts d = ( Set.mapMonotonic (move d) starts -- doors
                   , Set.mapMonotonic (move d . move d) starts) -- endpoint

-- Rendering -----------------------------------------------------------

-- | Render the maze using adjacent colors to represent rooms with similar distances
-- from the origin
draw :: Set Coord -> Map Coord Int -> Image PixelRGB8
draw doors rooms = generateImage toPixel width height
  where
    Just (C loy lox, C hiy hix) = boundingBox doors
    width  = hix - lox + 1
    height = hiy - loy + 1
    maxD   = maximum rooms
    isRoom = any (`Set.member` doors) . cardinal
    toPixel x y
      | c == origin                  = PixelRGB8 255 0 0
      | Set.member c doors           = PixelRGB8 255 255 255
      | Just d <- Map.lookup c rooms = colorWheel (fromIntegral (255 * d `quot` maxD))
      | otherwise                    = PixelRGB8 0 0 0
      where
        c = C (loy+y) (lox+x)

colorWheel :: Word8 -> PixelRGB8
colorWheel i
  | i < 85    = PixelRGB8 (255 - i * 3) 0 (i * 3)
  | i < 170   = PixelRGB8 0 ((i-85) * 3) (255 - (i-85)*3)
  | otherwise = PixelRGB8 ((i-170) * 3) (255 - (i-170)*3) 0
