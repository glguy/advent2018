{-|
Module      : Advent.Coord
Description : Row-major coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Coord where

import Data.Ix
import GHC.Arr

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

coordRow, coordCol :: Coord -> Int
coordRow (C row _) = row
coordCol (C _ col) = col

instance Ix Coord where
  unsafeIndex (C lorow locol, C hirow hicol) (C row col) =
    (row - lorow) * (hicol - locol + 1) + (col - locol)

  inRange (C lorow locol, C hirow hicol) (C row col) =
    lorow <= row && row <= hirow &&
    locol <= col && col <= hicol

  range (C lorow locol, C hirow hicol) =
    [ C row col | row <- [lorow..hirow], col <- [locol..hicol]]

above, below, left, right :: Coord -> Coord
above (C y x) = C (y-1)  x
below (C y x) = C (y+1)  x
left  (C y x) = C  y    (x-1)
right (C y x) = C  y    (x+1)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)

-- | Compute the cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the cardinal neighbors of a coordinate: north, south, east, west
neighbors :: Coord -> [Coord]
neighbors c = c `seq` [above c, left c, right c, below c,
                       above (left c), above (right c),
                       below (left c), below (right c)]
