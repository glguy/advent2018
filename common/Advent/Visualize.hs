{-|
Module      : Advent.Visualize
Description : Module for visualizing components of the solutions
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Visualize
  ( drawArray
  , Image
  , PixelRGB8(..)
  , writePng
  ) where

import Advent.Coord
import Codec.Picture
import Data.Array.IArray

drawArray :: IArray a e => a Coord e -> (Coord -> e -> PixelRGB8) -> Image PixelRGB8
drawArray a f =
  generateImage toPixel  (hicol-locol+1) (hirow-lorow+1)
  where
    (C lorow locol, C hirow hicol) = bounds a

    toPixel col row = f i (a ! i)
      where
        i = C (lorow+row) (locol+col)
