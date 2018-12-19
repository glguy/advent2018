{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/19>
-}
{-# Language OverloadedStrings, BangPatterns, DeriveTraversable #-}
module Main (main) where
-- ip 2
import Advent
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import           Data.Map (Map)
import           Data.IntMap (IntMap)
import           Data.Bits
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

-- | Print the answers to day 19
main :: IO ()
main =
  do pgm <- Vector.fromList . map runOne . map words <$> getInput 19
     let regs = IntMap.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]
     print (run pgm regs)

ip :: Int
ip = 2

run :: Vector (Registers -> Registers) -> Registers -> Registers
run pgm regs =
  let p = regs IntMap.! ip in
  case pgm Vector.!? fromIntegral p of
    Nothing -> regs
    Just f -> run pgm (f regs)

runOne :: [String] -> Registers -> Registers
runOne [o,a,b,c] = nextIP . sem (read a) (read b) (read c)
  where
    nextIP = IntMap.update (Just . succ) ip
    Just sem = Map.lookup o opcodes
runOne bad = error ("bad instruction: " ++ unwords bad)

opcodes :: Map String (Int -> Int -> Int -> Registers -> Registers)
opcodes =
  let sem f a b c regs = (IntMap.insert c $! f (regs IntMap.!) a b) regs
      val o = fromIntegral o in
  Map.fromList
  [ ("addr", sem $ \reg a b -> reg a + reg b)
  , ("addi", sem $ \reg a b -> reg a + val b)

  , ("mulr", sem $ \reg a b -> reg a * reg b)
  , ("muli", sem $ \reg a b -> reg a * val b)

  , ("banr", sem $ \reg a b -> reg a .&. reg b)
  , ("bani", sem $ \reg a b -> reg a .&. val b)

  , ("borr", sem $ \reg a b -> reg a .|. reg b)
  , ("bori", sem $ \reg a b -> reg a .|. val b)

  , ("setr", sem $ \reg a _ -> reg a)
  , ("seti", sem $ \_   a _ -> val a)

  , ("gtir", sem $ \reg a b -> if val a > reg b then 1 else 0)
  , ("gtri", sem $ \reg a b -> if reg a > val b then 1 else 0)
  , ("gtrr", sem $ \reg a b -> if reg a > reg b then 1 else 0)

  , ("eqir", sem $ \reg a b -> if val a == reg b then 1 else 0)
  , ("eqri", sem $ \reg a b -> if reg a == val b then 1 else 0)
  , ("eqrr", sem $ \reg a b -> if reg a == reg b then 1 else 0)
  ]

type Registers = IntMap Integer

