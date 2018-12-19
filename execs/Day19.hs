{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/19>

I finished part 2 with manual inspection, this only implements part 1.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import           Advent               (Parser, getParsedInput, number)
import           Data.Bits            ((.&.), (.|.))
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           Text.Megaparsec      (endBy, eof, some)
import           Text.Megaparsec.Char (letterChar, newline, space)

type Registers = IntMap Int

-- | Print the answers to day 19
main :: IO ()
main =
  do (ip, pgm) <- getParsedInput 19 parseInput
     let regs = IntMap.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]
     print (run ip pgm regs IntMap.! 0)

-- | Parse the input as an instruction pointer register and a vector
-- of register updating functions.
parseInput :: Parser (Int, Vector (Registers -> Registers))
parseInput =
  do ip  <- "#ip" *> space *> number <* newline
     pgm <- endBy parseInstruction newline <* eof
     pure (ip, Vector.fromList pgm)

-- | Parse a register updating function
parseInstruction :: Parser (Registers -> Registers)
parseInstruction =
  do name <- some letterChar
     let operand = space *> number
     case Map.lookup name opcodes of
       Just f  -> f <$> operand <*> operand <*> operand
       Nothing -> fail "unknown instruction"

-- | Given a program counter register and a program, run the program
-- until the instruction pointer points outside of the program. The
-- final state of the registers is returned.
run :: Int -> Vector (Registers -> Registers) -> Registers -> Registers
run ip pgm regs =
  case pgm Vector.!? (regs IntMap.! ip) of
    Nothing -> regs
    Just f  -> run ip pgm (nextIP (f regs))
  where
    nextIP = IntMap.update (Just . (1+)) ip

-- | Map from opcode names to opcode semantics. The functions expect
-- the operands A, B, and C as well as the current registers.
opcodes :: Map String (Int -> Int -> Int -> Registers -> Registers)
opcodes = Map.fromList
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
  where
    sem f a b c regs = (IntMap.insert c $! f (regs IntMap.!) a b) regs
    val v            = v
