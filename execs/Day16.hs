{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/16>
-}
{-# Language OverloadedStrings #-}
module Main (main) where

import           Advent               (Parser, getParsedInput, count, number)
import           Data.Maybe           (fromJust)
import           Data.List            (intersect, foldl')
import           Control.Monad        (foldM)
import           Control.Monad.Trans.State (StateT(..))
import           Data.Bits            ((.&.), (.|.))
import           Text.Megaparsec.Char (space, newline)
import           Text.Megaparsec      (between, many, sepBy, endBy, eof)
import qualified Data.IntMap as IntMap
import           Data.IntMap          (IntMap)
import qualified Data.Map as Map
import           Data.Map             (Map)
import qualified Data.Set as Set
import           Data.Set             (Set)

-- | Instructions are an opcode and 3 operands A B and C.
data Instruction = I !Int !Int !Int !Int deriving Show

-- | Examples are registers before, an instruction, and registers after.
data Example = E Registers Instruction Registers deriving Show

-- | Registers are represented using an IntMap. Assume no default values
type Registers = IntMap Int

-- | Print the answers to day 16
--
-- >>> :main
-- 592
-- 557
main :: IO ()
main =
  do (examples, instructions) <- getParsedInput 16 parseInput
     print (part1 examples)
     print (part2 examples instructions)

-- registers -----------------------------------------------------------

-- | Convert a 0-indexed list into Registers.
--
-- >>> toRegisters [1,2,4,8]
-- fromList [(0,1),(1,2),(2,4),(3,8)]
toRegisters :: [Int] -> Registers
toRegisters = IntMap.fromList . zip [0..]

-- parsing -------------------------------------------------------------

-- | Parser for an input file comprised of many examples followed by many
-- opcodes.
parseInput :: Parser ([Example], [Instruction])
parseInput = (,) <$> many parseExample     <* newline <* newline
                 <*> many parseInstruction <* eof

-- | Parse a single example entry from the input file.
parseExample :: Parser Example
parseExample =
  E <$> between "Before: " newline parseRegisters
    <*> parseInstruction
    <*> between "After:  " newline parseRegisters
    <*  newline

-- | Parse a single list of registers surrounded by square brackets
parseRegisters :: Parser Registers
parseRegisters = toRegisters <$> between "[" "]" (number `sepBy` ", ")

-- | Parse a single opcode and 3 operands.
parseInstruction :: Parser Instruction
parseInstruction = I <$> number <* space <*> number <* space
                     <*> number <* space <*> number <* newline

-- semantics -----------------------------------------------------------

-- | Pairs of opcode names and semantics function. Each function
-- expects the three operands A, B, C and the current registers
-- and produces the new registers.
opcodes :: Map String (Int -> Int -> Int -> Registers -> Registers)
opcodes =
  let sem f a b c regs = (IntMap.insert c $! f (regs IntMap.!) a b) regs
      val o = o in
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
  , ("seti", sem $ \reg a _ -> val a)

  , ("gtir", sem $ \reg a b -> if val a > reg b then 1 else 0)
  , ("gtri", sem $ \reg a b -> if reg a > val b then 1 else 0)
  , ("gtrr", sem $ \reg a b -> if reg a > reg b then 1 else 0)

  , ("eqir", sem $ \reg a b -> if val a == reg b then 1 else 0)
  , ("eqri", sem $ \reg a b -> if reg a == val b then 1 else 0)
  , ("eqrr", sem $ \reg a b -> if reg a == reg b then 1 else 0)
  ]

-- tasks ---------------------------------------------------------------

-- | How many samples in your puzzle input behave like three or more opcodes?
part1 :: [Example] -> Int
part1 = count $ \example -> 3 <= length (snd (getMatches example))

-- | What value is contained in register 0 after executing the test program?
part2 :: [Example] -> [Instruction] -> Int
part2 examples program = finalRegs IntMap.! 0 -- read final register 0
  where
    -- apply all of the instructions in order to a zero-initialized registers
    finalRegs = foldl' eval (toRegisters [0,0,0,0]) program

    -- Get constraints, satisfy them, lookup solution in opcode map
    semantics = (opcodes Map.!) <$> satConstraints (getConstraints examples)

    -- lookup the semantics for an instruction and apply it to the registers
    eval regs (I o a b c) = (semantics IntMap.! o) a b c regs

-- constraints and assignments -----------------------------------------

-- | Given an example extract the opcode and possible name for it.
--
-- >>> getMatches (E (toRegisters [3,2,1,1]) (I 9 2 1 2) (toRegisters [3,2,2,1]))
-- (9,["addi","mulr","seti"])
getMatches :: Example -> (Int, [String])
getMatches (E before (I o a b c) after) =
    (o, [ name | (name,f) <- Map.toList opcodes, after == f a b c before ])

-- | Get the constraints generated by a list of examples. Each opcode key is
-- associated with a list of possible opcode names.
getConstraints :: [Example] -> IntMap [String]
getConstraints = IntMap.fromListWith intersect . map getMatches

-- | Given some constraints, pick and element from each constraint so that each
-- element is unique. This function assumes a unique solution.
--
-- >>> satConstraints ["ab", "bc", "a"]
-- "bca"
satConstraints :: (Traversable t, Ord a) => t [a] -> t a
satConstraints constraints = fst (head (mapAccumLM pick constraints Set.empty))
  where
    pick :: Ord a => [a] -> Set a -> [(a, Set a)]
    pick possible soFar =
      [ (picked, Set.insert picked soFar)
          | picked <- possible
          , picked `Set.notMember` soFar ]

-- | Version of 'Data.Traversable.mapAccumL' that uses a 'Monad' instance
-- to combine the results of the function.
mapAccumLM ::
  (Traversable t, Monad m) =>
  (a -> acc -> m (b, acc)) -> t a -> acc -> m (t b, acc)
mapAccumLM f = runStateT . traverse (StateT . f)
