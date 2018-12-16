
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/16>
-}
{-# Language OverloadedStrings, BangPatterns, DeriveTraversable #-}
module Main (main) where

import           Advent               (Parser, getParsedInput, count, number)
import           Data.Maybe           (fromJust)
import           Data.List            (intersect, foldl')
import           Control.Monad        (foldM)
import           Data.Bits            ((.&.), (.|.))
import           Text.Megaparsec.Char (space, newline)
import           Text.Megaparsec      (many, sepBy, endBy, eof)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap, (!))

data Instruction = I !Int !Int !Int !Int
type Example     = ([Int], Instruction, [Int])
type Semantics   = Int -> Int -> Int -> IntMap Int -> IntMap Int

-- | Print the answers to day 16
main :: IO ()
main =
  do (examples, instructions) <- getParsedInput 16 parseLine
     print (part1 examples)
     print (part2 examples instructions)

parseLine :: Parser ([Example], [Instruction])
parseLine = (,) <$> many parseExample
                <* newline <* newline
                <*> endBy parseInstruction newline
                <* eof

parseExample :: Parser Example
parseExample =
  (,,) <$  "Before: [" <*> sepBy number ", " <* "]" <* newline
       <*> parseInstruction <* newline
       <*  "After:  [" <*> sepBy number ", " <* "]" <* newline
       <* newline

parseInstruction :: Parser Instruction
parseInstruction = I <$> number <* space <*> number <* space
                     <*> number <* space <*> number

opcodes :: [(String, Int -> Int -> Int -> IntMap Int -> IntMap Int)]
opcodes =
  [ ("addr", \a b c regs -> IntMap.insert c (regs ! a +   regs ! b) regs)
  , ("addi", \a b c regs -> IntMap.insert c (regs ! a +          b) regs)

  , ("mulr", \a b c regs -> IntMap.insert c (regs ! a *   regs ! b) regs)
  , ("muli", \a b c regs -> IntMap.insert c (regs ! a *          b) regs)

  , ("banr", \a b c regs -> IntMap.insert c (regs ! a .&. regs ! b) regs)
  , ("bani", \a b c regs -> IntMap.insert c (regs ! a .&.        b) regs)

  , ("borr", \a b c regs -> IntMap.insert c (regs ! a .|. regs ! b) regs)
  , ("bori", \a b c regs -> IntMap.insert c (regs ! a .|.        b) regs)

  , ("setr", \a _ c regs -> IntMap.insert c (regs ! a             ) regs)
  , ("seti", \a _ c regs -> IntMap.insert c (       a             ) regs)

  , ("gtir", \a b c regs -> IntMap.insert c (if        a > regs ! b then 1 else 0) regs)
  , ("gtri", \a b c regs -> IntMap.insert c (if regs ! a >        b then 1 else 0) regs)
  , ("gtrr", \a b c regs -> IntMap.insert c (if regs ! a > regs ! b then 1 else 0) regs)

  , ("eqir", \a b c regs -> IntMap.insert c (if        a == regs ! b then 1 else 0) regs)
  , ("eqri", \a b c regs -> IntMap.insert c (if regs ! a ==        b then 1 else 0) regs)
  , ("eqrr", \a b c regs -> IntMap.insert c (if regs ! a == regs ! b then 1 else 0) regs)
  ]

part1 :: [Example] -> Int
part1 examples = count p examples
  where
    p example = 3 <= length (getMatches example)

getMatches :: Example -> (Int, [String])
getMatches (before, I o a b c, after) =
    (o, [ name | (name,f) <- opcodes, after' == (f a b c before') ])
  where
    before' = IntMap.fromList (zip [0..] before)
    after'  = IntMap.fromList (zip [0..] after)

getConstraints :: [Example] -> IntMap [String]
getConstraints examples = IntMap.fromListWith intersect (map getMatches examples)

assignments :: [(Int, [String])] -> IntMap Semantics
assignments examples = fmap (\name -> fromJust (lookup name opcodes)) nameMap
  where
    [nameMap] = foldM pick IntMap.empty examples

    pick soFar (o, possible) =
      [ IntMap.insert o picked soFar
          | picked <- possible
          , picked `notElem` soFar ]

part2 :: [Example] -> [Instruction] -> Int
part2 examples program = finalRegs ! 0
  where
    finalRegs = foldl' eval (IntMap.fromList [(0,0),(1,0),(2,0),(3,0)]) program

    semantics = assignments (IntMap.toList (getConstraints examples))

    eval regs (I o a b c) = (semantics IntMap.! o) a b c regs
