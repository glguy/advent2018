{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/21>

I copied my Day 19 solution and then modified it to check the state
of the running program.

-}
{-# Language OverloadedStrings #-}
module Main (main) where

import           Advent               (Parser, getParsedInput, number)
import           Data.Bits            ((.&.), (.|.))
import           Data.Char            (chr, ord)
import           Data.IntMap          (IntMap)
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.IntSet          as IntSet
import           Data.Vector          (Vector)
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Generic  as Vector
import qualified Data.Vector.Generic.Mutable as M
import           Numeric              (showHex)
import           Text.Megaparsec      ((<|>), endBy, eof)
import           Text.Megaparsec.Char (newline, space)

type Registers = U.Vector Int

data Instruction = I Op !Int !Int !Int

data Op
  = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
  | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
  deriving Show

-- | Print the answers to day 21
main :: IO ()
main =
  do (ip, pgm) <- getParsedInput 21 parseInput

     putStrLn (renderProgram ip pgm)

     let regs = Vector.replicate 6 0
     let xs = run ip (fmap semantics pgm) regs
     print (head xs)      -- part 1
     print (findCycle xs) -- part 2

-- | Find the last integer in the list that occurs before a repeated integer.
--
-- >>> findCycle [1,2,3,4,2]
-- 4
findCycle :: [Int] -> Int
findCycle = go 0 IntSet.empty
  where
    go answer seen (x:xs)
      | IntSet.member x seen = answer
      | otherwise = go x (IntSet.insert x seen) xs

-- | Given a program counter register and a program, run the program
-- until the instruction pointer points outside of the program.
-- The list of values of register 5 during program counter 29 is returned.
run :: Int -> Vector (Registers -> Registers) -> Registers -> [Int]
run ip pgm regs =
  case pgm Vector.!? pc of
    Nothing -> []
    Just f
      | pc == 29 -> regs Vector.! 5 : run ip pgm (nextIP (f regs))
      | otherwise -> run ip pgm (nextIP (f regs))
  where
    pc = Vector.unsafeIndex regs ip
    nextIP regs = chg regs ip (1+)

chg vec i f = U.modify (\v -> M.unsafeModify v f i) vec
set vec i e = U.modify (\v -> M.unsafeWrite v i e) vec

-- | Map from opcode names to opcode semantics. The functions expect
-- the operands A, B, and C as well as the current registers.
semantics :: Instruction -> Registers -> Registers
semantics (I op a b c) = sem $ \reg ->
  case op of
    Addr -> reg a + reg b
    Addi -> reg a + val b

    Mulr -> reg a * reg b
    Muli -> reg a * val b

    Banr -> reg a .&. reg b
    Bani -> reg a .&. val b

    Borr -> reg a .|. reg b
    Bori -> reg a .|. val b

    Setr -> reg a
    Seti -> val a

    Gtir -> if val a > reg b then 1 else 0
    Gtri -> if reg a > val b then 1 else 0
    Gtrr -> if reg a > reg b then 1 else 0

    Eqir -> if val a == reg b then 1 else 0
    Eqri -> if reg a == val b then 1 else 0
    Eqrr -> if reg a == reg b then 1 else 0
  where
    sem f regs = set regs c $! f (Vector.unsafeIndex regs)
    val = id

-- parsing -------------------------------------------------------------

-- | Parse the input as an instruction pointer register and a vector
-- of register updating functions.
parseInput :: Parser (Int, Vector Instruction)
parseInput =
  do ip  <- "#ip" *> space *> number <* newline
     pgm <- endBy parseInstruction newline <* eof
     pure (ip, Vector.fromList pgm)

-- | Parse a register updating function
parseInstruction :: Parser Instruction
parseInstruction = I <$> parseOp <*> operand <*> operand <*> operand
  where
    operand = space *> number

parseOp :: Parser Op
parseOp =
  Addr <$ "addr" <|> Addi <$ "addi" <|> Mulr <$ "mulr" <|>
  Muli <$ "muli" <|> Banr <$ "banr" <|> Bani <$ "bani" <|>
  Borr <$ "borr" <|> Bori <$ "bori" <|> Setr <$ "setr" <|>
  Seti <$ "seti" <|> Gtir <$ "gtir" <|> Gtri <$ "gtri" <|>
  Gtrr <$ "gtrr" <|> Eqir <$ "eqir" <|> Eqri <$ "eqri" <|>
  Eqrr <$ "eqrr"

renderProgram :: Int -> Vector Instruction -> String
renderProgram ip pgm = unlines
  [ "L_" ++ showHex n (":\t" ++ renderInstruction ip i)
   | (n,i) <- Vector.toList (Vector.indexed pgm) ]

renderInstruction :: Int -> Instruction -> String
renderInstruction ip (I op a b c) =
  case op of
    Addr -> bin reg "+" reg
    Addi -> bin reg "+" val

    Mulr -> bin reg "*" reg
    Muli -> bin reg "*" val

    Banr -> bin reg "&" reg
    Bani -> bin reg "&" val

    Borr -> bin reg "|" reg
    Bori -> bin reg "|" val

    Setr -> un reg
    Seti -> un val

    Gtir -> bin val ">" reg
    Gtri -> bin reg ">" val
    Gtrr -> bin reg ">" reg

    Eqir -> bin val "==" reg
    Eqri -> bin reg "==" val
    Eqrr -> bin reg "==" reg
  where
    reg i = if i == ip then "IP" else [chr (ord 'a' + i)]
    val i = "0x" ++ showHex i ""
    assign v = reg c ++ " = " ++ v

    un f = assign (f a)
    bin f1 o f2 = assign (f1 a ++ " " ++ o ++ " " ++ f2 b)
