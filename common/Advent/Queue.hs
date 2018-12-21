{-|
Module      : Advent.Queue
Description : Banker's queue implementation
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Queue (Queue, singleton, empty, fromList, snoc, pop, appendList) where

import Data.Foldable

-- | FIFO Queue implementation
data Queue a = Queue [a] !Int [a] !Int

instance Foldable Queue where
  toList (Queue l _ r _) = l ++ reverse r
  foldr f z (Queue l _ r _) = foldr f (foldl (flip f) z r) l

-- | Renders using 'fromList' syntax
instance Show a => Show (Queue a) where
  showsPrec p q
    = showParen (p >= 11)
    $ showString "fromList "
    . shows (toList q)

-- | Construct a queue from a single element.
singleton :: a -> Queue a
singleton x = Queue [x] 1 [] 0

-- | Construct an empty queue
empty :: Queue a
empty = Queue [] 0 [] 0

-- | Construct a queue from a list. The head of the list will
-- be the first element returned by 'pop'
fromList :: [a] -> Queue a
fromList xs = Queue xs (length xs) [] 0

-- | Internal smart constructor for building queues that maintain
-- the invariant that the rear component is never longer than the
-- front component.
mkQueue :: Queue a -> Queue a
mkQueue q@(Queue f lenF r lenR)
  | lenR <= lenF = q
  | otherwise    = Queue (f ++ reverse r) (lenF + lenR) [] 0

-- | Add a new element to the end of a queue.
snoc :: a -> Queue a -> Queue a
snoc x (Queue f lenF r lenR) = mkQueue (Queue f lenF (x:r) (1+lenR))

-- | Append many items onto a queue. The items will pop from the queue
-- in the same order as they are in the given list.
appendList :: [a] -> Queue a -> Queue a
appendList xs q = foldl' (flip snoc) q xs

-- | Remove an element from the front of a queue and a new queue
-- without that element.
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) lenF r lenR) = Just (x, mkQueue (Queue f (lenF-1) r lenR))
pop _                         = Nothing
