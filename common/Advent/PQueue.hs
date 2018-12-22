{-|
Module      : Advent.PQueue
Description : Int-priority min queue
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}
{-# Language PatternSynonyms, ViewPatterns #-}
module Advent.PQueue
  (PQueue(Empty, (:<|)), Advent.PQueue.null, singleton, insert, view) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

newtype PQueue a = PQ (IntMap [a])

{-# Complete Empty, (:<|) #-}

pattern Empty :: PQueue a
pattern Empty <- (Advent.PQueue.null -> True)
  where
    Empty = PQ IntMap.empty

pattern (:<|) :: a -> PQueue a -> PQueue a
pattern v :<| q <- (view -> Just (v,q))

null :: PQueue a -> Bool
null (PQ q) = IntMap.null q

singleton :: Int -> a -> PQueue a
singleton p v = PQ (IntMap.singleton p [v])

insert :: Int -> a -> PQueue a -> PQueue a
insert k v (PQ q) = PQ (IntMap.alter aux k q)
  where
    aux Nothing   = Just [v]
    aux (Just vs) = Just (v:vs)

view :: PQueue a -> Maybe (a, PQueue a)
view (PQ q) =
  do ((k,xs),q1) <- IntMap.minViewWithKey q
     case xs of
       [] -> error "Malformed queue"
       [x] -> Just (x, PQ q1)
       x:xs -> let q2 = PQ (IntMap.insert k xs q1)
               in q2 `seq` Just (x,q2)
