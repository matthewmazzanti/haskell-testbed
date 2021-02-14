{-# LANGUAGE QuantifiedConstraints #-}

module Seq.Seq where

import Prelude hiding
    ( head
    , tail
    , last
    , init
    , truncate
    , take
    , drop
    )

class (Monad s, Traversable s, forall a. Monoid (s a)) => Seq s where
  length :: s a -> Integer

  head :: s a -> Maybe a
  tail :: s a -> Maybe (s a)
  last :: s a -> Maybe a
  init :: s a -> Maybe (s a)
  (!) :: s a -> Integer -> Maybe a

  -- reverse :: s a -> s a
  -- intersperse :: a -> s a -> s a
  -- transpose :: s (s a) -> s (s a)
  -- subsequences :: s a -> s (s a)
  -- permutations :: s a -> s (s a)

  insert :: s a -> a -> Integer -> Maybe (s a)
  remove :: s a -> Integer -> (s a, Maybe a)
  append :: s a -> a -> s a
  truncate :: s a -> (s a, Maybe a)
  push :: s a -> a -> s a
  pop :: s a -> (s a, Maybe a)

  split :: Integer -> s a -> (s a, s a)
  take :: Integer -> s a -> s a
  drop :: Integer -> s a -> s a

  sort :: Ord a => s a -> s a

index :: Seq s => s a -> Integer -> Maybe a
index = (!)
