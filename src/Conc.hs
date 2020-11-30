{-# LANGUAGE TypeFamilies #-}

module Conc where

import Prelude hiding (head, last, (!!))
import GHC.Exts (IsList(..))
import Data.Foldable (foldl', foldr')
import Debug.Trace
import qualified Data.List as L

data Cat a
    = Empty
    | Single a
    | Pair !Int !Int !(Cat a) !(Cat a) deriving Show

height :: Cat a -> Int
height (Empty)        = 0
height (Single _)     = 0
height (Pair h _ _ _) = h

size :: Cat a -> Int
size (Empty)        = 0
size (Single _)     = 1
size (Pair _ s _ _) = s

unpair :: Cat a -> (Cat a, Cat a)
unpair (Pair _ _ l r) = (l, r)

pair :: Cat a -> Cat a -> Cat a
pair xs ys = Pair h s xs ys
  where h = 1 + max (height xs) (height ys)
        s = size xs + size ys

instance Semigroup (Cat a) where
  Empty <> ys = ys
  xs <> Empty = xs
  xs <> ys
    | diff < -1 = left xs ys  -- Deconstruct left, concat to right
    | diff >  1 = right xs ys -- Deconstruct right, concat to left
    | otherwise = pair xs ys  -- Height valid, construct pair
    where
      diff = height ys - height xs

      left xs ys
        | height l >= height r        = pair l (r <> ys)
        | height xs - 3 == height nrr = pair l (pair rl nrr)
        | otherwise                   = pair (pair l rl) nrr
        where (l, r)   = unpair xs
              (rl, rr) = unpair r
              nrr      = rr <> ys

      right xs ys
        | height r >= height l        = pair (xs <> l) r
        | height ys - 3 == height nll = pair (pair nll lr) r
        | otherwise                   = pair nll (pair lr r)
        where (l, r)   = unpair ys
              (ll, lr) = unpair l
              nll      = xs <> ll

instance Monoid (Cat a) where
  mempty = Empty

instance Functor Cat where
  fmap _ Empty = Empty
  fmap fn (Single a) = Single (fn a)
  fmap fn (Pair h s l r) = Pair h s (fmap fn l) (fmap fn r)

instance Applicative Cat where
  pure = Single

  Empty <*> _         = Empty
  _ <*> Empty         = Empty
  Single fn    <*> as = fn <$> as
  Pair _ _ l r <*> as = (l <*> as) <> (r <*> as)

instance Monad Cat where
  Empty        >>= _  = Empty
  Single a     >>= fn = fn a
  Pair _ _ l r >>= fn = (l >>= fn) <> (r >>= fn)

instance Foldable Cat where
  foldMap _  Empty          = mempty
  foldMap fn (Single a)     = fn a
  foldMap fn (Pair _ _ l r) = foldMap fn l <> foldMap fn r

instance Traversable Cat where
  traverse _  Empty          = pure Empty
  traverse fn (Single a)     = Single <$> fn a
  traverse fn (Pair h s l r) = Pair h s <$> traverse fn l <*> traverse fn r



instance IsList (Cat a) where
  type Item (Cat a) = a

  fromList = collect . foldl' append [] . (pure <$>)
    where
      append :: [Cat a] -> Cat a -> [Cat a]
      append [] new = [new]
      append (cat:cats) new
        | height cat > height new  = new:cat:cats
        | otherwise                = append cats (cat <> new)

      collect :: [Cat a] -> Cat a
      collect = foldl' (flip (<>)) mempty

  toList = foldMap pure

cons :: a -> Cat a -> Cat a
cons a = (pure a <>)

snoc :: a -> Cat a -> Cat a
snoc a = (<> pure a)

head :: Cat a -> Maybe a
head (Empty)        = Nothing
head (Single a)     = Just a
head (Pair _ _ l _) = head l

last :: Cat a -> Maybe a
last Empty      = Nothing
last (Single a) = Just a
last (Pair _ _ _ r) = last r

(!!) :: Cat a -> Int -> Maybe a
(!!) (Single a) 0 = Just a
(!!) (Pair _ _ l r) n
  | n < 0      = Nothing
  | n < sl = l !! n
  | otherwise  = r !! (n - sl)
  where sl = size l
(!!) _ _ = Nothing

append :: Cat a -> a -> Cat a
append cat x = cat <> pure x

prepend :: Cat a -> a -> Cat a
prepend cat x = pure x <> cat
