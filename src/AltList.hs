{-# LANGUAGE QuantifiedConstraints
  , FlexibleInstances
  , UndecidableInstances
  , TypeFamilies
  , OverloadedLists
  , FlexibleContexts
  #-}

import Prelude hiding (NonEmpty)
import GHC.Exts (IsList(..), coerce)
import Data.Functor.Compose

data NonEmpty a
    = Pair a (NonEmpty a)
    | Leaf a
    deriving Show

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a

  fromList [] = error "Must be non empty"
  fromList (y:[]) = (Leaf y)
  fromList (x:xs) = Pair x (fromList xs)

  toList (Leaf x) = [x]
  toList (Pair x xs) = x:toList xs


instance Semigroup (NonEmpty a) where
  Pair x xs <> ys = Pair x (xs <> ys)
  Leaf x    <> ys = Pair x ys

instance Functor NonEmpty where
  fmap fn (Pair x xs) = Pair (fn x) (fmap fn xs)
  fmap fn (Leaf x)    = Leaf (fn x)

instance Applicative NonEmpty where
  pure = Leaf

  Pair fn fns <*> xs = (fn <$> xs) <> (fns <*> xs)
  Leaf fn     <*> xs = fn <$> xs

instance Monad NonEmpty where
  Pair x xs >>= fn = fn x <> (xs >>= fn)
  Leaf x    >>= fn = fn x

instance Foldable NonEmpty where
  foldMap fn (Pair x xs) = fn x <> foldMap fn xs
  foldMap fn (Leaf x)    = fn x


newtype Empty f a = Empty (Maybe (f a)) deriving Show

instance Semigroup (f a) => Semigroup (Empty f a) where
  Empty x <> Empty y = Empty (go x y)
    where
      go Nothing x = x
      go x Nothing = x
      go (Just x) (Just y) = Just (x <> y)

instance Semigroup (f a) => Monoid (Empty f a) where
  mempty = Empty Nothing

instance Functor f => Functor (Empty f) where
  fmap fn (Empty x) = Empty (fmap fn <$> x)

instance Applicative f => Applicative (Empty f) where
  pure = Empty . Just . pure

  Empty fns <*> Empty xs = Empty (go fns xs)
    where
      go Nothing _ = Nothing
      go _ Nothing = Nothing
      go (Just fns) (Just xs) = Just (fns <*> xs)

instance Foldable f => Foldable (Empty f) where
  foldMap fn (Empty xs) = foldMap (foldMap fn) xs

instance (Foldable f, forall a. Semigroup (f a), Applicative f) => Monad (Empty f) where
  xs >>= fn = foldMap fn xs

instance IsList (f a) => IsList (Empty f a) where
  type Item (Empty f a) = Item (f a)

  fromList [] = Empty $ Nothing
  fromList xs = Empty $ Just $ fromList xs

  toList (Empty (Nothing)) = []
  toList (Empty (Just xs)) = toList xs

type List a = Empty NonEmpty a

-- []
test1 :: List Int
test1 = []

test2 :: List Int
test2 = [10,20,30]

test3 :: List Int
test3 = [10,20,30,40,50]

testFn1 x = [x, x*2, x*3]
testFn2 x = test1
