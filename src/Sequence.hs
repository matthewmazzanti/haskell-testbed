{-# LANGUAGE QuantifiedConstraints, FlexibleInstances, UndecidableInstances #-}

import GHC.Exts (Constraint)

import Prelude hiding
    ( Semigroup, Monoid, Functor, Applicative, Monad, Foldable
    , (<$>), (>>=), join, id
    )

identity :: a -> a
identity x = x

class Empty m where
  empty :: m

class Semigroup s where
  (<>) :: s -> s -> s

class (Empty m, Semigroup m ) => Monoid m where


class Pure m where
  pure :: a -> m a

class Map f where
  (<$>) :: (a -> b) -> f a -> f b

class (Pure m, Map m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) mn fn = join (fn <$> mn)

  join :: m (m a) -> m a
  join = (>>= identity)

  {-# MINIMAL join | (>>=) #-}


class Fold f where
  fold :: Monoid m => f m -> m

class (Fold f, Map f) => FoldMap f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap fn xs = fold (fn <$> xs)

instance (Pure m, Fold m, Map m, forall a. Monoid (m a)) => Monad m where
  join = fold
