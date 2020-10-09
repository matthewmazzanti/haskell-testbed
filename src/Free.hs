{-# LANGUAGE UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  #-}

import Data.Functor.Identity

data Free f a = Pure a
              | Free (f (Free f a))

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
  show (Pure x) = "(Pure " ++ (show x) ++ ")"
  show (Free xs) = "(Free " ++ (show xs) ++ ")"

instance Functor f => Functor (Free f) where
  fmap fn (Pure x)  = Pure $ fn x
  fmap fn (Free xs) = Free $ (fn <$>) <$> xs

instance Functor f => Applicative (Free f) where
  pure = Pure

  Pure fn <*> Pure x = Pure $ fn x
  Pure fn <*> Free x = Free $ (fmap fn) <$> x
  Free fns <*> xs    = Free $ (<*> xs) <$> fns

instance Functor f => Monad (Free f) where
  Pure x  >>= fn = fn x
  Free xs >>= fn = Free $ (>>= fn) <$> xs


class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend fn = fmap fn . duplicate


data Cofree f a = Cofree a (f (Cofree f a))

instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
  show (Cofree x xs) = "(Cofree " ++ show x ++ " " ++ show xs ++ ")"

instance Functor f => Functor (Cofree f) where
  fmap fn (Cofree x xs) = Cofree (fn x) ((fn <$>) <$> xs)
  x <$ (Cofree _ ys) = Cofree x ((x <$) <$> ys)

instance Functor f => Comonad (Cofree f) where
  extract (Cofree x _) = x

  duplicate xs@(Cofree _ ys) = Cofree xs (duplicate <$> ys)


class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> c) -> f a -> g b -> c

instance Pairing f g => Pairing (Free f) (Cofree g) where
  pair fn (Pure x) (Cofree y _) = fn x y
  pair fn (Free xs) (Cofree _ ys) = pair (pair fn) xs ys

k :: a -> b -> a
k x _ = x

k' :: a -> b -> b
k' _ x = x

x :: Free Identity Int
x = Free (Identity (Pure 10))

cox :: Cofree Identity Int
cox = Cofree 1 (((+1) <$>) <$> Identity cox)
