{-#
    LANGUAGE RebindableSyntax
#-}

import GHC.Exts
import GHC.Integer

infixr 0 $
($) :: (a -> b) -> a -> b
fn $ x = fn x

(.) :: (b -> c) -> (a -> b) -> a -> c
fn . gn = \a -> fn (gn a)

identity :: a -> a
identity x = x

const :: a -> b -> a
const x _ = x

subsitute :: (a -> b -> c) -> (a -> b) -> a -> c
subsitute x y z = x z (y z)

fix :: (a -> a) -> a
fix fn = let x = fn x in x


class Functor f where
  (<$>) :: (a -> b) -> f a -> f b

  (<$) :: a -> f b -> f a
  a <$ fa = const a <$> fa

map :: Functor f => (a -> b) -> f a -> f b
map = (<$>)


class Functor f => Applicative f where
  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b

app :: Applicative f => f (a -> b) -> f a -> f b
app = (<*>)


class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)


newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fn <$> Identity x = Identity (fn x)

instance Applicative Identity where
  pure = Identity

  Identity fn <*> Identity x = Identity (fn x)

instance Monad Identity where
  Identity x >>= fn = fn x
