import Prelude hiding (head)
import Control.Comonad

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure x = Cons x (pure x)
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

instance Monad Stream where
  x >>= f = join (f <$> x)

instance Comonad Stream where
  extract = head

  duplicate stream@(Cons a as) = Cons stream (duplicate as)

head :: Stream a -> a
head (Cons x _) = x

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

join :: Stream (Stream a) -> Stream a
join (Cons (Cons x xs) (Cons _ xss)) = Cons x (join xss)


data Tream a = Node a (Tream a) (Tream a)

instance Functor Tream where
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tream where
  pure x = Node x (pure x) (pure x)
  (Node f lf rf) <*> (Node x lx rx) = Node (f x) (lf <*> lx) (rf <*> rl)

instance Monad Tream where
  x >>= f = join (f <$> x)

join :: Tream (Tream a) -> Tream a
join (Node (Node x lx rx)
