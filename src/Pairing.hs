{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Data.Function

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

-- i.   extend curr      = id
-- ii.  curr . extend f  = f
-- iii. extend f . extend g = extend (f .  extend g)
class Functor w => Comonad w where
  curr :: w a -> a
  dup :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

  dup = extend id
  extend f = fmap f . dup



data Stream a = Cons a (Stream a) deriving Show

instance Functor Stream where
  fmap fn (Cons a as) = Cons (fn a) (fn <$> as)

instance Applicative Stream where
  pure = fix . Cons

  Cons fn fns <*> Cons a as = Cons (fn a) (fns <*> as)

instance Monad Stream where
  Cons a as >>= fn = fn a

instance Comonad Stream where
  curr (Cons a _) = a
  dup stream@(Cons a as) = Cons stream (dup as)


nat = let nat' x = Cons x (nat' $ x + 1) in nat' 1

sumNext stream = curr stream + next stream

next :: Stream a -> a
next (Cons _ (Cons a _)) = a



data Sequence a = End a | Next (Sequence a) deriving Show

instance Functor Sequence where
  fmap fn (End a) = End (fn a)
  fmap fn (Next next) = Next (fn <$> next)

instance Applicative Sequence where
  pure = End

  End fn <*> seq      = fn <$> seq
  Next nextFn <*> seq = Next (nextFn <*> seq)

instance Monad Sequence where
  End a     >>= fn = fn a
  Next next >>= fn = next >>= fn

instance Comonad Sequence where
  curr (End a) = a
  curr (Next next) = curr next

  dup seq = seq <$ seq



instance Pairing Sequence Stream where
  pair fn (End a)     (Cons b _) = fn a b
  pair fn (Next next) (Cons _ stream) = pair fn next stream


move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (flip const) movement (dup space)



data Tree a = Branch a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Branch a (Branch b _ _) (Branch c _ _)) = "Branch (" ++ show a ++ ") (Branch (" ++ show b ++ ") (...) (...)) (Branch (" ++ show c ++ ") (...) (...))"

instance Functor Tree where
  fmap fn (Branch a l r) = Branch (fn a) (fmap fn l) (fmap fn r)

instance Applicative Tree where
  pure a = let x = Branch a x x in x

  Branch fn lFn rFn <*> Branch a l r = Branch (fn a) (lFn <*> l) (rFn <*> r)

instance Monad Tree where
  Branch a _ _ >>= fn = fn a

instance Comonad Tree where
  curr (Branch a _ _) = a
  dup tree@(Branch _ l r) = Branch tree (dup l) (dup r)

int :: Tree Int
int = let int' x = Branch x (int' $ x - 1) (int' $ x + 1) in int' 0


data Path a = Fin a | Lft (Path a) | Rgt (Path a) deriving Show

instance Functor Path where
  fmap fn (Fin a)    = Fin (fn a)
  fmap fn (Lft next) = Lft (fmap fn next)
  fmap fn (Rgt next) = Rgt (fmap fn next)

instance Applicative Path where
  pure = Fin

  Fin fn     <*> path = fn <$> path
  Lft nextFn <*> path = Lft (nextFn <*> path)
  Rgt nextFn <*> path = Rgt (nextFn <*> path)

instance Monad Path where
  Fin a    >>= fn = fn a
  Lft next >>= fn = next >>= fn
  Rgt next >>= fn = next >>= fn



instance Pairing Path Tree where
  pair fn (Fin a)    (Branch b _ _) = fn a b
  pair fn (Lft next) (Branch _ l _) = pair fn next l
  pair fn (Rgt next) (Branch _ _ r) = pair fn next r



useless = useless' (Fin ())
  where
    useless' x = Branch x (useless' $ x <$ Lft ()) (useless' $ x <$ Rgt ())
