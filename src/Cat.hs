{-# LANGUAGE TypeFamilies #-}

import GHC.Exts (IsList(..))

data Cat a
    = Empty
    | Leaf a
    | Pair (Cat a) (Cat a)
    deriving Show

instance Semigroup (Cat a) where
  Empty <> x = x
  x <> Empty = x
  x <> y     = Pair x y

instance Monoid (Cat a) where
  mempty = Empty

instance Functor Cat where
  fmap _  Empty      = Empty
  fmap fn (Leaf a)   = Leaf (fn a)
  fmap fn (Pair l r) = Pair (fn <$> l) (fn <$> r)

instance Applicative Cat where
  pure = Leaf

  Empty          <*> _     = Empty
  _              <*> Empty = Empty
  Leaf fn        <*> xs    = fn <$> xs
  Pair lFns rFns <*> xs    = (lFns <*> xs) <> (rFns <*> xs)

instance Monad Cat where
  Empty    >>= _  = Empty
  Leaf a   >>= fn = fn a
  Pair l r >>= fn = (l >>= fn) <> (r >>= fn)

instance Foldable Cat where
  foldMap _  Empty      = mempty
  foldMap fn (Leaf a)   = fn a
  foldMap fn (Pair l r) = foldMap fn l <> foldMap fn r

instance Traversable Cat where
  traverse _  Empty      = pure Empty
  traverse fn (Leaf a)   = Leaf <$> fn a
  traverse fn (Pair l r) = Pair <$> traverse fn l <*> traverse fn r

instance IsList (Cat a) where
  type Item (Cat a) = a

  fromList = foldMap pure
  toList = foldMap pure
