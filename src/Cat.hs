{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  , GADTs
  #-}

import Data.Monoid
import Control.Applicative

newtype Nil ne a = Nil (Maybe (ne a))

instance Semigroup (ne a) => Semigroup (Nil ne a) where
  Nil as <> Nil bs = Nil (as <> bs)

instance Semigroup (ne a) => Monoid (Nil ne a) where
  mempty = Nil Nothing

instance Functor ne => Functor (Nil ne) where
  fmap fn (Nil mne) = Nil ((fn <$>) <$> mne)

instance Applicative ne => Applicative (Nil ne) where
  pure = Nil . Just . pure

  Nil fns <*> Nil as = Nil ((<*>) <$> fns <*> as)


data NonEmpty a
    = End a
    | Cons a (NonEmpty a)
    deriving Show

instance Semigroup (NonEmpty a) where
  End a     <> ne = Cons a ne
  Cons a as <> ne = Cons a (as <> ne)

instance Functor NonEmpty where
  fmap fn (End a)     = End (fn a)
  fmap fn (Cons a ne) = Cons (fn a) (fn <$> ne)

instance Applicative NonEmpty where
  pure = End

  End fn      <*> ne = fn <$> ne
  Cons fn fns <*> ne = applyThen ne
    where
      applyThen (End a)     = Cons (fn a) (fns <*> ne)
      applyThen (Cons a as) = Cons (fn a) (applyThen as)

instance Monad NonEmpty where
  End a     >>= fn = fn a
  Cons a as >>= fn = fn a <> (as >>= fn)

instance Foldable NonEmpty where
  foldMap fn (End a)     = fn a
  foldMap fn (Cons a as) = fn a <> foldMap fn as

instance Traversable NonEmpty where
  traverse fn (End a)     = End <$> fn a
  traverse fn (Cons a as) = Cons <$> fn a <*> traverse fn as



data CatNE a
    = Leaf a
    | Pair (CatNE a) (CatNE a) deriving Show

-- toNonEmpty :: CatNE a -> NonEmpty a
-- toNonEmpty (Leaf a) = a :| []
-- toNonEmpty (Pair l r) = toNonEmpty l <> toNonEmpty r
-- 
-- fromNonEmpty :: NonEmpty a -> CatNE a
-- fromNonEmpty (a:|[]) = Leaf a
-- fromNonEmpty (a:|as) = Leaf a <> fromNonEmpty as


instance Semigroup (CatNE a) where
  (<>) = Pair

instance Functor CatNE where
  fmap fn (Leaf a) = Leaf (fn a)
  fmap fn (Pair l r) = Pair (fn <$> l) (fn <$> r)

instance Applicative CatNE where
  pure = Leaf

  Leaf fn      <*> cat = fn <$> cat
  Pair lFn rFn <*> cat = (lFn <*> cat) <> (rFn <*> cat)

instance Monad CatNE where
  Leaf a >>= fn = fn a
  Pair l r >>= fn = (l >>= fn) <> (r >>= fn)

instance Foldable CatNE where
  foldMap fn (Leaf a) = fn a
  foldMap fn (Pair l r) = (foldMap fn l) <> (foldMap fn r)

instance Traversable CatNE where
  traverse fn (Leaf a) = Leaf <$> fn a
  traverse fn (Pair l r) = Pair <$> traverse fn l <*> traverse fn r

