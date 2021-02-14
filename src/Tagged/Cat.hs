{-# LANGUAGE TypeFamilies, FlexibleContexts , FunctionalDependencies,
   MultiParamTypeClasses, FlexibleInstances #-}

import GHC.Exts (IsList(..))
import Control.Monad (join)
import Data.Monoid

class Monoid t => Tagged a t | a -> t where
  tag :: a -> t

data Cat t a
    = Empty
    | Leaf t a
    | Pair t (Cat t a) (Cat t a)

tagOf :: Monoid t => Cat t a -> t
tagOf Empty        = mempty
tagOf (Leaf t _)   = t
tagOf (Pair t _ _) = t


leaf :: Tagged a t => a -> Cat t a
leaf x = Leaf (tag x) x

pair :: Tagged a t => Cat t a -> Cat t a -> Cat t a
pair l r = Pair (tagOf l <> tagOf r) l r

instance (Show t, Show a) => Show (Cat t a) where
  show Empty        = "(Empty)"
  show (Leaf t a)   = join ["(Leaf ", show t, " ", show a, ")"]
  show (Pair t l r) = join ["(Pair ", show t, " ", show l, " ", show r, ")"]

instance Tagged a t => Semigroup (Cat t a) where
  Empty <> x = x
  x <> Empty = x
  x <> y     = pair x y

instance Tagged a t => Monoid (Cat t a) where
  mempty = Empty

instance Foldable (Cat t) where
  foldMap _  Empty        = mempty
  foldMap fn (Leaf _ a)   = fn a
  foldMap fn (Pair _ l r) = foldMap fn l <> foldMap fn r

instance Tagged a t => IsList (Cat t a) where
  type Item (Cat t a) = a

  fromList = foldMap leaf
  toList = foldMap pure

instance Tagged Integer (Sum Integer) where
  tag = Sum

instance Tagged Integer (Product Integer) where
  tag = Product

-- Tests
test :: Cat (Sum Integer) Integer
test = leaf 10

test2 = test <> test
