{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  #-}

import Control.Comonad

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

data Gen v i a
    = Done a
    | And (v i) (i -> Gen v i a)

instance Functor (Gen v i) where
  fmap fn (Done a) = Done (fn a)
  fmap fn (And view next) = And view ((fn <$>) . next)

instance Applicative (Gen v i) where
  pure = Done

  Done fn <*> as = fn <$> as
  And view nextFn <*> as = And view ((<*> as) <$> nextFn)

instance Monad (Gen v i) where
  Done a >>= fn = fn a
  And view next >>= fn = And view ((>>= fn) . next)


data CoGen v i a = CoGen
    { coDone :: a
    , coAnd :: v i -> (i, CoGen v i a)
    }

instance Functor (CoGen v i) where
  fmap fn (CoGen a coAnd) = CoGen (fn a) coAnd'
    where coAnd' = fmap (fmap fn) . coAnd

instance Comonad (CoGen v i) where
  extract = coDone

  duplicate coGen@(CoGen _ coAnd) = CoGen coGen coAnd'
    where coAnd' = fmap duplicate . coAnd

instance Pairing  (CoGen v i) (Gen v i) where
  pair fn (CoGen a _) (Done b) = fn a b
  pair fn (CoGen _ coAnd) (And view next) = pair fn coGen gen
      where (i, coGen) = coAnd view
            gen = next i

gen :: Gen [] Int Int
gen = And [10] (\_ -> Done 20)
