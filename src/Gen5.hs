{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  , GADTs
  #-}

import Control.Monad ((>=>))
import Data.Foldable ()
import Data.Bifunctor (first)

class View v where
  runView :: Show a => v a -> IO [a]

instance View Rose where
  runView view = do
      let t = traces view
      putStrLn $ "choose: " ++ show (snd <$> t)
      fst . (t !!) . read <$> getLine


data Gen v a where
  Done :: a -> Gen v a
  Next :: (Show i, View v) => v (j, i) -> (i -> Gen v a) -> Gen v a

instance Functor (Gen v) where
  fmap fn (Done a)      = Done (fn a)
  fmap fn (Next v next) = Next v (fmap fn . next)

instance Applicative (Gen v) where
  pure = Done

  Done fn       <*> gen = fn <$> gen
  Next v nextFn <*> gen = Next v ((<*> gen) . nextFn)

instance Monad (Gen v) where
  Done a      >>= fn = fn a
  Next v next >>= fn = Next v (next >=> fn)

next :: (Show i, View v) => v (i, j) -> Gen v i
next view = Next view Done

orr :: [Gen Rose a] -> Gen Rose a
orr gens = case getDone gens of
    Just a -> pure a
    Nothing -> do
        (i:is) <- next (Branch $ getViews gens)
        let (head, gen:tail) = splitAt i gens
        let gen' = stepGen is gen
        orr (head <> (gen':tail))
  where
    getDone []         = Nothing
    getDone (Done a:_) = Just a
    getDone (_:as)     = getDone as

    getViews :: [Gen Rose a] -> [Rose i]
    getViews [] = []
    getViews (Next v _:as) = v:getViews as

    stepGen i (Next _ next) = next i

gen1 :: Gen Rose [Int]
gen1 = do
    res1 <- next (Leaf 10)
    res2 <- next (Leaf 20)
    pure [res1, res2]

gen2 :: Gen Rose [Int]
gen2 = do
    res1 <- next (Branch [Leaf 10, Leaf 20])
    res2 <- next (Branch [Leaf 30, Leaf 40])
    pure [res1, res2]


run :: (View v, Show a) => Gen v a -> IO ()
run gen = case gen of
    Done a -> putStrLn $ "completed with: " ++ show a
    Next v next -> runView v >>= run . next

data Rose a
    = Branch [Rose a]
    | Leaf a
    deriving Show

instance Functor Rose where
  fmap fn (Leaf a) = Leaf (fn a)
  fmap fn (Branch as) = Branch ((fn <$>) <$> as)

instance Applicative Rose where
  pure = Leaf

  (<*>) (Leaf fn)    as = fn <$> as
  (<*>) (Branch fns) as = Branch ((<*> as) <$> fns)

instance Monad Rose where
  (>>=) (Leaf a)    fn = fn a
  (>>=) (Branch as) fn = Branch ((>>= fn) <$> as)

instance Foldable Rose where
  foldMap fn (Leaf a)    = fn a
  foldMap fn (Branch as) = foldMap (foldMap fn) as

rose1 = Leaf 1
rose2 = Branch [Leaf 1, Leaf 2, Leaf 3]
rose3 = roseFn2 <*> rose2

roseFn1 = (+) <$> rose1
roseFn2 = (+) <$> rose2

type Trace a = ([Int], a)

trace :: Rose a -> Trace a -> a
trace (Leaf a)    ([], _)     = a
trace (Branch as) (ix:ixs, a) = trace (as !! ix) (ixs, a)

traces :: Rose a -> [Trace a]
traces as = foldMap pure (build as)
  where
    build (Leaf a)   = Leaf ([], a)
    build (Branch as) = Branch [first (i:) <$> build a | (i, a) <- zip [0..] as]
