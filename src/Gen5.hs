{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  , GADTs
  #-}

class View v where
  runView :: Show a => v a -> IO a

instance View [] where
  runView view = do
    putStrLn $ "choose: " ++ show view
    (view !!) . read <$> getLine



data Gen v a where
  Done :: a -> Gen v a
  Next :: (Show i, View v) => v i -> (i -> Gen v a) -> Gen v a

instance Functor (Gen v) where
  fmap fn (Done a) = Done (fn a)
  fmap fn (Next v next) = Next v (fmap fn . next)

instance Applicative (Gen v) where
  pure = Done

  Done fn <*> gen = fn <$> gen
  Next v nextFn <*> gen = Next v ((<*> gen) . nextFn)

instance Monad (Gen v) where
  Done a >>= fn = fn a
  Next v next >>= fn = Next v ((>>= fn) . next)

next :: (Show i, View v) => v i -> Gen v i
next view = Next view Done


orr :: Gen v i -> Gen v i -> Gen [v] i
orr (Done a) _ = Done a
orr _ (Done a) = Done a


test :: Gen [] [Int]
test = do
    res1 <- next [10, 20, 30]
    res2 <- next ["10", "20", "30"]
    Done [res1, length res2]

run :: Show a => Gen [] a -> IO ()
run gen = case gen of
    Done a -> putStrLn $ "completed with: " ++ show a
    Next v next -> runView v >>= run . next
