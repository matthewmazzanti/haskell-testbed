{-# LANGUAGE LambdaCase, BlockArguments #-}

import Data.Array
import Data.Maybe
import Race2

data Gen o r = Emit r
             | Forever o
             | View o (IO (Gen o r))

instance (Show o, Show r) => Show (Gen o r) where
  show (Emit r) = "(Emit " ++ show r ++ ")"
  show (Forever o) = "(Forever " ++ show o ++ ")"
  show (View o io) = "(View " ++ show o ++ ", <io>)"

instance Functor (Gen o) where
  fmap fn (Emit r)    = Emit (fn r)
  fmap fn (Forever o) = Forever o
  fmap fn (View o io) = View o ((fn <$>) <$> io)

instance Applicative (Gen o) where
  pure = Emit

  Emit fn     <*> gen = fn <$> gen
  Forever o   <*> gen = Forever o
  View o ioFn <*> gen = View o ((<*> gen) <$> ioFn)

instance Monad (Gen o) where
  Emit r    >>= fn = fn r
  Forever o >>= _  = Forever o
  View o io >>= fn = View o ((>>= fn) <$> io)


newtype Orr r o = Orr { unOrr :: Gen o r }

instance (Show o, Show r) => Show (Orr r o) where
  show = show . unOrr

instance Functor (Orr r) where
  fmap fn (Orr gen) = Orr (fmap' fn gen)
    where
      fmap' fn (Emit a)    = Emit a
      fmap' fn (Forever o) = Forever (fn o)
      fmap' fn (View o io) = View (fn o) (unOrr . (fn <$>) . Orr <$> io)

instance Applicative (Orr r) where
  pure = Orr . Forever

  Orr (Forever fn) <*> orr = fn <$> orr
  Orr genFn  <*> Orr gen   = Orr (apply genFn gen)
    where
      apply (Emit r) _       = Emit r
      apply _ (Emit r)       = Emit r
      apply (View fn ioFn) (View o io) = View (fn o) appGen
        where
          ioFn' = Orr <$> ioFn
          io' = Orr <$> io
          app = ioFn' >>= \fn -> (fn <*>) <$> io'
          appGen = unOrr <$> app


await :: Monoid o => IO r -> Gen o r
await io = View mempty (pure <$> io)

view :: Monoid o => o -> IO r -> Gen o r
view x io = View x (pure <$> io)

display :: Monoid o => o -> Gen o r
display x = Forever x

step :: Traversable t => t (Gen o r) -> Gen (t o) r
step = unOrr . traverse Orr


toArray :: [a] -> Array Int a
toArray xs = array (0, length xs - 1) (zip [0..] xs)

collect :: (Monoid m, Foldable f) => f m -> m
collect = foldr (<>) mempty

-- orr :: Monoid o => [Gen o r] -> Gen o r
-- orr [] = error "Non-empty list required"
-- -- With the monoid cosntraint on outputs, we can directly yield a single child
-- orr [gen] = gen
-- orr gens = init (stepOrr $ toArray gens)
--   where
--     init :: Monoid o => Gen (Array Int o) r -> Gen o r
--     init (Emit r) = Emit r
--     init (Right (vals, ixIOs)) = do
--         -- Yield the initial view, and setup the race to run
--         (race, nexts) <- view (collect vals) $ do
--             -- Create the race
--             race <- mkRace (stepIO <$> waits)
--             -- Run the race getting the next step
--             next <- waitRace race
--             -- Return both race and next
--             pure (race, nexts)
--  
--         -- Loop!
--         loop (race, vals) nexts
--  
--     loop :: Monoid o => Orr o r -> [(Int, Gen o r)] -> Gen o r
--     loop (race, _) (_, Left r) = do
--         -- Terminate the remaining threads
--         await (killRace race)
--         -- Return the value
--         pure r
-- 
--     loop (race, vals) gens = do
--         -- Update the values array with the new value
--         let vals' = vals // [(id, _val wait)]
-- 
--         -- Yield the updated view, and wait for the next result
--         nexts <- view (collect vals') $ do
--             -- Add the next io into the race
--             addIOs id (stepIO wait) race
--             -- Get the next result
--             runRace race
-- 
--         -- Loop!
--         loop (race, vals') next
