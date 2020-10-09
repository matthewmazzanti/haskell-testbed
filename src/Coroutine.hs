{-# LANGUAGE GADTs #-}

import Control.Monad
import Data.Maybe
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Data.Array

data Cnt a = forall i. Cnt (IO i) (i -> a)

instance Functor Cnt where
  fmap fn (Cnt io cnt) = Cnt io (fn . cnt)

runWith :: MVar a -> Cnt a -> IO ThreadId
runWith mvar (Cnt io fn) = forkIO (io >>= putMVar mvar . fn)

run :: Cnt a -> IO a
run cnt = do
    mvar <- newEmptyMVar
    -- Run io on separate thread
    tid <- runWith mvar cnt
    -- Wait for results
    takeMVar mvar

runBoth :: (Cnt a, Cnt b) -> IO (a, b)
runBoth (cnt, cnt') = do
    mvar <- newEmptyMVar
    mvar' <- newEmptyMVar

    runWith mvar cnt
    runWith mvar' cnt'

    i <- takeMVar mvar
    i' <- takeMVar mvar'

    pure (i, i')

runAll :: [Cnt a] -> IO [a]
runAll cnts = mapM run cnts

runWithID :: MVar (ThreadId, a) -> Cnt a -> IO ThreadId
runWithID mvar (Cnt io fn) = forkIO $ do
    i <- io
    tid <- myThreadId
    putMVar mvar (tid, fn i)

data Race a = Race (MVar (ThreadId, a)) [(ThreadId, (Cnt a))]

mkRace :: [Cnt a] -> IO (Race a)
mkRace cnts = do
    mvar <- newEmptyMVar
    tids <- mapM (runWithID mvar) cnts
    pure $ Race mvar (zip tids cnts)

killRace :: Race a -> IO ()
killRace (Race _ tidCnts) = mapM_ (killThread . fst) tidCnts


addCnt :: Cnt a -> Race a -> IO (Race a)
addCnt cnt (Race mvar tidCnts) = do
    tid <- runWithID mvar cnt
    pure $ Race mvar ((tid, cnt) : tidCnts)


runRace :: Race a -> IO (a, Race a)
runRace race@(Race mvar tidCnts) = do
    (tid, i) <- takeMVar mvar
    pure (i, removeThread tid race)
      where
        removeThread :: ThreadId -> Race a -> Race a
        removeThread tid (Race mvar tidCnts) = Race mvar (filterTid tidCnts)
          where filterTid = filter $ (tid /=) . fst



data Gen o r = Done r
                   | Await (Cnt (Gen o r))
                   | Yield o (Cnt (Gen o r))

await io = Await $ Cnt io $ \x -> Done x
yield x io = Yield x $ Cnt io $ \x -> Done x

instance (Show o, Show r) => Show (Gen o r) where
  show (Done r)       = "(Done " ++ show r ++ ")"
  show (Await cnt)   = "(Await <io> <function>)"
  show (Yield x cnt) = "(Yield " ++ show x ++ " <io> <function>)"

instance Functor (Gen o) where
  fmap fn (Done r)       = Done (fn r)
  fmap fn (Await cnt)   = Await (fmap (fmap fn) cnt)
  fmap fn (Yield x cnt) = Yield x (fmap (fmap fn) cnt)

instance Applicative (Gen o) where
  pure = Done

  (Done fn)        <*> rest = fmap fn rest
  (Await cnt)     <*> rest = Await (fmap (<*> rest) cnt)
  (Yield x cnt)   <*> rest = Yield x (fmap (<*> rest) cnt)

instance Monad (Gen o) where
  (Done x)       >>= fn = fn x
  (Await cnt)   >>= fn = Await (fmap (>>= fn) cnt)
  (Yield x cnt) >>= fn = Yield x (fmap (>>= fn) cnt)



getDone :: Gen o r -> Maybe r
getDone (Done r) = Just r
getDone _ = Nothing

getYield :: Gen o r -> Maybe o
getYield (Yield x _) = Just x
getYield _ = Nothing

getAwait :: Gen o r -> Maybe (Cnt (Gen o r))
getAwait (Await cnt) = Just cnt
getAwait (Yield _ cnt) = Just cnt
getAwait _ = Nothing


bothIO :: (IO a, IO b) -> IO (a, b)
bothIO (io, io') = do
    i <- io
    i' <- io'
    pure (i, i')

-- orr :: Monoid o => [Gen o r] -> Gen o r
-- orr [] = error "Non-empty list required"
-- orr [co] = co
-- orr cos = go dones
--   where dones = mapMaybe getDone cos
--         yields = mapMaybe getYield cos
--         awaits = mapMaybe getAwait cos
--         race = mkRace awaits
-- 
--         go (r:_) = Done r
--         go _ [] awaits = Await $ Cnt (runRace race) cntinue
--         go _ yields awaits = Yield (foldr (<>) mempty yields) $ Cnt (runRace race) cntinue
-- 
--         cntinue :: (Gen o r, Race (Gen o r)) -> Gen o r
--         cntinue (Done r, _) = Done r


both :: Monoid o => Gen o r -> Gen o r -> Gen o r
both (Done r) _ = Done r
both _ (Done r) = Done r
both (Await cnt) (Await cnt') = Await (bothCnt cnt cnt' both)
both (Await cnt) (Yield x cnt') = Yield x (bothCnt cnt cnt' both)
both (Yield x cnt) (Yield x' cnt') = Yield (x <> x') (bothCnt cnt cnt' both)

bothCnt :: Cnt a -> Cnt b -> (a -> b -> c) -> Cnt c
bothCnt cnt cnt' fn = Cnt (runBoth (cnt, cnt')) (uncurry fn)


test = do
    yield [30] (putStrLn "hello")
    pure 40

test2 = do
    yield [30] (putStrLn "hello")
    pure 20

step :: (Show o, Show r) => Gen o r -> IO ()
step co = case co of
    Done r -> do
        putStrLn $ "done " ++ (show r)
    Await cnt -> do
        putStrLn "awaiting"
        next <- run cnt
        step next
    Yield x cnt -> do
        putStrLn $ "yielding: " ++ (show x)
        next <- run cnt
        step next

cnt1 = Cnt (threadDelay 5000000 >> putChar '1') id
cnt2 = Cnt (putChar '2') id
cntSleep = Cnt (threadDelay 7500000 >> putChar '3') id

testCnts = do
    race <- mkRace [cnt1, cntSleep]
    (_, race) <- runRace race
    race <- addCnt cnt1 race
    (_, race) <- runRace race
    killRace race

main = step test
