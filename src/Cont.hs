{-# LANGUAGE GADTs #-}

module Cont where

import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Data.Array
import Data.Maybe
import Data.IORef

data Cont a = forall i. Cont (IO i) (i -> a)

instance Functor Cont where
  fmap fn (Cont io cont) = Cont io (fn . cont)


runWith :: MVar a -> Cont a -> IO ThreadId
runWith mvar (Cont io fn) = forkIO (io >>= putMVar mvar . fn)


run :: Cont a -> IO a
run cont = do
    mvar <- newEmptyMVar
    -- Run io on separate thread
    tid <- runWith mvar cont
    -- Wait for results
    takeMVar mvar


runBoth :: (Cont a, Cont b) -> IO (a, b)
runBoth (cont, cont') = do
    mvar <- newEmptyMVar
    mvar' <- newEmptyMVar

    runWith mvar cont
    runWith mvar' cont'

    i <- takeMVar mvar
    i' <- takeMVar mvar'

    pure (i, i')


runAll :: [Cont a] -> IO [a]
runAll conts = mapM run conts


data Race i a = Race (MVar (i, a)) (Array i (Maybe ThreadId))


runWithID :: MVar (i, a) -> (i, Cont a) -> IO ThreadId
runWithID mvar (id, (Cont io fn)) = forkIO $ do
    i <- io
    putMVar mvar (id, fn i)


withIx :: Ix i => Array i e -> Array i (i, e)
withIx arr = array (bounds arr) (zip (indices arr) (assocs arr))


mkRace :: Ix i => Array i (Cont a) -> IO (Race i a)
mkRace conts = do
    mvar <- newEmptyMVar
    tids <- mapM (runWithID mvar) (withIx conts)
    pure $ Race mvar (Just <$> tids)


runRace :: Ix i => Race i a -> IO (i, a, Race i a)
runRace (Race mvar tids) = do
    (id, x) <- takeMVar mvar
    pure (id, x, Race mvar $ tids // [(id, Nothing)])


killRace :: Race i a -> IO ()
killRace (Race mvar tids) = mapM_ (mapM_ killThread) tids


killId :: Ix i => Race i a -> i -> IO (Race i a)
killId (Race mvar tids) id = do
    mapM_ killThread (tids ! id)
    pure $ Race mvar $ tids // [(id, Nothing)]


addCont :: Ix i => (i, Cont a) -> Race i a -> IO (Race i a)
addCont (id, cont) race@(Race mvar tids) = do
    killId race id
    tid <- runWithID mvar (id, cont)
    pure $ Race mvar $ tids // [(id, Just tid)]


cont1 = Cont (threadDelay 5000000 >> putChar '1') id
cont2 = Cont (putChar '2') id
contSleep = Cont (threadDelay 7500000 >> putChar '3') id

fromArray :: [a] -> Array Int a
fromArray xs = array (0, length xs - 1) (zip [0..] xs)

testConts = do
    race <- mkRace (fromArray [cont1, contSleep])
    putStrLn "Built race"
    (i, x, race) <- runRace race
    putStrLn "Got result"
    race <- addCont (i, cont1) race
    putStrLn "Added continuation"
    (i, x, race) <- runRace race
    putStrLn "Got result 2"
    killRace race
    putStrLn "Done"