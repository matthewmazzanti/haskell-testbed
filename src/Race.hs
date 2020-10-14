module Race where

import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Data.Array
import Data.Maybe
import Data.IORef
import Data.Foldable



runWith :: MVar a -> IO a -> IO ThreadId
runWith mvar io = forkIO (io >>= putMVar mvar)


run :: IO a -> IO a
run cont = do
    mvar <- newEmptyMVar
    -- Run io on separate thread
    tid <- runWith mvar cont
    -- Wait for results
    takeMVar mvar


runBoth :: (IO a, IO b) -> IO (a, b)
runBoth (io, io') = do
    mvar <- newEmptyMVar
    mvar' <- newEmptyMVar

    runWith mvar io
    runWith mvar' io'

    i <- takeMVar mvar
    i' <- takeMVar mvar'

    pure (i, i')


runAll :: Traversable t => t (IO a) -> IO (t a)
runAll conts = mapM run conts



type Tids i = IORef (Array i (Maybe ThreadId))


clearTid :: Ix i => Tids i -> i -> IO ()
clearTid tids id = modifyIORef' tids (// [(id, Nothing)])


clearAll :: Tids i -> IO ()
clearAll tids = modifyIORef' tids (Nothing <$)


setTid :: Ix i => Tids i -> i -> ThreadId -> IO ()
setTid tids id x = modifyIORef' tids (// [(id, Just x)])



data Race i a = Race (MVar (i, a)) (IORef (Array i (Maybe ThreadId)))


runWithID :: MVar (i, a) -> (i, IO a) -> IO ThreadId
runWithID mvar (id, io) = forkIO $ do
    i <- io
    putMVar mvar (id, i)


withIx :: Ix i => Array i e -> Array i (i, e)
withIx arr = array (bounds arr) (zip (indices arr) (assocs arr))


mkRace :: Ix i => Array i (IO a) -> IO (Race i a)
mkRace ios = do
    mvar <- newEmptyMVar
    tidsPure <- mapM (runWithID mvar) (withIx ios)
    tids <- newIORef (Just <$> tidsPure)
    pure (Race mvar tids)


runRace :: Ix i => Race i a -> IO (i, a)
runRace (Race mvar tids) = do
    (id, x) <- takeMVar mvar
    clearTid tids id
    pure (id, x)


killRace :: Race i a -> IO ()
killRace (Race _ tids) = do
    readIORef tids >>= mapM_ (mapM_ killThread)
    clearAll tids


killId :: Ix i => Race i a -> i -> IO ()
killId (Race _ tids) id = do
    readIORef tids >>= mapM_ killThread . (! id)
    clearTid tids id


addIO :: Ix i => i -> IO a -> Race i a -> IO ()
addIO id io race@(Race mvar tids) = do
    killId race id
    tid <- runWithID mvar (id, io)
    setTid tids id tid


io1 = threadDelay 5000000 >> putChar '1'
io2 = putChar '2'
ioSleep = threadDelay 7500000 >> putChar '3'

fromArray :: [a] -> Array Int a
fromArray xs = array (0, length xs - 1) (zip [0..] xs)

test = do
    race <- mkRace (fromArray [io1, ioSleep])
    putStrLn "Built race"
    (i, x) <- runRace race
    putStrLn "Got result"
    addIO i io1 race
    putStrLn "Added IO"
    (i, x) <- runRace race
    putStrLn "Got result 2"
    killRace race
    putStrLn "Done"
