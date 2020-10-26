{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}

module Race2 where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Array
import Data.Maybe
import Data.Foldable


data State a = Action (IO a)
             | Running
             | Done a


data Continue = Continue deriving Show
instance Exception Continue


data Worker a = Worker (TVar (State a)) ThreadId

mkWorker :: TVar (State a) -> IO (Worker a)
mkWorker tvar = forkIO loopCont >>= (pure . Worker tvar)
  where
    -- Loop with an exception handler. Need to unmask (interruptible) async
    -- exceptions since catch masks them automatically
    loopCont = catch loop \(e::Continue) -> interruptible loop

    -- Get op, perform op, write result, loop
    loop = join getIO >>= writeRes >> loop

    -- Get the operation to perform
    getIO = readThen \case
        Action io -> do
            write Running
            pure io
        _ -> retry

    -- Write the result or ignore
    writeRes res = readThen \case
        Running -> write (Done res)
        _ -> pure ()

    -- Atomically (read then perform another action)
    readThen = atomically . (readTVar tvar >>=)
    write = writeTVar tvar


addWorkerIO :: Worker a -> IO a -> STM ()
addWorkerIO (Worker tvar _) io = writeTVar tvar (Action io)


continue :: Worker a -> IO ()
continue (Worker _ tid) = throwTo tid Continue


getDone :: Worker a -> STM (Maybe a)
getDone (Worker tvar _) =
    readTVar tvar >>= pure . \case
        Done r -> Just r
        _      -> Nothing


killWorker :: Worker a -> IO ()
killWorker (Worker _ tid) = killThread tid



type Race i a = Array i (Worker a)

mkRace :: Ix i => Array i (IO a) -> IO (Race i a)
mkRace ios = do
    states <- atomically $ traverse (newTVar . Action) ios
    traverse mkWorker states


waitRace :: Ix i => Race i a -> IO [(i, a)]
waitRace race =
    atomically $ doneIx race >>= \case
        []  -> retry
        res -> pure res
  where
    doneIx :: Ix i => Race i a -> STM [(i, a)]
    doneIx = fmap catIx . traverse getDone

    catIx :: Ix i => Array i (Maybe a) -> [(i, a)]
    catIx = catMaybes . fmap sequenceA . assocs


addIO :: Ix i => Race i a -> [(i, IO a)] -> IO ()
addIO race ixIOs = do
    atomically $ sequenceA $ zipWith addWorkerIO workers ios
    traverse_ continue workers
  where
    ixs = fst <$> ixIOs
    ios = snd <$> ixIOs
    workers = (race !) <$> ixs


killRace :: Ix i => Race i a -> IO ()
killRace race = traverse_ killWorker race



io1 = threadDelay 1000000 >> pure 1
io2 = (pure 2::IO Int)
io3 = threadDelay 2000000 >> pure 3
allIO = [io1, io2, io3]

fromArray :: [a] -> Array Int a
fromArray xs = array (0, length xs - 1) (zip [0..] xs)

test = do
    race <- mkRace (fromArray allIO)
    putStrLn "Built race"

    res <- waitRace race
    putStrLn ("Got result:" ++ show res)

    addIO race (zip (fst <$> res) allIO)
    putStrLn "Added IO"

    res <- waitRace race
    putStrLn ("Got result:" ++ show res)

    killRace race
    putStrLn "Done"
