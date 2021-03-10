{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad
import qualified Data.Map as Map
import Control.Concurrent
import Control.Concurrent.MVar

data Event a = Event
    { _path :: [Int]
    , _res :: a
    }

data Action
    = Wait Int
    | Clear
    | Noop
    deriving (Show, Eq)

data View = View
    { _children :: Map.Map Int View
    , _action :: Action
    } deriving (Show, Eq)

view :: Action -> View
view = View Map.empty

joinViews :: [View] -> View
joinViews views = View (Map.fromList $ zip [0..] views) Noop

data Threads
    = Multi [Threads]
    | Thread ThreadId

data Run a = Run (MVar (Event a)) Threads
