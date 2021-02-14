import Control.Concurrent hiding (yield)
import Control.Monad.Free

data Event a = Event [Int] a deriving Show

instance Functor Event where
  fmap fn (Event ixs a) = Event ixs (fn a)

event :: a -> Event a
event = Event []

pushIndex :: Int -> Event a -> Event a
pushIndex ix (Event ixs a) = Event (ix:ixs) a

data View2 a = View [Int] Bool (IO a)

data View a = Run (IO a) | Multi [IO a]

instance Functor View where
  fmap fn (Run io) = Run (fn <$> io)
  fmap fn (Multi ios) = Multi (fmap fn <$> ios)

instance Semigroup (View a) where
  Run io <> Run io' = Multi [io, io']
  Run io <> Multi ios  = Multi (io:ios)
  Multi ios <> Run io  = Multi (ios <> pure io)
  Multi ios <> Multi ios' = Multi (ios <> ios')

pushIndexView :: Int -> View (Event a) -> View (Event a)
pushIndexView i view = pushIndex i <$> view

combineViews :: [View (Event a)] -> View (Event a)
combineViews views = foldr1 (<>) $ zipWith pushIndexView [0..] views

data GenF e a = Disp
    { _view :: View (Event e)
    , _next :: Event e -> a
    }

instance Functor (GenF e) where
  fmap fn (Disp view next) = Disp view (fn . next)

type Gen e a = Free (GenF e) a
type Stepped e a = GenF e (Gen e a)

orr :: [Gen e a] -> Gen e a
orr gens = case step gens of
    Left a -> pure a
    Right steppeds -> disp steppeds
  where
    next :: [Stepped e a] -> Event e -> Gen e a
    next steppeds event = case stepOne steppeds event of
        Left a -> pure a
        Right steppeds -> disp steppeds

    disp :: [Stepped e a] -> Gen e a
    disp steppeds = Free $ Disp view next'
      where view = combineViews $ _view <$> steppeds
            next' = next steppeds

step :: [Gen e a] -> Either a [Stepped e a]
step [] = Right []
step (Pure a:_) = Left a
step ((Free genf):gens) = case step gens of
    Left a -> Left a
    Right genfs -> Right (genf:genfs)

stepOne :: [Stepped e a] -> Event e -> Either a [Stepped e a]
stepOne steppeds (Event (ix:ixs) e) = case _next stepped event of
    Pure a -> Left a
    Free stepped' -> Right (replaceAt ix steppeds stepped')
  where stepped = steppeds !! ix
        event = Event ixs e

replaceAt :: Int -> [a] -> a -> [a]
replaceAt 0 (x:xs) y = y:xs
replaceAt i (x:xs) y
  | i > 0     = x:replaceAt (i-1) xs y
  | otherwise = error "Index cant be negative"

yield :: IO e -> Gen e e
yield io = Free $ Disp view (\(Event _ e) -> pure e)
  where view = Run (event <$> io)

test = do
    yield $ do
        threadDelay 1000000
        pure 10

    yield $ do
        threadDelay 1000000
        pure 20

    pure 30

testRun :: (Show e, Show a) => Gen e a -> IO ()
testRun (Pure a) = putStrLn ("Returned " <> show a)
testRun (Free (Disp (Run io) next)) = do
    e <- io
    putStrLn ("Got event " <> show e)
    testRun (next e)
testRun (Free (Disp (Multi (io:_)) next)) = do
    e <- io
    putStrLn ("Got event " <> show e)
    testRun (next e)
