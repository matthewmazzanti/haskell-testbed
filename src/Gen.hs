import Race
import Control.Monad.Free
import Data.Array

-- Remove
import Control.Concurrent hiding (yield)

data GenF o a = Yield { _val :: o, _io :: IO a }

instance Functor (GenF o) where
  fmap fn (Yield x io) = Yield x (fn <$> io)

type Gen o r = Free (GenF o) r

await :: Monoid o => IO r -> Gen o r
await io = Free $ Yield mempty (pure <$> io)

yield :: Monoid o => o -> IO r -> Gen o r
yield x io = Free $ Yield x (pure <$> io)


type Wait o r = GenF o (Gen o r)
type Step o r = Either r (Wait o r)

step :: (Gen o r) -> Either r (Wait o r)
step (Pure r) = Left r
step (Free x) = Right x


type Steps t o r = Either r (t (Wait o r))

stepOrr :: Traversable t => t (Gen o r) -> Steps t o r
stepOrr = traverse step


stepIO :: Wait o r -> IO (Step o r)
stepIO = (step <$>) . _io


collect :: (Monoid m, Foldable f) => f m -> m
collect = foldr (<>) mempty


toArray :: [a] -> Array Int a
toArray xs = array (0, length xs - 1) (zip [0..] xs)


type Orr o r = (Race Int (Step o r), Array Int o)

orr :: Monoid o => [Gen o r] -> Gen o r
orr [] = error "Non-empty list required"
-- With the monoid cosntraint on outputs, we can directly yield a single child
orr [gen] = gen
-- Create array for the generators, step them to get initial value, and run
orr gens = init $ stepOrr $ toArray gens
  where
    init :: Monoid o => Steps (Array Int) o r -> Gen o r
    init (Left r)      = pure r
    init (Right waits) = do
        -- Get the initial values for the view
        let vals = _val <$> waits

        -- Yield the initial view, and setup the race to run
        (race, next) <- yield (collect vals) $ do
            -- Create the race
            race <- mkRace (stepIO <$> waits)
            -- Run the race getting the next step
            next <- runRace race
            -- Return both race and next
            pure (race, next)

        -- Loop!
        loop (race, vals) next

    loop :: Monoid o => Orr o r -> (Int, Step o r) -> Gen o r
    loop (race, _) (_, Left r) = do
        -- Terminate the remaining threads
        await (killRace race)
        -- Return the value
        pure r

    loop (race, vals) (id, Right wait) = do
        -- Update the values array with the new value
        let vals' = vals // [(id, _val wait)]

        -- Yield the updated view, and wait for the next result
        next <- yield (collect vals') $ do
            addIO id (stepIO wait) race
            runRace race

        -- Loop!
        loop (race, vals') next

test1 i = do
    yield [i] $ do
        threadDelay 1000000

    test1 (i + 1)


watch :: (Show o, Show r) => Gen o r -> IO ()
watch gen = case gen of
    Pure r -> putStrLn $ "done " ++ (show r)
    Free (Yield x io) -> do
        putStrLn $ "yield: " ++ (show x)
        io >>= watch
