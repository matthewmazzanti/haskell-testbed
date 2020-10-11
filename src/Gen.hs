import Cont
import Control.Monad.Free
import Control.Concurrent hiding (yield)
import Data.Array
import Data.Foldable
import Data.Either
import Data.IORef

data GenF o a = Yield { _val :: o, _cont :: Cont a }

instance Functor (GenF o) where
  fmap fn (Yield x cont) = Yield x (fn <$> cont)

type Gen o r = Free (GenF o) r
type Wait o r = GenF o (Gen o r)

await :: Monoid o => IO r -> Gen o r
await io = Free $ Yield mempty (Cont io pure)

yield :: Monoid o => o -> IO r -> Gen o r
yield x io = Free $ Yield x (Cont io pure)


type Step o r = Either r (Wait o r)

step :: (Gen o r) -> Either r (Wait o r)
step (Pure r) = Left r
step (Free x) = Right x


type Steps t o r = Either r (t (Wait o r))

stepOrr :: Traversable t => t (Gen o r) -> Steps t o r
stepOrr = traverse step


stepCont :: Wait o r -> Cont (Step o r)
stepCont = (step <$>) . _cont


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

        -- Wait for the next value of the race to complete
        race <- await $ mkRace (stepCont <$> waits)

        -- Yield the initial view, and create a race for the child actions
        next <- yield (collect vals) $ do
            runRace race

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
            addCont id (stepCont wait) race
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
    Free (Yield x cont) -> do
        putStrLn $ "yield: " ++ (show x)
        run cont >>= watch
