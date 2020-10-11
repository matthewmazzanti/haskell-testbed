import Cont
import Control.Monad.Free
import Control.Concurrent hiding (yield)
import Data.Array
import Data.Foldable
import Data.Either
import Data.IORef

data GenF o a = Yield { _value :: Maybe o, _cont :: Cont a }

instance Functor (GenF o) where
  fmap fn (Yield x cont) = Yield x (fn <$> cont)

type Gen o r = Free (GenF o) r
type Wait o r = GenF o (Gen o r)
type Step o r = Either r (Wait o r)

await :: IO r -> Gen o r
await io = Free $ Yield Nothing (Cont io pure)

yield :: Maybe o -> IO r -> Gen o r
yield x io = Free $ Yield x (Cont io pure)

step :: (Gen o r) -> Either r (Wait o r)
step (Pure r) = Left r
step (Free x) = Right x

stepOrr :: Traversable t => t (Gen o r) -> Either r (t (Wait o r))
stepOrr = traverse step

collect :: (Monoid m, Foldable f) => f m -> m
collect = foldr (<>) mempty

toArray :: [a] -> Array Int a
toArray xs = array (0, length xs - 1) (zip [0..] xs)

orr :: Semigroup o => [Gen o r] -> Gen o r
orr [] = error "Non-empty list required"
orr [co] = co
orr gens = init $ stepOrr $ toArray gens
  where
    init :: Semigroup o => Either r (Array Int (Wait o r)) -> Gen o r
    init (Left r)      = pure r
    init (Right waits) = do
        let xs = _value <$> waits
            createRace = mkRace ((step <$>) <$> (_cont <$> waits))
        race <- yield (collect xs) createRace
        res <- await (runRace race)
        loop race xs res

    loop :: Semigroup o
         => Race Int (Step o r)
         -> Array Int (Maybe o)
         -> (Int, Step o r)
         -> Gen o r
    loop race xs (id, Left r) = do
        await (killRace race)
        pure r

    loop race xs (id, Right wait) = do
        let xs' = (xs // [(id, _value wait)])
            io  = (do addCont id (step <$> _cont wait) race
                      runRace race)
        res <- yield (collect xs') io
        loop race xs' res


test1 i = do
    yield (Just [i]) (threadDelay 1000000)
    test1 (i + 1)


watch :: (Show o, Show r) => Gen o r -> IO ()
watch gen = case gen of
    Pure r -> putStrLn $ "done " ++ (show r)
    Free (Yield x cont) -> do
        putStrLn $ "yielding: " ++ (show x)
        run cont >>= watch
