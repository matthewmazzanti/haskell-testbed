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

await :: IO r -> Gen o r
await io = Free $ Yield Nothing (Cont io pure)

yield :: o -> IO r -> Gen o r
yield x io = Free $ Yield (Just x) (Cont io pure)

yield' :: Maybe o -> IO i -> (i -> Gen o r) -> Gen o r
yield' x io next = Free $ Yield x $ Cont io next

await' :: IO i -> (i -> Gen o r) -> Gen o r
await' io next = Free $ Yield Nothing $ Cont io next

step :: (Gen o r) -> Either r (Wait o r)
step (Pure r) = Left r
step (Free x) = Right x

stepOrr :: Traversable t => t (Gen o r) -> Either r (t (Wait o r))
stepOrr = traverse step

collect :: (Monoid m, Foldable f) => f m -> m
collect = foldr (<>) mempty

toArray :: [a] -> Array Int a
toArray xs = array (0, length xs - 1) (zip [0..] xs)

type Res o r = (Int, Gen o r, Race Int (Gen o r))

orr :: Semigroup o => [Gen o r] -> Gen o r
orr [] = error "Non-empty list required"
orr [co] = co
orr gens = init $ stepOrr $ toArray gens
  where
    init :: Semigroup o => Either r (Array Int (Wait o r)) -> Gen o r
    init (Left r)      = pure r
    init (Right waits) = yield' (collect xs) io (loop xs)
      where
        xs = _value <$> waits
        io = mkRace (_cont <$> waits) >>= runRace

    loop :: Semigroup o => Array Int (Maybe o) -> Res o r -> Gen o r
    loop xs (id, gen, race)   = loopBody $ step gen
      where
        loopBody (Left r)     = await (killRace race) >> pure r
        loopBody (Right wait) = yield' (collect xs') io (loop xs')
          where xs' = (xs // [(id, _value wait)])
                io  = addCont (id, (_cont wait)) race >> runRace race


test i = do
    yield [i] (threadDelay 1000000)
    test (i + 1)

test2 i = do
    yield [i] (threadDelay 2000000)
    test (i + 1)

watch :: (Show o, Show r) => Gen o r -> IO ()
watch gen = case gen of
    Pure r -> putStrLn $ "done " ++ (show r)
    Free (Yield x cont) -> do
        putStrLn $ "yielding: " ++ (show x)
        run cont >>= watch
