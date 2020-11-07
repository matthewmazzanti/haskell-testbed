{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  , GADTs
  #-}

import Control.Monad.Free
import Control.Comonad.Cofree



class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing ((->) a) ((,) a) where
  pair fn gn (x, y) = fn (gn x) y

instance Pairing ((,) a) ((->) a) where
  pair fn (x, y) gn = fn y (gn x)

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair fn (x :< _)  (Pure y)  = fn x y
  pair fn (_ :< xs) (Free ys) = pair (pair fn) xs ys




data GenF e a = View [e] (e -> a)

instance Functor (GenF o) where
  fmap gn (View es fn) = View es (gn . fn)

type Gen e a = Free (GenF e) a

view :: [a] -> Gen a a
view as = liftF $ View as id

interpret :: (Show e, Show a) => Gen e a -> IO ()
interpret gen = case gen of
    Pure r -> putStrLn ("completed with: " ++ show r)
    Free (View es fn) -> do
        putStrLn ("choose: " ++ show es)
        input <- (es !!) . read <$> getLine
        let gen' = fn input
        interpret gen'

test = do
    q1 <- view ["foo", "bar", "baz"]
    q2 <- view ["asdf", "qwer", "zcxv"]
    pure [q1, q2]


data CoGenF e a = CoGenF ([e] -> IO (e, a))

instance Functor (CoGenF o) where
  fmap gn (CoGenF fn) = CoGenF (fmap (gn <$>) . fn)

type CoGen e a = Cofree (CoGenF e) a

-- mkCoAdder :: Limit -> Count -> CoAdder (Limit, Count)
-- mkCoAdder limit count = coiter next (limit, count)
--   where next x = CoAdderF (coAdd x) (coClear x) (coTotal x)
-- 
-- coAdd :: (Limit, Count) -> Int -> (Bool, (Limit, Count))
-- coAdd (limit, count) x = (under, (limit, nextCount))
--     where count' = count + x
--           under = count' <= limit
--           nextCount = if under then count' else count


mkCoGen :: Show e => [e] -> CoGen e [e]
mkCoGen es = coiter (CoGenF . coView) []

coView :: Show e => [e] -> [e] -> IO (e, [e])
coView _ es = do
    putStrLn ("choose: " ++ show es)
    res <- (es !!) . read <$> getLine
    pure (res, es)

instance Pairing CoGenF GenF where
  pair fn (CoGenF mkIO)   (View x handler) = fn adderState (gn under)
    where (under, adderState) = mkIO x
