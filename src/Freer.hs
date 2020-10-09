{-# LANGUAGE GADTs #-}

data Freer g a where
  Pure :: a -> Freer g a
  Bind :: g x -> (x -> Freer g a) -> Freer g a

instance Functor (Freer g) where
  fmap fn (Pure x)      = Pure (fn x)
  fmap fn (Bind gx fn') = Bind gx (fmap fn . fn')

instance Applicative (Freer g) where
  pure = Pure

  Pure fn <*> x     = fmap fn x
  Bind gx fns <*> x = Bind gx ((<*> x) . fns)

instance Monad (Freer g) where
  Pure x >>= fn      = fn x
  Bind gx fn' >>= fn = Bind gx ((>>= fn) . fn')

eta :: g a -> Freer g a
eta gx = Bind gx Pure

data Generator i o r where
  Yield :: o -> Generator i o o
  Await :: Generator i o i
  YieldAwait :: o -> Generator i o i

type FGen i o = Freer (Generator i o)

yield = eta . Yield
await = eta Await
yieldAwait = eta . YieldAwait

test :: FGen Int Int Int
test = do
    yield 10
    x <- await
    pure 30

interpret :: [i] -> FGen i o r -> ([o], Maybe r)
interpret [] _                       = ([], Nothing)
interpret _ (Pure r)                 = ([], Just r)
interpret (i:is) (Bind (Yield o) fn) = (o:os, r)
  where (os, r) = interpret is (fn i)
interpret (i:is) (Bind (Await) fn)   = (os, r)
  where (os, r) = interpret is (fn i)

interpret (Bind (YieldAwait o) fn)   = (o:os, r)
  where (os, r) = interpret (fn 1)
