data Generator a b r = Pure r
                     | Yield a (b -> Generator a b r)

instance Functor (Generator a b) where
  fmap fn (Pure x)        = Pure $ fn x
  fmap fn (Yield y next)  = Yield y $ fmap fn . next

instance Applicative (Generator a b) where
  pure = Pure

  (Pure fn) <*> (Pure x)       = Pure $ fn x
  (Pure fn) <*> (Yield y next) = Yield y $ (fn <$>) . next
  (Yield y fnext) <*> xs       = Yield y $ (<*> xs) . fnext

instance Monad (Generator a b) where
  (Pure x) >>= fn       = fn x
  (Yield y next) >>= fn = Yield y ((>>= fn) . next)

yield :: a -> Generator a () ()
yield x = Yield x (\_ -> Pure ())

await :: Generator () x x
await = Yield () (\x -> Pure x)

test :: Generator Int Int Int
test = Yield 10 (\x -> Yield x $ \_ -> Pure $ x + 1)

into :: Generator a b r -> [b] -> ([b], [a], Maybe r)
into _ []                  = ([], [], Nothing)
into (Pure r) ys           = (ys, [], Just r)
into (Yield x next) (y:ys) = (ys', x:xs, res)
  where (ys', xs, res) = into (next y) ys
