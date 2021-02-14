{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
  #-}

import Data.Function
import Data.IORef
import Control.Monad

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

-- i.   extend curr      = id
-- ii.  curr . extend f  = f
-- iii. extend f . extend g = extend (f .  extend g)
class Functor w => Comonad w where
  curr :: w a -> a
  dup :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

  dup = extend id
  extend f = fmap f . dup



data Stream a = Cons a (Stream a) deriving Show

instance Functor Stream where
  fmap fn (Cons a as) = Cons (fn a) (fn <$> as)

instance Applicative Stream where
  pure = fix . Cons

  Cons fn fns <*> Cons a as = Cons (fn a) (fns <*> as)

instance Monad Stream where
  Cons a as >>= fn = fn a

instance Comonad Stream where
  curr (Cons a _) = a
  dup stream@(Cons a as) = Cons stream (dup as)


nat = let nat' x = Cons x (nat' $ x + 1) in nat' 1

sumNext stream = curr stream + next stream

next :: Stream a -> a
next (Cons _ (Cons a _)) = a



data Sequence a = End a | Next (Sequence a) deriving Show

instance Functor Sequence where
  fmap fn (End a) = End (fn a)
  fmap fn (Next next) = Next (fn <$> next)

instance Applicative Sequence where
  pure = End

  End fn <*> seq      = fn <$> seq
  Next nextFn <*> seq = Next (nextFn <*> seq)

instance Monad Sequence where
  End a     >>= fn = fn a
  Next next >>= fn = next >>= fn

instance Comonad Sequence where
  curr (End a) = a
  curr (Next next) = curr next

  dup seq = seq <$ seq



instance Pairing Sequence Stream where
  pair fn (End a)     (Cons b _) = fn a b
  pair fn (Next next) (Cons _ stream) = pair fn next stream


move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ x -> x) movement (dup space)

type UI a m = a (IO (m ()))

type Component w a m = w (UI a m)

data Console a = Console { _text :: String, _action :: a }

explore :: (Comonad w, Pairing m w) => Component w Console m -> IO ()
explore component = do
    let (Console text action) = curr component

    putStrLn text
    movement <- action
    explore (move component movement)

counter :: Component Stream Console Sequence
counter = render <$> nat
  where
    render :: Int -> UI Console Sequence
    render state = Console (show state) $ do
        getLine >>= pure . \case
            "1" -> Next (End ())
            "2" -> Next (Next (End ()))


data Choices a = Choices [(a, String)]




data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap gn (Store fn s) = Store (gn . fn) s

instance Comonad (Store s) where
  curr (Store fn s) = fn s

  dup (Store fn s) = Store (Store fn) s



data State s a = State (s -> (s, a))

instance Functor (State s) where
  fmap gn (State fn) = State $ (gn <$>) . fn

instance Applicative (State s) where
  pure a = State $ \s -> (s, a)

  State fnFn <*> State fn = State $ \s ->
      let (s', gn) = fnFn s
          (s'', a) = fn s'
       in (s'', gn a)

instance Monad (State s) where
  State fn >>= gn = State $ \s ->
      let (s', a) = fn s
          State hn = gn a
       in hn s'


modify :: (s -> s) -> State s ()
modify fn = State $ \s -> (fn s, ())


put :: s -> State s ()
put s = State $ \_ -> (s, ())


get :: State s s
get = State (\s -> (s, s))



instance Pairing (State s) (Store s) where
  pair fn (State gn) (Store hn s) = fn a b
    where (s', a) = gn s
          b = hn s'

listCmpt :: Component (Store [String]) Console (State [String])
listCmpt = Store render []
  where
    render :: [String] -> UI Console (State [String])
    render list = Console ("Got: " ++ show list) $ do
        input <- getLine
        pure $ if input == "" then put [] else modify (++[input])
