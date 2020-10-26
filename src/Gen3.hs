{-# LANGUAGE LambdaCase, BlockArguments #-}

import Data.Array
import Data.Maybe
import Race2

data Base m o r = Emit r
                | Forever o
                | View o m

mapInner :: Functor m => (a -> b) -> Base (m a) o r -> Base (m b) o r
mapInner _ (Emit r)    = Emit r
mapInner _ (Forever o) = Forever o
mapInner fn (View o m) = View o (fn <$> m)

instance (Show o, Show r) => Show (Base m o r) where
  show (Emit r)    = "(Emit " ++ show r ++ ")"
  show (Forever o) = "(Forever " ++ show o ++ ")"
  show (View o _)  = "(View " ++ show o ++ ", <m>)"


newtype Gen o r = Gen { unGen :: Base (IO (Gen o r)) o r }

instance (Show o, Show r) => Show (Gen o r) where
  show = show . unGen

instance Functor (Gen o) where
  fmap fn (Gen base) = Gen (go fn base)
    where
      go fn (Emit r)    = Emit (fn r)
      go fn (Forever o) = Forever o
      go fn (View o io) = View o ((fn <$>) <$> io)

instance Applicative (Gen o) where
  pure = Gen . Emit

  Gen (Emit fn)     <*> gen = fn <$> gen
  Gen (Forever o)   <*> _   = Gen (Forever o)
  Gen (View o ioFn) <*> gen = Gen $ View o ((<*> gen) <$> ioFn)

instance Monad (Gen o) where
  Gen (Emit r)    >>= fn = fn r
  Gen (Forever o) >>= _  = Gen (Forever o)
  Gen (View o io) >>= fn = Gen $ View o ((>>= fn) <$> io)



newtype Orr r o = Orr { unOrr :: Base (IO (Orr r o)) o r }

instance (Show o, Show r) => Show (Orr r o) where
  show = show . unOrr

instance Functor (Orr r) where
  fmap fn (Orr base) = Orr (go fn base)
    where
      go fn (Emit a)    = Emit a
      go fn (Forever o) = Forever (fn o)
      go fn (View o io) = View (fn o) ((fn <$>) <$> io)

instance Applicative (Orr r) where
  pure = Orr . Forever

  Orr (Emit r)       <*> _               = Orr (Emit r)
  _                  <*> Orr (Emit r)    = Orr (Emit r)
  Orr (Forever fn)   <*> orr             = fn <$> orr
  Orr (View fn ioFn) <*> Orr (View o io) = Orr (View (fn o) app)
    where
      app = ioFn >>= \fn -> (fn <*>) <$> io


toOrr :: Gen o r -> Orr r o
toOrr = Orr . mapInner toOrr . unGen

toGen :: Orr r o -> Gen o r
toGen = Gen . mapInner toGen . unOrr
