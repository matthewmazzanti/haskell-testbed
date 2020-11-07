{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , LambdaCase
  , BlockArguments
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



data AdderF a
    = Add Int (Bool -> a)
    | Clear a
    | Total (Int -> a)


instance Functor AdderF where
  fmap fn (Add i gn) = Add i (fn . gn)
  fmap fn (Clear a) = Clear (fn a)
  fmap fn (Total gn) = Total (fn . gn)


type Adder a = Free AdderF a


add :: Int -> Adder Bool
add x = liftF $ Add x id


clear :: Adder ()
clear = liftF $ Clear ()


total :: Adder Int
total = liftF $ Total id


interpret :: Int -> Int -> Adder r -> r
interpret limit count adder =
    case adder of
        Pure r -> r
        Free (Add i fn) ->
            let count' = i + count
                test = count' <= limit
                nextCount = if test then count' else count
             in interpret limit nextCount (fn test)
        Free (Clear a) ->
            interpret limit 0 a
        Free (Total fn) ->
            interpret limit count (fn count)



data CoAdderF a = CoAdderF
    { addH   :: Int -> (Bool, a)
    , clearH :: a
    , totalH :: (Int, a)
    }

instance Functor CoAdderF where
  fmap fn (CoAdderF add clear total) =
      CoAdderF ((fn <$>) . add) (fn clear) (fn <$> total)

type Limit = Int
type Count = Int

type CoAdder a = Cofree CoAdderF a


mkCoAdder :: Limit -> Count -> CoAdder (Limit, Count)
mkCoAdder limit count = coiter next (limit, count)
  where next x = CoAdderF (coAdd x) (coClear x) (coTotal x)


coClear :: (Limit, Count) -> (Limit, Count)
coClear (limit, count) = (limit, 0)


coTotal :: (Limit, Count) -> (Int, (Limit, Count))
coTotal (limit, count) = (count, (limit, count))


coAdd :: (Limit, Count) -> Int -> (Bool, (Limit, Count))
coAdd (limit, count) x = (under, (limit, nextCount))
    where count' = count + x
          under = count' <= limit
          nextCount = if under then count' else count


instance Pairing CoAdderF AdderF where
  pair fn (CoAdderF add _ _)   (Add x gn) = fn adderState (gn under)
    where (under, adderState) = add x

  pair fn (CoAdderF _ clear _) (Clear x)  = fn clear x

  pair fn (CoAdderF _ _ total) (Total gn) = fn adderState (gn count)
    where (count, adderState) = total


interpret' :: Limit -> Count -> Adder r -> r
interpret' limit count adder = pair (\_ b -> b) (mkCoAdder limit count) adder

interpretIO :: Limit -> Count -> Adder r -> r
interpretIO limit count adder = pair (\_ b -> b) coadder adder
  where coadder = mkCoAdder limit count

test = do
    add 1
    add 2
    add 3
    total >>= pure

