import Prelude hiding (Monoid)
import Control.Monad
import Test.QuickCheck

class Monoid m where
  one :: () -> m
  mult :: (m, m) -> m

lambda :: a -> ((), a)
lambda x = ((), x)

rho :: a -> (a, ())
rho x = (x, ())

alpha :: ((a, b), c) -> (a, (b, c))
alpha ((x, y), z) = (x, (y, z))

(<#>) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f <#> g) (x, y) = (f x, g y)

law1_left :: Monoid m => m -> m
law1_left = mult . (one <#> id) . lambda

law1_middle :: Monoid m => m -> m
law1_middle = id

law1_right :: Monoid m => m -> m
law1_right = mult . (id <#> one) . rho
