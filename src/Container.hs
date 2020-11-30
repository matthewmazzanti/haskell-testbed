import Criterion.Main
import Data.Array
import GHC.Exts (IsList(..))

import qualified Conc as C
import qualified Data.Vector as V
import qualified Data.Sequence as S

nats = [1..]
squares = (\x -> x * x) <$> nats
powers = (\x -> 2 ^ x - 1) <$> nats
count = 12
countPow = 2 ^ count

squaresList len = take len squares
squaresArr len = listArray (0, len - 1) squares

squaresVec :: Int -> V.Vector Int
squaresVec len = fromList $ squaresList len

squaresCat :: Int -> C.Cat Int
squaresCat len = fromList $ squaresList len

squaresSeq :: Int -> S.Seq Int
squaresSeq len = S.fromList $ squaresList len


indexes =
    [ bgroup "list" . fmap (\x -> bench (show x) $ nf (squaresList countPow !!) x)
    , bgroup "vec" .  fmap (\x -> bench (show x) $ whnf (squaresVec countPow V.!) x)
    , bgroup "seq" . fmap (\x -> bench (show x) $ whnf (flip S.lookup $ squaresSeq countPow) x)
    , bgroup "cat" . fmap (\x -> bench (show x) $ whnf (squaresCat countPow C.!!) x)
    ] <*> [take count powers]

prependList :: [e] -> e -> [e]
prependList = flip (:)

prepends =
    [ bgroup "list" . fmap (\x -> bench (show x) $ nf (prependList $ squaresList x) 1)
    , bgroup "vec" . fmap (\x -> bench (show x) $ whnf (flip V.cons $ squaresVec x) 1)
    , bgroup "cat" . fmap (\x -> bench (show x) $ whnf (C.prepend $ squaresCat x) 1)
    ] <*> [take count powers]

appendList :: [e] -> e -> [e]
appendList xs x = xs <> pure x

appends =
    [ bgroup "list" . fmap (\x -> bench (show x) $ nf (appendList $ squaresList x) 1)
    , bgroup "arr" . fmap (\x -> bench (show x) $ whnf (V.snoc $ squaresVec x) 1)
    , bgroup "cat" . fmap (\x -> bench (show x) $ whnf (C.append $ squaresCat x) 1)
    ] <*> [take count powers]

main = defaultMain
    [ bgroup "indexes" indexes
    ]
