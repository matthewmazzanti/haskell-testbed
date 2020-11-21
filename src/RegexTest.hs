import Regex

import Test.QuickCheck
import Criterion.Main
import Debug.Trace


instance Arbitrary Regex where
  arbitrary = sized genRegex
    where
      genRegex n = frequency
          [ (2 * n + 1, Lit <$> arbitraryASCIIChar)
          , (n, And <$> genRegex' <*> genRegex')
          , (n, Or <$> genRegex' <*> genRegex')
          , (n, Star <$> genRegex')
          , (n, Plus <$> genRegex')
          , (n, pure Empty)
          ]
        where genRegex' = genRegex (n - 1)

fromRegex :: Regex -> Gen String
fromRegex (Empty)      = pure []
fromRegex (Lit c)      = pure [c]
fromRegex (And re re') = (<>) <$> fromRegex re <*> fromRegex re'
fromRegex (Or re re')  = oneof [fromRegex re, fromRegex re']
fromRegex (Star re)    = frequency [(1, pure []), (10, fromRegex re)]
fromRegex (Plus re)    = (<>) <$> fromRegex re <*> fromRegex (Star re)

testNFA toNFA re = forAll (fromRegex re) (matchNFA $ toNFA re)

testDFA re = forAll (fromRegex re) (matchDFA $ toDFA $ toNFA' re)

fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)


-- Our benchmark harness.
main = defaultMain
    [ bgroup "dfa" $ (go $ matchDFA dfa) <$> (take 5 $ powers)
    , bgroup "nfa" $ (go $ matchNFA nfa) <$> (take 5 $ powers)
    ]
  where
    re = And (Star $ Lit 'a') (And (Star $ Lit 'a') (And (Star $ Lit 'a') (And (Lit 'a') (And (Lit 'a') (Lit 'a')))))
    nfa = toNFA re
    dfa = toDFA nfa
    as = repeat 'a'
    powers = iterate (*2) 8

    go match size = bench (show size) $ whnf match (take size as)
