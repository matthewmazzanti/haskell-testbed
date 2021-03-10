data Dyck = Pair [Dyck] deriving Show

data Top = Top Stack [Dyck] Stack

fromChar :: Char -> Top
fromChar '(' = Top [] [] [[]]
fromChar ')' = Top [[]] [] []

-- (( = Top [] [] [[], []]
-- (()( = Top [] [] [[Pair []], []]
-- (()()(() = Top [] [] [[Pair [], Pair []], [Pair[]]]
--
-- (() = Top [] [] [[Pair []]]
-- Top [] [] [[Pair []]] <> Top [] [] [[Pair []]] = Top [] [] [[Pair []], [Pair []]]

type Stack = [[Dyck]]

merge :: Stack -> Stack -> Stack
merge (l:pl:ls) (r:rs) = merge (pl <> l <> r:ls) rs
merge (l:ls) (r:pr:rs) = merge ls (l <> r <> pr:rs)
