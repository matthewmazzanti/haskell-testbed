data Dyck = Pair [Dyck]

instance Show Dyck where
  show (Pair []) = "()"
  show (Pair pairs) = "(" <> foldMap show pairs <> ")"

parse :: String -> [Dyck] -> Maybe Dyck
parse "" _ = Nothing
parse ('(':str) stack = parse str (Pair []:stack)
parse (')':str) (curr:Pair sibs:stack) = parse str (Pair (sibs <> [curr]):stack)
parse ")" [curr] = Just curr
parse (')':str) stack = Nothing
