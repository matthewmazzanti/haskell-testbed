type Sym = Char

data Regex
    = Empty
    | Lit Sym
    | Or Regex Regex
    | And Regex Regex
    | Star Regex
    | Plus Regex
    deriving Show

data ESym = S Sym | E deriving Show

fromSyms :: [Sym] -> [ESym]
fromSyms [] = E:[]
fromSyms (sym:syms) = E:(S sym):fromSyms syms

dropE :: [ESym] -> [ESym]
dropE (E:syms) = syms
dropE syms = syms

matchRegex :: Regex -> [Sym] -> Bool
matchRegex re syms = isJust $ match re (fromSyms syms)

match :: Regex -> [ESym] -> Maybe [ESym]
match _ [] = Nothing
match _ (E:E:_) = error "Invalid epsilon"
match Empty (E:syms)   = Just syms
match Empty (S _:syms) = Nothing

match (Lit c) syms = case (dropE syms) of
    [] -> Nothing
    (S sym:syms) -> if c == sym then Just syms else Nothing

match (And re re') syms    = match re syms >>= match re'
match (Or re re') syms     = match re syms <|> match re' syms
match (Plus re) syms       = match re syms >>= match (Star re)

match star@(Star re) syms@(sym:_) = loop <|> empty
  where loop  = match re syms >>= match star
        empty = case sym of
            E   -> tail syms)
            S _ -> Nothing
