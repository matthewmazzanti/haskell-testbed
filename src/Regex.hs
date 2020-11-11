type Sym = Char

data Regex
    = Empty
    | Lit Sym
    | Or Regex Regex
    | And Regex Regex
    | Star Regex
    | Plus Regex
    deriving Show

data NFA
    = Split Int NFA NFA
    | Step Int Sym NFA
    | Final Int
    deriving Show

state :: NFA -> Int
state (Split i _ _) = i
state (Step i _ _) = i
state (Final i) = i


conv :: Regex -> NFA
conv re = let (nfa, i) = conv' 0 (Final i) re in nfa
  where
    conv' :: Int -> NFA -> Regex -> (NFA, Int)
    conv' i next (Lit c) = (Step i c next, i + 1)

    conv' i next (Or re re') = (Split i nfa nfa', i'' + 1)
      where (nfa, i')   = conv' i next re
            (nfa', i'') = conv' i' next re

    conv' i next (And re re') = (nfa, i'' + 1)
      where (nfa, i')   = conv' i nfa' re
            (nfa', i'') = conv' i' next re

    conv' i next (Star re) = (split, i'' + 1)
      where split      = Split i next nfa
            (nfa, i'') = conv' i split re

    conv' i next (Plus re) = (nfa, i'' + 1)
      where split      = Split i next nfa
            (nfa, i'') = conv' i split re

test = conv (And (Star (Lit 'a')) (Lit 'b'))

closure :: NFA -> [NFA]
closure (Split _ l r) = closure l <> closure r
closure x = [x]

stepNFA :: Sym -> NFA -> [NFA]
stepNFA sym (Step _ sym' nfa)  = if sym == sym' then [nfa] else []
stepNFA _ _ = []

runNFA :: [Sym] -> NFA -> [NFA]
runNFA []         nfa = closure nfa
runNFA (sym:syms) nfa = closure nfa >>= stepNFA sym >>= runNFA syms

hasFinal :: [NFA] -> Bool
hasFinal [] = False
hasFinal (Final _:_) = True
hasFinal (_:nfas) = hasFinal nfas

run nfa syms = hasFinal (runNFA syms nfa)

closureST :: NFA -> [Int]
closureST (Final i)     = [i]
closureST (Step i _ _)  = [i]
closureST (Split i l r) = i:(closureST l <> closureST r)
