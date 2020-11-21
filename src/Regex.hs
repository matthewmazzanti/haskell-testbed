{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Regex where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Debug.Trace

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

idNFA :: NFA -> Int
idNFA (Split i _ _) = i
idNFA (Step i _ _) = i
idNFA (Final i) = i

instance Eq NFA where
  x == y = idNFA x == idNFA y

instance Ord NFA where
  compare x y = compare (idNFA x) (idNFA y)

toNFA :: Regex -> NFA
toNFA re = let (nfa, i) = conv 0 (Final i) re in nfa
  where
    conv :: Int -> NFA -> Regex -> (NFA, Int)
    conv i next (Empty) = (next, i)
    conv i next (Lit c) = (Step i c next, i + 1)

    conv i next (And re re') = (nfa, k)
      where (nfa,  j) = conv i nfa' re
            (nfa', k) = conv j next re'

    conv i next (Or re re') = (Split i nfa nfa', k)
      where (nfa,  j) = conv (i + 1) next re
            (nfa', k) = conv j next re'

    conv i next (Star re) = (split, j)
      where split    = Split i next nfa
            (nfa, j) = conv (i + 1) split re

    conv i next (Plus re) = (nfa, j)
      where split    = Split i next nfa
            (nfa, j) = conv (i + 1) split re

toNFA' :: Regex -> NFA
toNFA' re = let (nfa, i) = runState (conv re $ Final i) 0 in nfa
  where
    inc :: State Int Int
    inc = state \i -> (i, i + 1)

    mkStar :: Regex -> NFA -> State Int (NFA, NFA)
    mkStar re next = mdo
        split <- Split <$> inc <*> pure next <*> pure nfa
        nfa <- conv re split
        pure (split, nfa)

    conv :: Regex -> NFA -> State Int NFA
    conv (Empty) next      = pure next
    conv (Lit c) next      = Step <$> inc <*> pure c <*> pure next
    conv (And re re') next = conv re' next >>= conv re
    conv (Or re re') next  = Split <$> inc <*> conv re next <*> conv re' next
    conv (Star re) next    = mkStar re next >>= pure . fst
    conv (Plus re) next    = mkStar re next >>= pure . snd

type Closure = Set.Set NFA


closure :: NFA -> Closure
closure = execClosure . closureST

execClosure :: State Closure a -> Closure
execClosure = flip execState Set.empty

closureST :: NFA -> State Closure ()
closureST nfa = do
    nfas <- get
    if Set.member nfa nfas then pure () else do
        modify $ Set.insert nfa
        recurse nfa
  where
    recurse (Split _ l r) = closureST l >> closureST r
    recurse _ = pure ()

stepNFA :: Sym -> Closure -> Closure
stepNFA sym set = execClosure $ traverse closureST $ stepAll set
  where
    stepAll :: Closure -> [NFA]
    stepAll = catMaybes . (stepOne sym <$>) . Set.toList

    stepOne :: Sym -> NFA -> Maybe NFA
    stepOne sym (Step _ sym' nfa)
      | sym == sym' = Just nfa
      | otherwise   = Nothing
    stepOne _ _ = Nothing

runNFA :: [Sym] -> NFA -> Closure
runNFA syms nfa = go syms $ closure nfa
  where
    go :: [Sym] -> Closure -> Closure
    go [] set         = set
    go (sym:syms) set = go syms $ stepNFA sym set

hasFinalNFA :: Closure -> Bool
hasFinalNFA set = go $ Set.toList set
  where
    go :: [NFA] -> Bool
    go [] = False
    go (Final _:_) = True
    go (_:nfas) = go nfas

matchNFA :: NFA -> [Sym] -> Bool
matchNFA nfa syms = hasFinalNFA $ runNFA syms nfa


type Edges = Map.Map Sym DFA
data DFA = DFA Bool Edges deriving Show

type Cache = Map.Map Closure DFA

hasFinalDFA :: DFA -> Bool
hasFinalDFA (DFA final _) = final

toDFA :: NFA -> DFA
toDFA nfa = evalState (conv $ closure nfa) Map.empty
  where
    -- Convert an NFA to a DFA within a state cache. The cache allows us to
    -- create circular references to the next DFA by keeping references to the
    -- other DFAs as the graph is being constructed.
    conv :: Closure -> State Cache DFA
    conv set = do
        cache <- get
        case Map.lookup set cache of
            -- DFA fragment previously computed, simply return.
            Just dfa -> pure dfa
            -- DFA fragment not computed. Build the DFA, and add to cache.
            Nothing -> build set

    -- Get the edges from the NFAs for each available symbol. See stepAll.
    -- Traverse is used to map the resulting edges of closures to edges of DFAs,
    -- and runs in the state cache.
    build :: Closure -> State Cache DFA
    build set = mdo
        modify $ Map.insert set dfa

        edges <- traverse conv $ stepAll set

        let dfa = DFA (hasFinalNFA set) edges

        pure dfa

    stepAll :: Closure -> Map.Map Sym Closure
    stepAll set = Map.fromList $ symClosure set <$> symsOf set
      where
        -- All of the unique symbols within this closure. Done to avoid having
        -- to guess all available characters.
        symsOf :: Closure -> [Sym]
        symsOf = catMaybes . fmap symOf . Set.toList
          where
            symOf :: NFA -> Maybe Sym
            symOf (Step _ sym _) = Just sym
            symOf _ = Nothing

        -- Where the work of creating the edges is done. For a given symbol and
        -- a closure, we calculate the closure of stepping with that symbol
        symClosure :: Closure -> Sym -> (Sym, Closure)
        symClosure set sym = (sym, stepNFA sym set)

runDFA :: [Sym] -> DFA -> Maybe DFA
runDFA [] dfa = Just dfa
runDFA (sym:syms) (DFA _ edges) = Map.lookup sym edges >>= runDFA syms

matchDFA :: DFA -> [Sym] -> Bool
matchDFA dfa syms = case runDFA syms dfa of
    Just dfa -> hasFinalDFA dfa
    Nothing -> False
