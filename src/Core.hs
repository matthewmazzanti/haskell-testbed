module Core where

type Name = String
type IsRec = Bool

data Expr a
    = EVar Name             -- Variable
    | ENum Int              -- Number
    | EConstr Int Int       -- Constructor
    | EAp (Expr a) (Expr a) -- Application
    | ELet                  -- Let(rec) expressions
        IsRec               --   boolean
        [(a, Expr a)]       --   Definitions
        (Expr a)            --   Body
