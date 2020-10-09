data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)
            deriving Show

add :: Expr Int -> Expr Int -> Expr Int
add = Add

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B


eval :: Expr a -> a
eval (I n) = n

    {-
eval :: Expr -> Maybe (Either Int Bool)
eval (I x) = Just (Left x)
eval (Add e1 e2) = case (eval e1, eval e2) of
    (Just (Left x), Just (Left y)) -> Just $ Left $ x + y
    _                              -> Nothing
eval (Mul e1 e2) = case (eval e1, eval e2) of
    (Just (Left x), Just (Left y)) -> Just $ Left $ x * y
    _                              -> Nothing
eval (B x) = Just (Right x)
eval (Eq e1 e2) = case (eval e1, eval e2) of
    (Just (Right x), Just (Right y)) -> Just $ Right $ x == y
    (Just (Left x), Just (Left y))   -> Just $ Right $ x == y
    _                                -> Nothing
    -}
