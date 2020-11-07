
data List a = Cons a (List a) | Nil

instance Functor (List a)
