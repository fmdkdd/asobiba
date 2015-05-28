-- Recopying to follow examples from:
-- https://dl.dropboxusercontent.com/u/828035/Monads/monads.pdf

class Monade m where
    return :: a -> m a
    (>.>) :: m a -> (a -> m b) -> m b

data Tree a = Fork (Tree a) (Tree a)
            | Leaf a
            | Nil
  deriving Show

instance Monade Tree where
    return t = Leaf t
    Nil >.> f = Nil
    Leaf a >.> f = f a
    Fork u v >.> f = Fork (u >.> f) (v >.> f)

tree2 = Fork (Leaf 2) (Leaf 3)

f 2 = Fork Nil (Leaf "Two")
f 3 = Fork (Leaf "Three") (Leaf "x")
