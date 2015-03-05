module hlist

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

data hList : List Type -> Type where
  hNil : hList []
  (::) : a -> hList xs -> hList (a :: xs)

total head : hList (x :: xs) -> x
head (x :: xs) = x

total tail : hList (x :: xs) -> hList xs
tail (x :: xs) = xs

total (++) : hList xs -> hList ys -> hList (xs ++ ys)
(++) hNil      ys = ys
(++) (x :: xs) ys = x :: (xs ++ ys)

index : Fin n -> hList xs -> index n xs
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs
-- --index (S k) (x :: xs) = proj k xs
