module hlist

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

namespace HList
  data hList : List Type -> Type where
    hNil : hList []
    hCons : a -> hList xs -> hList (a :: xs)

  total head : hList (x :: xs) -> x
  head (hCons x xs) = x

  total tail : hList (x :: xs) -> hList xs
  tail (hCons x xs) = xs

  total (++) : hList xs -> hList ys -> hList (xs ++ ys)
  (++) hNil         ys = ys
  (++) (hCons x xs) ys = hCons x (xs ++ ys)

  index : hList xs -> Fin n -> (index n xs)
  index (hCons x xs) Z     = x
  index (hCons x xs) (S k) = index xs k
  --proj (S k) (x :: xs) = proj k xs
