module hlist

data Vect : Nat -> Type -> Type where
  vNil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

total (++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) vNil      ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

index : Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

-- index : Nat -> Vect n a -> a
-- index Z     (x :: xs) = x
-- index (S k) (x :: xs) = index k xs

namespace hList
  data hList : Vect n Type -> Type where
    hNil : hList vNil
    (::) : a -> hList xs -> hList (a :: xs)

  -- data hList : List Type -> Type where
  --   hNil : hList Nil
  --   (::) : a -> hList xs -> hList (a :: xs)

  -- total head : hList (x :: xs) -> x
  -- head (x :: xs) = x

  -- total tail : hList (x :: xs) -> hList xs
  -- tail (x :: xs) = xs

  -- total (++) : hList xs -> hList ys -> hList (xs ++ ys)
  -- (++) hNil      ys = ys
  -- (++) (x :: xs) ys = x :: (xs ++ ys)

  -- indexTy : Nat -> hList xs -> Type
  -- indexTy Z     ((::) {a=Type} x xs) = x
  -- indexTy (S k) (x :: xs) = indexTy k xs

  indexTy : Nat -> Vect n a -> a
  indexTy Z     (x :: xs) = x
  indexTy (S k) (x :: xs) = indexTy k xs

  -- index : Fin n -> hList xs -> hlist.index n xs
  index : Fin n -> hList xs -> indexTy n xs
  --index FZ     (x :: xs) = x
  index (FS k) (x :: xs) = index k xs

  -- data indexTy : hList xs -> Type where
  --   zz : Nat -> hList (x :: xs) -> indexTy x
    --ZZ : (S k) (x :: xs) -> indexTy k xs

  -- index : Fin n -> hList xs -> hlist.index n xs
  -- index : Fin n -> hList xs -> index (FZ {k=n}) xs
  -- index FZ     (x :: xs) = x
  --index (FS k) (x :: xs) = hList.hList.index k xs

  -- index : Fin n -> hList xs -> index (FZ {k=n}) xs
  -- index FZ     (x :: xs) = x
