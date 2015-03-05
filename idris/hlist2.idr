{- Failed attempt at defining `index` on HLists.  See hlist.idr for the correct version. -}

module hlist

data Vect : Nat -> Type -> Type where
  vNil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

length : Vect n a -> Nat
length vNil      = 0
length (x :: xs) = 1 + (length xs)

(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) vNil      ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

-- index : Fin n -> Vect n a -> a
-- index FZ     (x :: xs) = x
-- index (FS k) (x :: xs) = index k xs

data Exactly : Nat -> Type where
  eZ : Exactly Z
  eS : Exactly n -> Exactly (S n)

plain : Exactly n -> Nat
plain eZ     = Z
plain (eS k) = S (plain k)

-- This is not total, so cannot be reduced when used in a type
-- signature.  Indeed, it cannot be total unless we produce an element
-- of `a`, which we do not have
-- index : Nat -> Vect n a -> a
-- index _     vNil      = ?h
-- index Z     (x :: xs) = x
-- index (S k) (x :: xs) = index k xs

namespace hList
  data hList : Vect n Type -> Type where
    hNil : hList vNil
    (::) : a -> hList xs -> hList (a :: xs)

  -- data hList : List Type -> Type where
  --   hNil : hList Nil
  --   (::) : a -> hList xs -> hList (a :: xs)

  head : hList (x :: xs) -> x
  head (x :: xs) = x

  tail : hList (x :: xs) -> hList xs
  tail (x :: xs) = xs

  (++) : hList xs -> hList ys -> hList (xs ++ ys)
  (++) hNil      ys = ys
  (++) (x :: xs) ys = x :: (xs ++ ys)

  -- plain : hList xs -> Vect (length xs) a
  -- plain hNil      = vNil
  -- plain (x :: xs) = (::) {a=a} x (plain xs)

  -- indexTy : Nat -> hList xs -> Type
  -- indexTy Z     ((::) {a=Type} x xs) = x
  -- indexTy (S k) (x :: xs) = indexTy k xs

  -- indexTy : Nat -> Vect n a -> a
  -- indexTy Z     (x :: xs) = x
  -- indexTy (S k) (x :: xs) = indexTy k xs

  -- indexTy : Nat -> Vect n Type -> Type
  -- indexTy Z     (x :: xs) = x
  -- indexTy (S k) (x :: xs) = indexTy k xs

  -- index : Fin n -> hList xs -> hlist.index n xs
  -- index : Fin n -> hList xs -> index n xs
  -- index FZ     (x :: xs) = x
  -- index (FS k) (x :: xs) = index k xs

  -- data indexTy : hList xs -> Type where
  --   zz : Nat -> hList (x :: xs) -> indexTy x
    --ZZ : (S k) (x :: xs) -> indexTy k xs

  -- hIndex : Exactly n -> hList xs -> index n xs
  -- hIndex k xs = index (plain k) xs

  -- hIndex : Exactly n -> hList xs -> index n xs
  -- index : Fin n -> hList xs -> index (FZ {k=n}) xs
  -- hIndex : (k:Fin n) -> hList xs -> index k xs
  -- hIndex eZ     (x :: xs) = x
  -- hIndex (eS k) (x :: xs) = hIndex k xs

  -- index : Fin n -> hList xs -> index (FZ {k=n}) xs
  -- index FZ     (x :: xs) = x
