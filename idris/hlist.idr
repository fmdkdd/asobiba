module hlist

-- Check totality of each function in the current file.  Like adding
-- the `total` keyword in front of each function signature.  Totality
-- is required if a function is to be called in a signature.
%default total

using (a:Type, k:Nat)
  data Vect : Nat -> Type -> Type where
    vNil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

(++) : {a:Type, n:Nat, m:Nat} -> Vect n a -> Vect m a -> Vect (n + m) a
(++) vNil      ys = ys
(++) (x :: xs) ys = x :: (xs ++ ys)

using (k:Nat)
  data Fin : Nat -> Type where
    FZ : Fin (S k)
    FS : Fin k -> Fin (S k)

index : {a:Type, n:Nat} -> Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

-- Namespace because (::) is already defined by Vect
namespace hList
  using (n:Nat, a:Type, xs:Vect n Type)
    data hList : Vect n Type -> Type where
      hNil : hList vNil
      (::) : a -> hList xs -> hList (a :: xs)

  head : {n:Nat, x:Type, xs:Vect n Type} -> hList (t :: ts) -> t
  head (x :: xs) = x

  tail : {n:Nat, x:Type, xs:Vect n Type} -> hList (t :: ts) -> hList ts
  tail (x :: xs) = xs

  (++) : {txs:Vect n Type, tys:Vect m Type} -> hList txs -> hList tys -> hList (txs ++ tys)
  (++) hNil      ys = ys
  (++) (x :: xs) ys = x :: (xs ++ ys)

  -- It types, thanks to the helpful people on the Idris mailing list.
  -- see https://groups.google.com/d/msg/idris-lang/FiXh78d0C2Y/XSt0WfCcwiUJ
  index : {n:Nat, ts:Vect n Type} -> (k:Fin n) -> hList ts -> index k ts
  index FZ     (x :: xs) = x
  index (FS k) (x :: xs) = index k xs

  -- Failed attempts at defining index

  -- 1. Using `Nat` (or `Exactly n`) instead of `Fin n` for the index
  -- value.  When defining Vect.index, the function cannot be total,
  -- since there is no guarantee that n won’t go over the length of
  -- `ts`.

  -- is not total.  Use ‘:missing hList.index’ in the REPL to show
  -- missing cases.  In this case, we never test for (index _ vNil),
  -- i.e., when the first argument is larger than `n`.
  -- index : Nat -> Vect n a -> a
  -- index Z     (x :: xs) = x
  -- index (S k) (x :: xs) = index k xs

  -- 2. Defining hList using the List type, rather than Vect.

  -- data hList : List Type -> Type where
  --   hNil : hList Nil
  --   (::) : a -> hList xs -> hList (a :: xs)

  -- Cannot unify, since List.index requires a proof that (n < length
  -- ts), in order for the function to be total.
  -- index : (n:Nat) -> hList ts -> index n ts

  -- We can add such a proof as an argument.  In the base case, it
  -- works because 0 is obviously less than the non-empty list.
  -- index : (n:Nat) -> hList ts -> (p : lt n (length ts) = True) -> index n ts p
  -- index Z     (x :: xs) Refl = x
  -- XXX: how to write the recursion?  A proof that (lt (S k) (length
  -- (x :: xs)) is a proof that (lt k xs), but how do you tell that to
  -- Idris?
  --index (S k) (x :: xs) p    = index k xs p
