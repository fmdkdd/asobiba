module hlist

data HList : List Type -> Type where
  Nil : HList Nil
  (::) : a -> HList xs -> HList (a :: xs)

-- List.index requires a proof that (n < |ts|) as final argument.  We
-- can push this requirement to the caller of hlist.index by requiring
-- this proof as an implicit argument.
indexImpl : (n:Nat) -> HList ts -> index n ts ok
indexImpl Z     (x :: xs) = x
indexImpl (S k) (x :: xs) = indexImpl k xs

-- > indexImpl 2 [1,"a",True] {ok=Refl}
-- True : Bool
-- > indexImpl 3 [1,"a",True] {ok=Refl}
-- Can't unify True with lte (S (fromInteger 2)) (length [])

-- Why does it not reduce `lte`?  Because there is a missing implicit
-- in `length`: the type of the elements of the list.

-- > indexImpl 2 [] {ok=Refl} {a=Int}
-- Can't convert True = True with False = True

-- But, I cannot find a way to prove that indexImpl is total.  We are
-- missing the case of any Nat with the empty list, but Idris does not
-- seem convinced this is impossible.  Maybe the middle is not
-- excluded?
-- indexImpl _  [] {ok=(False = True)} impossible


-- XXX: This course of action fails.  I’m not entirely sure why.

-- total
-- lengthLemma : (lt (S n) (S m) = True) -> (lt n m = True)
-- lengthLemma p = p

-- indexExpl : (n:Nat) -> HList ts -> (ok: lt n (length ts) = True) -> index n ts ok
-- indexExpl Z     (x :: xs) _  = x
-- indexExpl (S k) (x :: xs) ok = indexExpl k xs (lengthLemma ok)
