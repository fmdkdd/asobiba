module ListLen

data HList : List Type -> Type where
  Nil : HList Nil
  (::) : a -> HList xs -> HList (a :: xs)

-- We can construct (n < m) if we have (n + 1 < m + 1).
total
lengthLemma : LT (S n) (S m) -> LT n m
lengthLemma (LTESucc x) = x

-- List.index uses a predicate explicit argument `n < |l| = True`, and
-- here we use an implicit proof that is (maybe?)  automatically
-- constructed by the checker using `auto`.  At least, without `auto`
-- we have to specify {ok=LTESucc LTEZero} for getting the value
-- indexed at 0.
total
listIndex : (n : Nat) -> (xs : List a) -> {auto ok : LT n (length xs)} -> a
-- {ok=LETZero} is stating `0 < |[]|`, and that is impossible
-- We still need to specify this case for the function to be total?
listIndex _     []        {ok = LTEZero} impossible
listIndex Z     (x :: xs)      = x
-- `ok` is a proof that `n + 1 < |x :: xs|`, and by the lengthLemma we
-- can state that `n < |xs|` and hand that to the recursive call.
listIndex (S k) (x :: xs) {ok} = listIndex k xs {ok=lengthLemma ok}

-- The definition of hIndex is identical, except for the type signature.
total
hIndex : {ts : List Type} -> (n : Nat) -> HList ts -> {auto ok : LT n (length ts)}
         -> listIndex n ts {ok=ok}
hIndex _     []        {ok=LTEZero} impossible
hIndex Z     (x :: xs)      = x
hIndex (S k) (x :: xs) {ok} = hIndex k xs {ok=lengthLemma ok}


-- Limits of the `auto` keyword.

-- The auto keyword has a hardcoded depth of search.

-- listIndex 99 [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
-- returns “Can't solve goal when elaborating argument _ok_”

-- But,
-- List.index 99 [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] Refl
-- returns 1 : Integer
