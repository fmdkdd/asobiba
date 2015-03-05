module dependent

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

-- Type-dependent append
-- Break it!
(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

-- Do you see the bug?
index : Nat -> Vect n a -> a
index Z     (x :: xs) = x
index (S k) (x :: xs) = index k xs


-- Finite sets

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

-- index : Fin n -> Vect n a -> a
-- index FZ     (x :: xs) = x
-- index (FS k) (x :: xs) = index k xs


-- Pairs (sigma types)

-- data Sigma : (A : Type) -> (P : A -> Type) -> Type where
--   MkSigma : {P : A -> Type} -> (a : A) -> P a -> Sigma A P

vec : Sigma Nat (\n => Vect n Int)
vec = MkSigma 2 [3,4]

-- Syntactic sugar
vec' : (n : Nat ** Vect n Int)
vec' = (2 ** [3, 4])

-- Inference of first element
vec'' : (n : Nat ** Vect n Int)
vec'' = (_ ** [3, 4])

-- Inference of type of first element
vec''' : (n ** Vect n Int)
vec''' = (_ ** [3,4])


filter : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter p Nil = (_ ** [])
filter p (x :: xs) with (filter p xs)
                   | ( _ ** xs' ) = if p x then ( _ ** x :: xs' )
                                           else ( _ ** xs' )

-- data Elem : a -> Vect n a -> Type where
--   Here  : Elem x (x :: xs)
--   There : Elem x xs -> Elem x (y :: xs)

-- testVec : Vect 4 Int
-- testVec = [1,2,3,4]

-- inVect : Elem 3 testVec
-- inVect = There (There Here)
