module prims

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False

-- Naturals are useful for dependent types
data Naturel = Z | S Naturel

plus : Naturel -> Naturel -> Naturel
plus Z     y = y
plus (S k) y = S (plus k y)

-- prims.plus (S (S Z)) (S (S Z))

mult : Naturel -> Naturel -> Naturel
mult Z     y = Z
mult (S k) y = plus y (mult k y)

-- prims.mult (S (S Z)) (S (S Z))

-- Desugared into arabic numbers

-- Polymorphic lists, not type-dependent
data Liste a = lNil | (::) a (Liste a)
--data Liste a = Nil | Cons a (Liste a)

(++) : Liste a -> Liste a -> Liste a
(++) lNil      ys = ys
(++) (x :: xs) ys = lNil
