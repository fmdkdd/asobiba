type Name  = String

data Term  = Var Name
           | Con Int
           | Add Term Term
           | Lam Name Term
           | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong   = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e   = Main.lookup x e
interp (Con i) e   = unitM (Num i)
interp (Add u v) e = interp u e `bindM` (\a ->
                                  interp v e `bindM` (\b ->
                                  add a b))
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindM` (\f ->
                     interp u e `bindM` (\a ->
                     apply f a))

lookup :: Name -> Environment -> M Value
lookup x []        = errorM ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x==y then unitM b else Main.lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b             = errorM ("should be numbers: " ++ showval a ++ "," ++ showval b)

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a       = errorM ("should be function: " ++ showval f)

test :: Term -> String
test t = showM (interp t [])

term0 :: Term
term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
         (Add (Con 10) (Con 11)))

term1 :: Term
term1 = (App (Con 1) (Con 2))

-- Error messages
data M a    = Success a | Error String
unitM a     = Success a
errorM s    = Error s

(Success a) `bindM` k = k a
(Error s) `bindM` k   = Error s

showM (Success a) = "Success: " ++ showval a
showM (Error s)   = "Error: " ++ s
