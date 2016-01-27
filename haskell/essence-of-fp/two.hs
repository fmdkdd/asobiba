type Name  = String

data Term  = Var Name
           | Con Int
           | Add Term Term
           | Lam Name Term
           | App Term Term
           | At Position Term

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
interp (At p t) e  = resetP p (interp t e)

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
term1 = (At (0,1) (App (At (0,5) (Con 1)) (At (0,10) (Con 2))))

-- Error messages with positions
type Position = (Int,Int)

showpos :: Position -> String
showpos (x,y) = "line " ++ show x ++ ", column " ++ show y

pos0 = (0,0)

type M a = Position -> E a

unitM a  = \p -> unitE a
errorM s = \p -> errorE (showpos p ++ ": " ++ s)

m `bindM` k = \p -> m p `bindE` (\x -> k x p)

showM m = showE (m pos0)

resetP :: Position -> M x -> M x
resetP q m = \p -> m q

data E a    = Success a | Error String

unitE a  = Success a
errorE s = Error s

(Success a) `bindE` k = k a
(Error s) `bindE` k   = Error s

showE (Success a) = "Success: " ++ showval a
showE (Error s)   = "Error: " ++ s
