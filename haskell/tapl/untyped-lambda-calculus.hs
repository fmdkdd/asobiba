import Data.Maybe

type Name = String

data Term = TVar Name
          | TAbs Name Term
          | TApp Term Term
          deriving Show

freevar :: Name -> Term -> Bool
freevar x (TVar y) = x == y
freevar x (TAbs y t1) = x /= y && freevar x t1
freevar x (TApp t1 t2) = freevar x t1 || freevar x t2

freshvar :: Name -> Term -> Name
freshvar x t = if freevar x t
               then freshvar (x ++ x) t
               else x

subst :: Name -> Term -> Term -> Term
subst x s (TVar y) = if y == x
                     then s
                     else TVar y
subst x s (TAbs y t1) = if y == x
                        then TAbs y t1 -- y shadows x
                        else TAbs w (subst x s (subst y (TVar w) t1))
                             where w = freshvar y t1
subst x s (TApp t1 t2) = TApp (subst x s t1) (subst x s t2)



eval1 :: Term -> Maybe Term
eval1 (TApp (TAbs x t12) v2) = return $ subst x v2 t12

eval1 (TApp t1 t2) = do t1' <- eval1 t1
                        t2' <- eval1 t2
                        return $ TApp t1' t2'

-- eval1 (TApp v1 t2) = do t2' <- eval1 t2
--                         return $ TApp v1 t2'



eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Just t' -> eval t'
                         Nothing -> t
