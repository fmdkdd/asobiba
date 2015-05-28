import Data.Maybe

type Name = String

data Term = TVar Name
          | TAbs Name Term
          | TApp Term Term
          | TRecord [(Name, Term)]
          | TProj Term Name
  deriving Show

freevar :: Name -> Term -> Bool
freevar x (TVar y) = x == y
freevar x (TAbs y t1) = x /= y && freevar x t1
freevar x (TApp t1 t2) = freevar x t1 || freevar x t2
freevar x (TRecord ts) = all (freevar x . snd) ts
freevar x (TProj t1 _) = freevar x t1

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

subst x s (TRecord ts) = TRecord (map (\(n,t) -> (n, subst x s t)) ts)

subst x s (TProj t n) = TProj (subst x s t) n

-- Eval

isVal :: Term -> Bool
isVal (TAbs _ _) = True
isVal (TRecord ts) = all (isVal . snd) ts

eval1 :: Term -> Maybe Term
eval1 (TApp (TAbs x t12) v2) = return $ subst x v2 t12 -- E-AppAbs

eval1 (TApp t1 t2) = if isVal t1
                     then do t2' <- eval1 t2 -- E-App2
                             return $ TApp t1 t2'
                     else do t1' <- eval1 t1 -- E-App1
                             return $ TApp t1' t2

eval1 (TProj t1@(TRecord ts) n) = if isVal t1 -- E-ProjRcd
                                  then lookup n ts
                                  else Nothing

eval1 (TProj t1 n) = do t1' <- eval1 t1 -- E-Proj
                        return $ TProj t1' n

eval1 (TRecord ts) = do ts' <- evalNextField ts
                        return $ TRecord ts'   -- E-Rcd

eval1 _ = Nothing

evalNextField :: [(Name, Term)] -> Maybe [(Name, Term)]
evalNextField [] = Nothing
evalNextField ((n,t):ts) = if isVal t
                           then do ts' <- (evalNextField ts)
                                   return $ (n,t):ts'
                           else do t' <- eval1 t
                                   return $ (n,t'):ts

eval :: Term -> Term
eval t = case eval1 t of Just t' -> eval t'
                         Nothing -> t
