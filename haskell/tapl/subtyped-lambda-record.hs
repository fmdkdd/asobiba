import Data.Maybe
import Data.List

type Name = String

data Term = TVar Name
          | TAbs Name Type Term
          | TApp Term Term
          | TRecord [(Name, Term)]
          | TProj Term Name
  deriving Show

freevar :: Name -> Term -> Bool
freevar x (TVar y) = x == y
freevar x (TAbs y _ t1) = x /= y && freevar x t1
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

subst x s (TAbs y ty t1) = if y == x
                           then TAbs y ty t1 -- y shadows x
                           else TAbs w ty (subst x s (subst y (TVar w) t1))
                               where w = freshvar y t1

subst x s (TApp t1 t2) = TApp (subst x s t1) (subst x s t2)

subst x s (TRecord ts) = TRecord (map (\(n,t) -> (n, subst x s t)) ts)

subst x s (TProj t n) = TProj (subst x s t) n

-- Eval

isVal :: Term -> Bool
isVal (TAbs _ _ _) = True
isVal (TRecord ts) = all (isVal . snd) ts

eval1 :: Term -> Maybe Term
eval1 (TApp (TAbs x _ t12) v2) = return $ subst x v2 t12 -- E-AppAbs

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

-- Types

type Field = (Name, Type)

showField :: Field -> String
showField (n,ty) = n ++ ":" ++ show ty

showFields :: [Field] -> String
showFields tys = "{" ++ (intercalate "," fields) ++ "}"
    where fields = map showField tys

data Type = TyTop
          | TyArrow Type Type
          | TyRecord [Field]
  deriving Eq

instance Show Type where
    show TyTop = "TyTop"
    show (TyArrow ty1 ty2) = show ty1 ++ "->" ++ show ty2
    show (TyRecord tys) = showFields tys

type TypeError = String

type Context = [(Name, Type)]

typeof :: Term -> Context -> Either Type TypeError
typeof (TVar x) ctx = case lookup x ctx of
                        Just ty -> Left ty
                        Nothing -> Right $ "no type binding for variable " ++ x

typeof (TAbs x ty1 t2) ctx = let ctx' = (x,ty1):ctx
                             in case typeof t2 ctx' of
                                  Left ty2 -> Left $ TyArrow ty1 ty2
                                  Right err -> Right err

typeof (TApp t1 t2) ctx = case typeof t2 ctx of
                            Left ty11 ->
                                case typeof t1 ctx of
                                  Left (TyArrow ty11' ty12) -> if subtype ty11 ty11'
                                                               then Left ty12
                                                               else Right $ "cannot apply, "
                                                                        ++ show ty11
                                                                        ++ " is not a subtype of "
                                                                        ++ show ty11'
                                  Left ty1 -> Right $ "cannot apply type "
                                                   ++ show ty1
                                  Right err -> Right err
                            Right err -> Right err

typeof (TRecord ts) ctx = case typeField ts ctx of
                            Left tys -> Left $ TyRecord tys
                            Right err -> Right err

typeof (TProj t n) ctx = case typeof t ctx of
                           Left (TyRecord tys) -> case lookup n tys of
                                                    Just ty -> Left ty
                                                    Nothing -> Right $ "record "
                                                               ++ showFields tys
                                                               ++ " does not contain "
                                                               ++ n
                           Left ty -> Right $ "cannot project type" ++ show ty
                           Right err -> Right err

typeField :: [(Name, Term)] -> Context -> Either [(Name, Type)] TypeError
typeField [] ctx = Left []
typeField ((n,t):ts) ctx = case typeof t ctx of
                             Left ty -> case typeField ts ctx of
                                          Left tys -> Left $ (n,ty):tys
                                          Right err -> Right err
                             Right err -> Right err

prettyType :: Term -> IO ()
prettyType t = case typeof t [] of
                 Left ty -> putStrLn $ show ty
                 Right err -> putStrLn $ "Type Error: " ++ err

-- Subtyping

subtype :: Type -> Type -> Bool
subtype s TyTop = True

subtype (TyRecord _) (TyRecord []) = True
subtype (TyRecord ss) (TyRecord ((l,t):ts)) =
    case lookup l ss of
      Just s -> subtype s t && subtype (TyRecord ss) (TyRecord ts)
      Nothing -> False

subtype (TyArrow s1 s2) (TyArrow t1 t2) = subtype t1 s1 && subtype s2 t2

subtype _ _ = False

main :: IO ()
main = print $ eval (TRecord [])
