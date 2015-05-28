import Data.Maybe

type Name = String

data Term = TVar Name
          | TAbs Name Type Term
          | TApp Term Term
          | TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
          deriving Show

freevar :: Name -> Term -> Bool
freevar x (TVar y) = x == y
freevar x (TAbs y _ t1) = x /= y && freevar x t1
freevar x (TApp t1 t2) = freevar x t1 || freevar x t2
freevar x _ = True

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

-- Eval

eval1 :: Term -> Maybe Term
eval1 (TApp (TAbs x _ t12) v2) = return $ subst x v2 t12

eval1 (TApp t1 t2) = do t1' <- eval1 t1
                        t2' <- eval1 t2
                        return $ TApp t1' t2'

eval1 (TIf TTrue t2 _) = return t2
eval1 (TIf TFalse _ t3) = return t3
eval1 (TIf t1 t2 t3) = do t1' <- eval1 t1
                          return $ TIf t1' t2 t3

eval1 (TSucc t1) = do t1' <- eval1 t1
                      return $ TSucc t1'

eval1 (TPred TZero) = return $ TZero

eval1 (TPred (TSucc nv)) = return nv

eval1 (TPred t1) = do t1' <- eval1 t1
                      return $ TPred t1'

eval1 (TIsZero TZero) = return TTrue

eval1 (TIsZero (TSucc _)) = return TFalse

eval1 (TIsZero t1) = do t1' <- eval1 t1
                        return $ TIsZero t1'

eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Just t' -> eval t'
                         Nothing -> t

-- Types

data Type = TyBool
          | TyNat
          | TyArrow Type Type
  deriving (Show, Eq)

type TypeError = String

type Context = [(Name, Type)]

typeof :: Term -> Context -> Either Type TypeError
typeof TTrue _ = Left TyBool
typeof TFalse _ = Left TyBool
typeof (TIf t1 t2 t3) ctx = let ty1 = typeof t1 ctx
                                ty2 = typeof t2 ctx
                                ty3 = typeof t3 ctx
                            in if ty1 == Left TyBool
                               then if ty2 == ty3
                                    then ty2
                                    else Right "type of branches do not match"
                               else Right "guard must be of type bool"
typeof TZero _ = Left TyNat
typeof (TSucc t1) ctx = if typeof t1 ctx == Left TyNat
                        then Left TyNat
                        else Right "succ must apply to a number"
typeof (TPred t1) ctx = if typeof t1 ctx == Left TyNat
                        then Left TyNat
                        else Right "pred must apply to a number"
typeof (TIsZero t1) ctx = if typeof t1 ctx == Left TyNat
                          then Left TyBool
                          else Right "isZero must apply to a number"

typeof (TVar x) ctx = case lookup x ctx of
                        Just ty -> Left ty
                        Nothing -> Right $ "no type binding for variable " ++ x

typeof (TAbs x ty t2) ctx = let ctx' = (x,ty):ctx
                            in case typeof t2 ctx' of
                                 Left ty2 -> Left $ TyArrow ty ty2
                                 Right err -> Right err

typeof (TApp t1 t2) ctx = case typeof t2 ctx of
                            Left ty11 ->
                                case typeof t1 ctx of
                                  Left (TyArrow ty11' ty12) -> if ty11' == ty11
                                                               then Left ty12
                                                               else Right $ "expected "
                                                                        ++ show ty11'
                                                                        ++ ", but found "
                                                                        ++ show ty11
                                  Left ty1 -> Right $ "cannot apply type "
                                                   ++ show ty1
                                  Right err -> Right err
                            Right err -> Right err

prettyType :: Term -> String
prettyType t = case typeof t [] of
                 Left ty -> show ty
                 Right err -> "Type Error: " ++ err
