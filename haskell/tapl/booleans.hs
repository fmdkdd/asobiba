import Data.Maybe

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | Wrong
  deriving Show

eval1 :: Term -> Maybe Term
eval1 (TIf TTrue t2 t3) = return t2
eval1 (TIf TFalse t2 t3) = return t3
eval1 (TIf t1 t2 t3) = do t1' <- eval1 t1
                          return $ TIf t1' t2 t3
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Just t' -> eval t'
                         Nothing -> t
