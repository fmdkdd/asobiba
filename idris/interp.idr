-- Straight from the Idris tutorial

import Data.Vect
import Data.Fin

data Ty = TyInt | TyBool | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyInt       = Int
interpTy TyBool      = Bool
interpTy (TyFun A T) = interpTy A -> interpTy T

using (G:Vect n Ty) -- context as implicit argument
  data Expr : Vect n Ty -> Ty -> Type

  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
    Stop : HasType FZ (t :: G) t
    Pop  : HasType k G t -> HasType (FS k) (u :: G) t

   -- de Bruijn indexed variables (no names, number of lambdas between
   -- definition and use).
  data Expr : Vect n Ty -> Ty -> Type where
    Var : HasType i G t -> Expr G t
    Val : (x : Int) -> Expr G TyInt
    Lam : Expr (a :: G) t -> Expr G (TyFun a t)
    App : Expr G (TyFun a t) -> Expr G a -> Expr G t
    Op  : (interpTy a -> interpTy b -> interpTy c) ->
          Expr G a -> Expr G b -> Expr G c
    If  : Expr G TyBool ->
          Lazy (Expr G a) ->
          Lazy (Expr G a) -> Expr G a

  data Env : Vect n Ty -> Type where
    Nil  : Env Nil
    (::) : interpTy a -> Env G -> Env (a :: G)

  lookup : HasType i G t -> Env G -> interpTy t
  lookup Stop    (x :: xs) = x
  lookup (Pop k) (x :: xs) = lookup k xs

  interp : Env G -> Expr G t -> interpTy t
  interp env (Var i) = lookup i env
  interp env (Val x) = x
  interp env (Lam sc) = \x => interp (x :: env) sc
  interp env (App f s) = interp env f (interp env s)
  interp env (Op op x y) = op (interp env x) (interp env y)
  interp env (If x t e) = if interp env x then interp env t
                                          else interp env e

  add : Expr G (TyFun TyInt (TyFun TyInt TyInt))
  add = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))

  fact : Expr G (TyFun TyInt TyInt)
  fact = Lam (If (Op (==) (Var Stop) (Val 0))
                 (Val 1)
                 (Op (*) (App fact (Op (-) (Var Stop) (Val 1)))
                         (Var Stop)))

main : IO ()
main = do putStr "Enter a number: "
          x <- getLine
          print (interp [] fact (cast x))
