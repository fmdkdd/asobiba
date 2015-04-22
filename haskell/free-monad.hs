{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

-- Comprehending the free monad, by following the examples in
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

-- The first approach is to define a datatype of our language.
data Toy0 b = Output0 b
            | Bell0
            | Done0

-- A program is a list of instructions.  All programs have the same type.
type Prog0 out = [Toy0 out]

prog00 :: Prog0 Char
prog00 = [Output0 'A', Done0]

prog01 :: Prog0 Char
prog01 = [Bell0, Output0 'A', Done0]

-- Extending the language is not an option here, since the datatype is closed.
-- Prog0 is solely a list of Toy0 instructions.  We have to allow more into the
-- type.

data Ext0 b = Put0 b
            | Get0

type Prog01 out = [Either (Toy0 out) (Ext0 out)]

prog010 = [Left (Output0 'A'), Right (Put0 'B'), Left (Done0)]

-- Using Either will work for two languages, but how do you scale?  Either3,
-- Either4, EitherN?  What if you don't know the type of the language when you
-- write the type of Prog?

data Toy1 b next = Output1 b next
                 | Bell1 next
                 | Done1

data Ext1 b next = Put1 b next
                 | Get1 next

prog11 :: Toy1 Char (Toy1 b next)
prog11 = Output1 'A'
         Done1

prog12 :: Toy1 b (Toy1 Char (Toy1 b1 next))
prog12 = Bell1 (Output1 'A' Done1)

prog13 :: Toy1 Char (Ext1 Char (Toy1 b next))
prog13 = Output1 'A' (Put1 'B' Done1)

-- We can compose languages.  However, programs do not have the same signature,
-- so we cannot just write an eval function yet.

-- That's where the Fix datatype comes into play.

data Fix f = Fix (f (Fix f))

prog11' :: Fix (Toy1 Char)
prog11' = Fix (Output1 'A' (Fix Done1))

prog12' :: Fix (Toy1 Char)
prog12' = Fix (Bell1 (Fix (Output1 'A' (Fix Done1))))

-- If we want to type a program composed of two languages, we are back to using
-- Either.  The free monad does not give you composition of types right away.
-- We should look to Swierstra for that.

-- prog13' = Fix (Output1 'A' (Fix (Put1 'B' (Fix Done1))))

-- In any case, the free monad does give you syntactic sugar for writing
-- sequential instructions in one language.  But what do you gain that you could
-- not do using a straight datatype definition?

data Toy2 b next = Output2 b next
                 | Bell2 next
                 | Done2
                 deriving (Functor, Show)

type Prog2 b = Free (Toy2 b) ()

output x = liftF (Output2 x ())
bell     = liftF (Bell2 ())
done     = liftF Done2

prog20 :: Prog2 Char
prog20 = output 'A'

prog21 = do
  prog20
  bell
  done

-- Now I can write evaluators for Prog2 programs.

showProg2 :: (Show b) => Prog2 b -> String
showProg2 (Free (Output2 a k)) = show a ++ showProg2 k
showProg2 (Free (Bell2 k)) = "*G*" ++ showProg2 k
showProg2 (Free Done2) = "done\n"
showProg2 (Pure ()) = "return\n"

-- This may be written more simply with iterM.
-- See http://stackoverflow.com/a/20575505
-- Though the setup is not exactly the same: here we are returning a value, and
-- in the SO answer they are just printing for side effects.

-- showProg2_go :: (Show b) => Toy2 b String -> String
-- showProg2_go (Output2 a k) = show a >> k
-- showProg2_go (Bell2 k) = "*G*" >> k
-- showProg2_go Done2 = "done\n"

-- showProg2' :: Prog2 String -> String
-- showProg2' = iterM showProg2_go

-- So, with an AST datatype, you have to explicitly build the program with
-- constructors, while the free monad gives you a nicer syntax to express your
-- programs in Haskell.  In both cases, you just get a data structure and no
-- side effects, and you are free to interpret this structure in any way by
-- feeding it to different evaluators.
