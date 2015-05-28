{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative

-- Silence, flycheck!
main :: IO ()
main = print ""

-- Either

data Either_ e a = Left_ e | Right_ a
  deriving Show

instance Functor (Either_ e) where
    fmap _ (Left_ x) = Left_ x
    fmap g (Right_ x) = Right_ (g x)


type Value = Either String Int

t1 = Right_ 1
t2 = Left_ "1"

double :: a -> (a, a)
double x = (x, x)


-- ((->) e)

-- instance Functor ((->) e) where
--     fmap g a = g a

t3 :: Int -> String
t3 = show

t31 :: (String, String)
t31 = double $ t3 4

t32 :: (String, String)
t32 = fmap double t3 $ 4

-- Same thing !


-- ((,) e)

-- instance Functor ((,) e) where
--     fmap g (a, b) = (a, g b)

-- Pair

data Pair a = Pair a a
  deriving Show

instance Functor Pair where
    fmap g (Pair a b) = Pair (g a) (g b)

-- Tree

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
    fmap g (Leaf h) = Leaf $ g . h
    fmap g (Node ns) = Node $ fmap (fmap g) ns

t4 = Node [Leaf (dd 2), Node [Leaf (dd 3), Leaf (dd 4)]]

dd a x = a * x

visit :: Int -> ITree a -> [a]
visit x (Leaf f) = [f x]
visit x (Node []) = []
visit x (Node (n:ns)) = visit x n ++ visit x (Node ns)

-- 4. Type of kind * -> * which cannot be made an instance of Functor

data Boo a b = Boo a
  deriving Show

-- instance Functor (Boo e) where
--     fmap g (Boo x) = Boo (g x)

-- Seems like a bogus type .. but cannot be made into a functor!

-- or so I thought.  GHC can derive the following functor

-- instance Functor (Boo e) where
--     fmap g (Boo x) = Boo x

-- But it does nothing useful since g is never applied

-- Composition of Functors

-- instance (Functor f, Functor f') => Functor (f f') where
--     fmap g a = fmap (fmap g) a

-- on the right track, but cannot compose Functors in instance (f
-- . f')
-- see
-- http://hackage.haskell.org/package/category-extras-0.53.3/docs/src/Control-Functor-Composition.html
-- for a workaround

newtype CompF f g a = CompF { runCompF :: f (g a) }

class Composition o where
    decompose :: (f `o` g) x -> f (g x)
    compose :: f (g x) -> (f `o` g) x

instance Composition CompF where
    compose = CompF
    decompose = runCompF

instance (Functor f, Functor f') => Functor (CompF f f') where
    fmap g = compose . fmap (fmap g) . decompose

-- instance (Show f, Show f') => Show (CompF f f' a) where
--     show = compose . show . decompose

-- Applying ...

--CompF [] Maybe Int
t5 = CompF [Just 2, Nothing]

-- need this instance with XFlexibleInstances
instance Show (CompF [] Maybe (Integer, Integer)) where
    show = show . decompose

-- but we do get [Just (2,2), Nothing]

t5b = [Just 2, Nothing]
-- fmap double t5b
-- [(Just 2,Just 2),(Nothing,Nothing)]

-- fmap (fmap double) t5b
-- [Just (2,2), Nothing]


-- Bogus functor which does not satisfy the first functor law

data Bogus a b = A a | B a deriving Show

instance Functor (Bogus a) where
    fmap _ (A x) = B x
    fmap _ (B x) = B x


-- Evil functor
data List_ a = Nil | Cons a (List_ a) deriving Show

instance Functor List_ where
    fmap _ Nil = Nil
    fmap g (Cons x xs) = Cons (g x) (Cons (g x) (fmap g xs))

tl = Cons 1 $ Cons 2 $ Cons 3 Nil

-- violates both functor laws

-- fmap id tl
-- L 1 (L 1 (L 2 (L 2 (L 3 (L 3 Nope)))))
-- fmap ((*2) . (+1)) tl
-- L 4 (L 4 (L 6 (L 6 (L 8 (L 8 Nope)))))
-- ((fmap (*2)) . (fmap (+1))) tl
-- L 4 (L 4 (L 4 (L 4 (L 6 (L 6 (L 6 (L 6 (L 8 (L 8 (L 8 (L 8 Nope)))))))))))

-- 4.3 Instances

zipWith_ :: (a -> b -> c) -> List_ a -> List_ b -> List_ c
zipWith_ _ Nil _ = Nil
zipWith_ _ _ Nil = Nil
zipWith_ g (Cons a as) (Cons b bs) = Cons (g a b) (zipWith_ g as bs)

l1 = Cons (*1) $ Cons (*2) $ Cons (*3) Nil
l2 = Cons 1 $ Cons 2 $ Cons 3 Nil

instance Applicative List_ where
    pure x = Cons x (pure x)
    gs <*> xs = zipWith_ ($) gs xs

-- Non-deterministic list
newtype NDList a = NDList { unNDList :: [a] } deriving Show

instance Functor NDList where
    fmap _ (NDList []) = NDList []
    fmap g (NDList (x:xs)) = NDList $ (g x) : (fmap g xs)

ll1 = NDList [(*1), (*2), (*3)]
ll2 = NDList [1, 2, 3]

-- instance Applicative NDList where
--     pure x = NDList [x]
--     (NDList []) <*> _ = NDList []
--     (NDList (g:gs)) <*> (NDList xs) = NDList $ (map g xs) ++ (gs <*> xs)

-- shorter
instance Applicative NDList where
    pure x = NDList [x]
    (NDList gs) <*> (NDList xs) = NDList [ g x | g <- gs, x <- xs ]

-- 4.5 Monoidal functor
class Applicative f => Monoidal f where
    unit :: f ()
    unit = pure ()
    (**) :: f a -> f b -> f (a,b)
    a ** b = pure (,) <*> a <*> b

-- Harder, found solution from the paper
-- http://www.soi.city.ac.uk/~ross/papers/Constructors.pdf
class Monoidal f => Applicative' f where
    pure' :: a -> f a
    pure' x = fmap (const x) unit
    (<*!>) :: f (a -> b) -> f a -> f b
    g <*!> x = fmap (uncurry id) (g Main.** x)

-- 5.2 Monad instances

instance Monad NDList where
    (NDList []) >>= _ = NDList $ []
    (NDList (x:xs)) >>= g = NDList $ (unNDList (g x)) ++ (unNDList ((NDList xs) >>= g))
    return = pure

test1 = ll2 >>= \x ->
        return $ x * 2

test2 = do x <- ll2
           return $ x * 2


class Monad' m where
    bind :: m a -> (a -> m b) -> m b
    ret :: a -> m a

instance Monad' [] where
    bind [] _ = []
    bind (x:xs) g = (g x) ++ (bind xs g)
    ret = pure

test3 = [1,2,3] >>= \x ->
        return $ x * 2

test4 = do x <- [1,2,3]
           return $ x * 2

test5 = [1,2,3] >>= return . (*2)

instance Monad' ((->) e) where
    bind k g = \x -> g (k x) x
    ret = pure

data Free f a = Var a
              | N (f (Free f a))

instance (Functor f) => Functor (Free f) where
    fmap g (Var x) = Var (g x)
    fmap g (N x) = N (fmap (fmap g) x)

instance (Functor f) => Monad (Free f) where
    return = Var
    (Var x) >>= g = g x
    (N x) >>= g = N (fmap (>>= g) x)

-- 5.3 Intuition

join' :: Monad m => m (m a) -> m a
join' x = x >>= id
--join' = (>>= id)

bind' :: (Functor m, Monad m) => m a -> (a -> m b) -> m b
bind' x k = join' $ fmap k x

fmap' :: (Functor m, Monad m) => (a -> b) -> m a -> m b
fmap' g x = bind' x (return . g)

-- 6.4 Composing Monads

distrib :: n (m a) -> m (n a)
distrib = undefined

join2 :: (Monad m, Monad n) => m (n (m (n a))) -> m (n a)
join2 a = a >>= \b ->
          distrib b >>= \c ->
          return $ join' c
-- join2 a = join' `fmap` (distrib =<< a)

--  10.2 FoldMap

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable [] where
    foldMap _ [] = mempty
    foldMap g (t:ts) = mappend (g t) (foldMap g ts)

data Nat = Nat Int deriving Show

instance Monoid Nat where
    mempty = Nat 0
    mappend (Nat a) (Nat b) = Nat $ a + b


-- 10.3 Derived folds

instance Monoid ([] a) where
    mempty = []
    mappend = (++)

toList :: Foldable f => f a -> [a]
toList xs = foldMap return xs

newtype All = All { getAll :: Bool }

instance Monoid All where
    mempty = All True
    mappend (All a) (All b) = All $ a && b

newtype Any = Any { getAny :: Bool }

instance Monoid Any where
    mempty = Any True
    mappend (Any a) (Any b) = Any $ a || b

all' :: Foldable t => (a -> Bool) -> t a -> Bool
--all' p ts = getAll $ foldMap (\t -> All (p t)) ts
all' p = getAll . foldMap (All . p)

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' p = getAny . foldMap (Any . p)

data Tree a = BNode (Tree a) (Tree a) | BLeaf a deriving Show

instance Foldable Tree where
    foldMap g (BLeaf x) = g x
    foldMap g (BNode a b) = mappend (foldMap g a) (foldMap g b)

tree1 :: Tree Int
tree1 = BNode (BNode (BLeaf 1) (BLeaf 2)) (BLeaf 3)

newtype Sum a = Sum { getSum :: a } deriving Show

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum a) (Sum b) = Sum $ a + b

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

tree2 :: Tree Float
tree2 = BNode (BNode (BLeaf 1.25) (BLeaf 2.25)) (BLeaf 3.5)

tree3 :: Tree String
tree3 = BNode (BNode (BLeaf "1") (BLeaf "2")) (BLeaf "3")
