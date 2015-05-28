{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

-- Sets

type Set a = [a]

member :: Eq a => a -> Set a -> Bool
member x [] = False
member x (a:as) | x == a    = True
                | otherwise = member x as

insert :: Eq a => a -> Set a -> Set a
insert x [] = [x]
insert x a | x `member` a = a
           | otherwise    = x : a

prop_1 :: Int -> Set Int -> Bool
prop_1 x s = member x (insert x s)


subset :: Eq a => Set a -> Set a -> Bool
subset [] _ = True
subset (a:as) bs | member a bs = as `subset` bs
                 | otherwise   = False

-- Reflexivity
prop_2'1 :: Set Int -> Bool
prop_2'1 a = a `subset` a

prop_2'2 :: Set Int -> Bool
prop_2'2 a = a `subset` (insert 2 a)

-- Anti-symmetry
prop_2'3 :: Set Int -> Set Int -> Property
prop_2'3 a b = a `subset` b && b `subset` a ==> a `equals` b

-- Transitivity
prop_2'4 :: Set Int -> Set Int -> Set Int -> Property
prop_2'4 a b c = a `subset` b && b `subset` c ==> a `subset` c


equals :: Eq a => Set a -> Set a -> Bool
equals a b = a `subset` b && b `subset` a

-- Reflexivity
prop_3'1 :: Set Int -> Bool
prop_3'1 a = a `equals` a

-- Symmetry
prop_3'2 :: Set Int -> Set Int -> Property
prop_3'2 a b = a `equals` b ==> b `equals` a

-- Transitivity
prop_3'3 :: Set Int -> Set Int -> Set Int -> Property
prop_3'3 a b c = a `equals` b && b `equals` c ==> a `equals` c


union :: Eq a => Set a -> Set a -> Set a
union [] bs = bs
union (a:as) bs = as `union` (insert a bs)

prop_4'0 :: Bool
prop_4'0 = ([1,2] `union` [3,4,1]) `equals` [1,2,3,4]

-- Coherence
prop_4'1 :: Set Int -> Set Int -> Bool
prop_4'1 a b = a `subset` ab && b `subset` ab
  where ab = a `union` b

-- Associativity
prop_4'2 :: Set Int -> Set Int -> Set Int -> Bool
prop_4'2 a b c = ab'c `equals` a'bc
  where ab'c = (a `union` b) `union` c
        a'bc = a `union` (b `union` c)

-- Left and right zero
prop_4'3 :: Set Int -> Bool
prop_4'3 a = a `equals` (a `union` []) && a `equals` ([] `union` a)

-- Commutativity
prop_4'4 :: Set Int -> Set Int -> Bool
prop_4'4 a b = (a `union` b) `equals` (b `union` a)


inter :: Eq a => Set a -> Set a -> Set a
inter [] _ = []
inter (a:as) bs | member a bs = a : as `inter` bs
                | otherwise   = as `inter` bs

prop_5'0 :: Bool
prop_5'0 = ([1,2] `inter` [3,4,1]) `equals` [1]

-- Coherence
prop_5'1 :: Set Int -> Set Int -> Bool
prop_5'1 a b = ab `subset` a && ab `subset` b
  where ab = a `inter` b

-- Associativity
prop_5'2 :: Set Int -> Set Int -> Set Int -> Bool
prop_5'2 a b c = ab'c `equals` a'bc
  where ab'c = (a `inter` b) `inter` c
        a'bc = a `inter` (b `inter` c)

-- Left and right absorb
prop_5'3 :: Set Int -> Bool
prop_5'3 a = ([] `equals` (a `inter` [])) && ([] `equals` ([] `inter` a))

-- A binary relation is a set of couple
type Relation a = Set (a,a)

base :: Relation a -> Set a
base [] = []
base ((x,y):as) = [x,y] ++ base as

prop_6'0 :: Bool
prop_6'0 = base [(1,2),(3,4),(1,4)] `equals` [1,2,3,4]


reflex :: Eq a => Set a -> Relation a -> Bool
reflex bs as = [ (b,b) | b <- bs ] `subset` as

ferm_reflex :: Eq a => Set a -> Relation a -> Relation a
ferm_reflex bs as = [ (b,b) | b <- bs ] `union` as

prop_7'1 :: Relation Int -> Bool
prop_7'1 a = reflex b (ferm_reflex b a)
  where b = base a


sym :: Eq a => Relation a -> Bool
sym a = all id [ (y,x) `member` a | (x,y) <- a ]

ferm_sym :: Eq a => Relation a -> Relation a
ferm_sym a = [ (y,x) | (x,y) <- a ] `union` a

prop_8'1 :: Relation Int -> Bool
prop_8'1 = sym . ferm_sym


trans :: Eq a => Relation a -> Bool
trans a = all id [ (x,z) `member` a | (x,y1) <- a, (y2,z) <- a, y1 == y2 ]

ferm_trans :: Eq a => Relation a -> Relation a
ferm_trans a | trans a   = a
             | otherwise = ferm_trans ([ (x,z) | (x,y1) <- a,
                       (y2,z) <- a, y1 == y2 ] `union` a)

prop_9'1 :: Relation Int -> Bool
prop_9'1 = trans . ferm_trans


rel_eq :: Eq a => Relation a -> Bool
rel_eq a = reflex (base a) a && sym a && trans a

ferm_eq :: Eq a => Relation a -> Relation a
ferm_eq a = ferm_trans $ ferm_sym $ ferm_reflex (base a) a

prop_10'1 :: Relation Int -> Bool
prop_10'1 = rel_eq . ferm_eq


main = $(quickCheckAll)
