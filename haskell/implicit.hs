-- Example from [OSC+12] "The Implicit Calculus"

sort :: (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort cmp (p:xs) = (sort cmp lesser) ++ [p] ++ (sort cmp greater)
    where lesser = filter (cmp p) xs
          greater = filter (\x -> not $ cmp p x) xs

-- with Ord typeclass

osort :: Ord a => [a] -> [a]
osort = sort (>)

-- without Ord typeclass

data Fruit = Banana
           | Apple
  deriving Show

cmpFruit :: Fruit -> Fruit -> Bool
cmpFruit Banana _ = True
cmpFruit Apple _ = False

-- and with Ord typeclass again for Fruits

instance Eq Fruit where
    Banana == Banana = True
    Apple == Apple = True
    _ == _ = False

instance Ord Fruit where
    (>) = cmpFruit

-- tests

main :: IO ()
main = do
  print $ osort [4, 3, 1, 2, 5, 10, 0, 3] -- with Ord
  print $ osort [[4, 3, 1], [2, 5, 10], [0, 3]] -- with Ord
  print $ sort cmpFruit [Banana, Apple, Apple, Banana, Apple] -- without Ord
  print $ osort [Banana, Apple, Apple, Banana, Apple]         -- with Ord
