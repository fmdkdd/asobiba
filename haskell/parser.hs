import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c,cs)])


instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f = Parser (\cs -> concat
                             [parse (f a) cs' | (a,cs') <- parse p cs])


test1 = do
  a <- item
  b <- item
  return (a,b)

test1Raw = item >>= (\a -> item >>= (\b -> return (a,b)))
test1Raw' = item >>= (\a ->
            item >>= (\b ->
            return (a,b)))

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p1 p2 = Parser (\cs -> parse p1 cs ++ parse p2 cs)

many :: Parser a -> Parser [a]
many p = do
  return []
  `mplus`
  do a <- p
     as <- many p
     return (a:as)

ynam :: Parser a -> Parser [a]
ynam p = do
  do a <- p
     as <- ynam p
     return (a:as)
  `mplus`
  return []

first :: Parser a -> Parser a
first p = Parser (\cs -> case parse p cs of
                           [] -> []
                           ((a,cs'):_) -> [(a,cs')])

sat :: (a -> Bool) -> Parser a -> Parser a
sat c p = Parser (\cs -> [(a,cs') | (a,cs') <- parse p cs, c a])


test2 = parse (first $ ynam (sat isAlpha item)) "hello"
test3 = parse (first $ ynam (sat isAlpha item)) "he123llo"
test4 = parse (ynam item) "hello"



digit :: Parser Char
digit = Parser (\s -> case s of
                   ('0':t) -> [('0',t)]
                   ('1':t) -> [('1',t)]
                   _ -> [])

integer :: Parser Float
integer = do
  p <- ynam digit
  return (read p)

--px :: Parser
px = Parser (\s -> case s of
                ('p':'x':t) -> [(Px,t)]
                _ -> [])

em = Parser (\s -> case s of
                ('e':'m':t) -> [(Em,t)]
                _ -> [])

unit = px `mplus` em

length = do
  l <- integer
  u <- unit
  return (u l)

-- data Rule = Rule Selector [Declaration]
-- data Selector = Tag String
--               | Id String
--               | Class String
--               | Wildcard
-- data Declaration = Declaration Property Value
-- newtype Property = Property String
-- data Value = LengthV Length
--            | ColorV Color
data Length = Px Float
            | Em Float deriving Show
-- data Color = Rgb Int Int Int
--            | Hsl Int Int Int
--            | Colorname String

-- length :: Tokens -> (Tokens, Length)
-- length (t:ts) =

--rule :: String -> (String, Rule)
