-- Tetris as simple as possible, but with all the bells and whistles.
-- Inspiration: https://github.com/imalooney/t3tr0s/blob/master/devblog/day01.md
-- https://github.com/tylerneylon/termtris

module Polyominoes where

import Data.Set

{- Polyominoes can be generated recursively.

A polyomino of size N on the plane is made from N contiguous ‘tiles’.
Disregarding rotation, there are few distinct polyominoes when N is
small.

(M is a tile, polyominoes are separated by one space)

N = 1 ........... M

N = 2 ........... MM

N = 3 ........... MMM M  MM
                      MM M


-}
data Shape = Fill Shape Shape Shape Shape | End deriving (Show, Eq, Ord)

-- Pretty printer
-- pp :: Shape -> String
-- pp End = ""
-- pp (Fill right down) = "m" ++ pp right ++ "\n" ++ pp down

-- Fill End End

-- Fill (Fill End End) End
-- Fill End (Fill End End)

grow :: Shape -> [Shape]
grow End = [Fill End End End End]
grow (Fill t r d l) = do t' <- grow t
                         r' <- grow r
                         d' <- grow d
                         l' <- grow l
                         [Fill t' r d l, Fill t r' d l, Fill t r d' l, Fill t r d l']

-- growRight :: Shape -> Shape
-- growRight End = Fill End End
-- growRight (Fill r d) = Fill (growRight r) d

-- growDown :: Shape -> Shape
-- growDown End = Fill End End
-- growDown (Fill r d) = Fill r (growDown d)
