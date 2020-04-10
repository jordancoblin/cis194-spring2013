-- Towers of Hanoi

-- Three pegs, disks stacked on first peg. Must moved disks from a to b (extra peg is c)
-- Solution:
-- 1. Move n-1 disks from a to c using b as temp storage.
-- 2. Move largest disk from a to b.
-- 3. Move n-1 disks from c to b using temp storage.

type Peg = String
type Move = (Peg, Peg)

-- Without guards
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi 1 a b c = [(a, b)]
-- hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- With guards
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to storage
    | n == 1 = [(from, to)]
    | otherwise = hanoi (n-1) from storage to ++ [(from, to)] ++ hanoi (n-1) storage to from