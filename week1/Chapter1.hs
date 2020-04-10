y :: Int
y = y + 1

a :: Bool
a = True

b :: String
b = "hello there"

c :: Char
c = 'c'

-- Compute sum of integers from 1 to n
sumtutorial :: Int -> Int
sumtutorial 0 = 0
sumtutorial n = n + sumtutorial (n-1)

sumtutorial2 :: Int -> Int
sumtutorial2 n
    | n == 0 = 0
    | otherwise = n + sumtutorial2 (n-1)

isEven :: Int -> Bool
isEven n = mod n 2 == 0

sumPair :: (Int, Int) -> Int
sumPair (n, m) = n + m

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

hailstoneSeq :: Int -> [Int]
hailstoneSeq 1 = [1]
hailstoneSeq n = [n] : hailstoneSeq (n-1)