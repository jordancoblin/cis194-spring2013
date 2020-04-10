
-- Logic:
-- 1. Double value of every second digit starting from right (last digit stays same). E.g. [1,3,8,5] -> [2,3,16,6]
-- 2. Add digts of doubled values & digits from original number. E.g. [2,3,16,6] -> 2+3+1+6+6 = 18
-- 3. Calculate remainder when sum is dividedb by 10. 
-- If result == 0, number is valud

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseList (toDigits x)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverseList (doubleEveryOtherFromLeft (reverseList l))

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x1:(x2:xs)) = x1 : [2 * x2] ++ doubleEveryOtherFromLeft xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumDigitsInteger x) + sumDigits xs

sumDigitsInteger :: Integer -> Integer
sumDigitsInteger 0 = 0
sumDigitsInteger x = (mod x 10) + sumDigitsInteger (div x 10)

-- Exercise 4
validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0


