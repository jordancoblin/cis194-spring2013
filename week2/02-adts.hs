data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King 
    deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False


data FailDouble = Failure 
    | OK Double
    deriving Show

safeDiv :: Double -> Double -> FailDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailDouble -> Double
failureToZero Failure = 0
failureToZero (OK x) = x

failureToZero2 :: FailDouble -> Double
failureToZero2 fd = case fd of 
    Failure -> 0
    OK x -> x

data Person = Person String Int Thing

jordan :: Person
jordan = Person "jordan" 26 King
chris = Person "chris" 27 Cabbage

getAge :: Person -> Int
getAge (Person _ age _) = age

checkFav :: Person -> String
checkFav (Person name _ King) = name ++ " is my name"
checkFav p = "otherwise this name "


-- Recursive types
data IntList = Empty | Cons Int IntList

sumList :: IntList -> Int
sumList x = case x of
    Empty -> 0
    Cons x xs -> x + sumList xs