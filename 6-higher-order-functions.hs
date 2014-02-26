-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts a b = undefined

-- Define a square function
sq :: Int -> Int
sq x = undefined

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares a b = undefined

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b = undefined

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = undefined

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = undefined

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the prodcut of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication = undefined

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial = undefined

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

divideByTwenty :: (Fractional a) => a -> a
divideByTwenty = (/20)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = [] 
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = (f a b):(zipWith f as bs)

threeFunc :: Char -> Bool -> String
threeFunc char bool = char : (show bool)
-- flip threeFunc :: (Bool -> Char -> String) 
-- (flip threeFunc) True 'a' == threeFunc 'a' True

flipThreeFunc :: Bool -> Char -> String
flipThreeFunc bool char = threeFunc char bool

flipV2 :: (Char -> Bool -> String) -> (Bool -> Char -> String)
flipV2 f bool char = f char bool

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f b a = f a b
--myZipWith (myFlip threeFunc) [True, False, True] "wejkdslfsd"

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = f a : myMap f as

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

filterNotNull :: [[a]] -> [[a]]
filterNotNull = filter notNull
  where notNull :: [a] -> Bool 
        notNull as = not (null as)

-- find the sum of all odd squares that are smaller than 10,000
oddSqrs :: Integer
oddSqrs = sum [x^2 | x <- [1..(floor (sqrt 10000))], odd x]
oddSqrsV2 = sum (takeWhile (<10000) [x^2 | x <- [1..], odd x])
oddSqrsV3 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq x
  | odd x = x : collatzSeq oddRes
  | otherwise = x : collatzSeq evenRes
  where oddRes = (3 * x) + 1
        evenRes = x `div` 2

-- for all starting numbers between 1 and 100, how many Collatz chains have a length greater than 15
collatzLarge :: Int
collatzLarge = length large
  where large = filter (>15) lengths
        lengths = [length (collatzSeq x) | x <- [1..100]]

thirtySix = ((map (*) [1,2,3,4,5]) !! 3) 9
