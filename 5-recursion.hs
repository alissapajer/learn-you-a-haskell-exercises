import System.Environment

-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
power :: (Fractional a) => Int -> Int -> a
power x y
  | y == 0 = 1
  | y < 0 = (1 / intX) * (power x (y+1))
  | otherwise = intX * (power x (y-1))
  where intX = fromIntegral x

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1,0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]	
-- try to use a where clause
fib :: (Num a, Ord a) => a -> [a]
fib x
  | x < 0 = error "not defined"
  | x == 0 = [0]
  | x == 1 = 1:(fib 0)
fib x = (first + second) : rest
  where rest = fib (x-1)
        (first, second) = case rest of (x:y:_) -> (x,y)
                                       [x] -> (x,0)  -- TODO we should never hit these two last cases
                                       [] -> (0,0)
  

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a step = (-1) * (signum a) * ((abs a) + step)

{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance

 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)

 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)

 - The piCalc' function is defined as 
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 - 
 - You may find the stepReverseSign function handy
 -}

piCalc :: (Show a, Show b, Floating a, Integral b, Ord a) => a -> (a, b)
piCalc a = piCalc' 1 0 a 0

piCalc' :: (Ord a, Floating a, Integral b) => a -> a -> a -> b -> (a, b)
piCalc' den prevPi tol iters
  | abs (newPi - prevPi) <= tol = (prevPi, iters)
  | otherwise = piCalc' newDen newPi tol (iters + 1)
  where newPi = prevPi + (4 / den)
        newDen = stepReverseSign den 2

myMax :: (Ord a) => [a] -> a
myMax []  = undefined
myMax [x] = x
myMax xs
  | (head xs) > (last xs) = myMax (init xs)
  | otherwise             = myMax (tail xs)

myMax2 :: (Ord a) => [a] -> a
myMax2 []     = undefined
myMax2 [x]    = x
myMax2 (x:xs) = x `max` myMax2 xs

myReplicate :: Int -> a -> [a]
myReplicate b a
  | b < 1 = []
  | otherwise = take b (repeat a)

myReplicate2 :: Int -> a -> [a]
myReplicate2 b a
  | b < 1 = []
  | otherwise = a:(myReplicate (b-1) a)

myTake :: Int -> [a] -> [a]
myTake i _
  | i < 1 = []
myTake _ []     = []
myTake i (x:xs) = x:(myTake (i-1) xs)

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _          = []
myZip _ []          = []
myZip (a:as) (b:bs) = (a,b) : myZip as bs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | a == x    = True
  | otherwise = myElem a xs

divide :: (Ord a) => a -> [a] -> ([a],[a])
divide _ [] = ([],[])
divide p (a:as)
  | a < p = (a:(fst rec), snd rec)
  | otherwise = (fst rec, a:(snd rec))
  where rec = divide p as

divide2 :: (Ord a) => a -> [a] -> ([a],[a])
divide2 _ [] = ([],[])
divide2 p as = (left, right)
  where left = [x | x <- as, x < p]
        right = [x | x <- as, x >= p]
  
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) = (quicksort left) ++ [a] ++ (quicksort right)
  where (left, right) = divide2 a as

main :: IO ()
main = do
  [tol] <- getArgs
  let result = piCalc (read tol :: Double)
  print (show result)


-- blog post
-- why does main method need to be at end in order for :l to load all the things?
-- reading the tol as a Float vs Double
-- Double is how it's read in the ghci, why?
-- tail recusion - how can we get this method to complete more iterations?
-- it stack overflows when run through the main method
