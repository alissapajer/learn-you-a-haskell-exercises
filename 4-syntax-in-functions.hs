-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit 0 = "one"
englishDigit 1 = "two"
englishDigit x = "unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y)
  | y /= 0 = x / y
  | otherwise = undefined

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList xs = False

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

listHead :: [a] -> a
listHead [] = error "nope"
listHead (x:_) = x

listLength :: (Num b) => [a] -> b
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

listSum :: (Num a) => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum(xs)

numberProdSort :: (RealFloat a) => a -> a -> String
numberProdSort x y
  | prod <= ten = "10 or under"
  | prod < twenty = "between 10 and 20"
  | otherwise = "20 or greater"
  where prod = x * y
        (ten, twenty) = (10, 20)

myMax :: (Ord a) => a -> a -> a
myMax x y
  | x > y = x
  | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
myCompare x y
  | x < y = LT
  | x == y = EQ
  | otherwise = GT

listCrazy :: (RealFloat a) => [(a, a)] -> [a]
listCrazy xs = [crazy x y | (x, y) <- xs]
  where crazy a b = a * b * 17

listCrazy_v2 :: (RealFloat a) => [(a, a)] -> [a]
listCrazy_v2 xs = [crazy x y | (x, y) <- xs, let crazy a b = a * b * 17]

cylinderSA :: (RealFloat a) => a -> a -> a
cylinderSA radius height = result
  where tops = 2 * pi * radius ^ 2
        circ = 2 * pi * radius
        side = circ * height
        result = tops + side

cylinderSA_v2 :: (RealFloat a) => a -> a -> a
cylinderSA_v2 radius height =
  let tops = 2 * pi * radius ^2
      circ = 2 * pi * radius
      side = circ * height
  in tops + side
