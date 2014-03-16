func1 :: (Num a) => a -> a
func1 x = x + 3

func2 :: (Num a) => a -> a
func2 x = x * 4


prepend :: a -> [a] -> [a]
prepend x xs = x : xs

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

--zipSum :: (Num a) => [a] -> [a] -> [a]
--zipSum xs ys = map summed (zip xs ys)
--  where summed = \(a, b) -> a + b

zipSum :: (Num a) => [a] -> [a] -> [a]
zipSum = zipWith (+)

myConcat :: [a] -> [a] -> [a]
myConcat = (++)

infixl 9 `leftassoc`
leftassoc :: (Num a) => a -> a -> a
leftassoc = (-)

infixr 9 `rightassoc`
rightassoc :: (Num a) => a -> a -> a
rightassoc = (-)
