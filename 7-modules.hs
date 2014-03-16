import Data.List
import DistanceConversions

-- Fill in the DistanceConversions module first, and import it here
-- create a higher-order function for converting an area between two dimensions
-- this will take the function for converting a distance, and an area to convert
-- using the functions defined in the DistanceConversions module
-- Example areaConv inchesToCentimetres 9 = 58.0644

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion area = linearConversion . linearConversion $ area

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = areaConv inchesToCentimetres

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = areaConv ((/100) . inchesToCentimetres . feetToInches . yardsToFeet . (*22))

addThreeLists :: (Num a) => [a] -> [a] -> [a] -> [a]
addThreeLists = zipWith3 (\x y z -> x + y + z)

--find out how many times each element appears in the list
countItems :: (Ord a) => [a] -> [(a, Int)]
countItems xs = map inner (group . sort $ xs)

--should only take a nonempty list with all elements equal
inner :: [a] -> (a, Int)
inner all@(x:_) = (x, length all)
inner [] = undefined

myOn :: (b -> b -> c) -> (a -> b) -> a -> a -> c
myOn f g x y = f (g x) (g y)
