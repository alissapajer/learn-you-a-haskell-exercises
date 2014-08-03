import Data.List
import Data.Monoid
import Data.Maybe

{-
 - We are going to create some types for a deck of cards
 - The cards need to have an ordering, based on the standard ranking http://en.wikipedia.org/wiki/Standard_52-card_deck#Rank_and_color
 - We are assuming Aces are high.
 - Therefore, the following statements should be true:
 -    (Card Ace Spades) > (Card King Spades)
 -    (Card Two Clubs) < (Card Three Clubs)
 -
 - We are going to provide our own implementation of the Show typeclass for the Card type.
 - When displaying the Card instance in GHCI, or calling show (Card digit suit), the String which should be displayed is "The <Digit> of <Suit>"
 -
 - Uncomment the following declarations to complete the implementation, and provide an implementation for instance Show Card
 -}

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord, Bounded)

data Digit = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord, Bounded)

data Card = Card Digit Suit deriving (Eq, Ord, Bounded)

instance Show Card where
    show (Card digit suit) = "The " ++ (show digit) ++ " of " ++ (show suit)

-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard x y
  | x <= y = y
  | otherwise = x

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
    play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
    play = elem maxBound

-- Create a new Coin type
data Coin = Heads | Tails deriving (Show, Eq)

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
instance Hand Coin where
    play = isInfixOf $ take 10 $ repeat Heads

-- Have a play with implementing Hand for some other types, for instance Int and Bool

instance Monoid Int where
    mempty = 0
    mappend = (+)

data MyList a = Empty | Cons a (MyList a) deriving (Show)

foo :: MyList Int
foo = Empty

bar :: MyList Int
bar = Cons 5 (Cons 7 Empty)

myConcat :: MyList a -> MyList a -> MyList a
myConcat Empty ys = ys
myConcat (Cons x xs) ys = Cons x $ myConcat xs ys

concatted = myConcat bar bar

data BTree a = EmptyTree | Node a (BTree a) (BTree a) deriving (Show)

insertNode :: (Ord a) => a -> BTree a -> BTree a
insertNode elem EmptyTree = Node elem EmptyTree EmptyTree  
insertNode elem tree@(Node a1 treeL treeR)
  | elem == a1 = tree
  | elem < a1 = Node a1 (insertNode elem treeL) treeR
  | elem > a1 = Node a1 treeL (insertNode elem treeR)
  
elemTree :: (Ord a) => a -> BTree a -> Bool
elemTree elem EmptyTree = False
elemTree elem (Node a1 treeL treeR)
  | elem == a1 = True
  | elem < a1 = elemTree elem treeL
  | elem > a1 = elemTree elem treeR

-- foldl :: (a -> b -> a) -> a -> [b] -> a

treeFromList :: (Ord a) => [a] -> BTree a
treeFromList [] = EmptyTree
treeFromList (x:xs) = foldl (flip insertNode) (Node x EmptyTree EmptyTree) xs

class Combiner a where
  combine :: a -> a -> a

instance Combiner Int where
  combine a b = a + b

data Box a = EmptyBox | Box a

instance (Combiner a) => Combiner (Box a) where
  combine EmptyBox box = box
  combine box EmptyBox = box
  combine (Box x) (Box y) = Box $ combine x y

pts :: [Int]
pts = [4,5,1,0,-1,9]

pts2 :: [Bool]
pts2 = [True, False, False]

-- how to tell if Maybe has a Functor, use :info Maybe
reduce :: (Combiner a) => [a] -> Maybe a
reduce [] = Nothing
reduce (x:[]) = Just x
reduce (x:xs) = fmap (combine x) $ reduce xs


reduce' :: (Combiner a) => [a] -> Maybe a
reduce' [] = Nothing
reduce' (x:xs) = foldl myf (Just x) xs

myf :: (Combiner a) => Maybe a -> a -> Maybe a
myf mb x = fmap (combine x) mb


--instance Combiner (Box Int) where
--  combine EmptyBox box = box
--  combine box EmptyBox = box
--  combine (Box x) (Box y) = Box $ x + y

--data NewBox a = NewBox a

--instance Combiner (NewBox Int) where
--  combine (NewBox x) (NewBox y) = x + y 


data Letter = Aa | Bb | Cc
instance Combiner Letter where
  combine _ Aa = Aa
  combine x _ = x

-- illustrate difference between foldl and foldr here
ltrs :: [Letter]
ltrs = [Bb, Cc, Aa]  --left: Aa, right: Bb

reducefoo :: (Combiner a) => [a] -> Maybe a
reducefoo = undefined

