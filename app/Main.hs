{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Main where
import System.Environment
import qualified Control.Monad as Mo
import Ok
import qualified Control.Monad.Writer as W
import Control.Monad.State
import qualified Data.Monoid as M
import qualified Data.Map as Map
import qualified Data.Semigroup as M

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2 
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

lostNumbers = [4, 8, 15, 29]

big = [3, 2, 1] > [2, 1, 0]

sumV = sum [1, 2, 3, 4]

takeCycle = take 10 (cycle [1, 2, 3])

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

xys num = [x * y | x <- [2 ..], y <- [8 ..], x * y < num]

length' xs = sum [1 | _ <- xs]

rightTrinangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

factorial :: Integer -> Integer
factorial n = product [1 .. n]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Boy"
lucky x = "Sorry, fuck you"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "fuck"
head' (x : _) = x

-- tell :: (Show a) => [a] -> String
-- tell [] = "The list is empty"
-- tell [x] = "The list has one element: " ++ show x
-- tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
-- tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "you are too skinny"
  | bmi <= normal = "you are normal"
  | bmi <= fat = "you are fat"
  | otherwise = "what the fuck?"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs] where bmi w h = w / h ^ 2

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

zip' :: [a] -> [a] -> [(a, a)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let left = quicksort [y| y <- xs, y <= x]
      right = quicksort [y| y <- xs, y > x]
  in left ++ [x] ++ right

-- point free style
fn = ceiling . negate . tan . cos . max 50 

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- recursive function
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = a
myGCD a b = myGCD b reminder 
    where reminder = a `mod` b

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n - 1) xs

myCycle [] = []
myCycle (x:xs) = x : myCycle (xs ++ [x])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

fib a b 0 = (a, b)
fib a b c = fib b (a + b) (c - 1)

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs

cup flOz = \message -> message flOz -- sotre flOz in lambda
getCup aCup = aCup id
drinkCup aCup drinkOz = if ozDiff >= 0 
                        then cup ozDiff
                        else cup 0
                        where ozDiff = getCup aCup - drinkOz

data Box a = Box a deriving Show

simple :: a -> a
simple x = x

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ " " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female deriving Show 

data ABOType = Aa | Bb | AB | O

data RhType = Pos | Neg

data BloodType = BloodType ABOType RhType

data Name = Name String String

data Patient = Patient {
    name :: Name,
    sex :: Sex
}

jackie = Patient {name=Name "sb" "sb", sex=Male}

class FF a where
    fun1 :: a -> a
    fun2 :: a -> String
    fun3 :: a -> b -> Bool

class Describable a where
    describle :: a -> String

instance Describable Sex where
    describle Male = "ok"

data TwoSideDie = One | Two

-- Cipher
class Cipher a where
    decode :: a -> String -> String
    encode :: a -> String -> String

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = []
intToBits' 1 = [True]
intToBits' x = result
    where result = if reminder == 1
                   then True : intToBits' (x `div` 2)
                   else False : intToBits' (x `div` 2)
                   where reminder = x `mod` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits x = leadingFalses ++ reverse result
    where  leadingFalses = replicate num False
           num = maxBits - length result
           result = intToBits' x

charToBits :: Char -> Bits
charToBits x = intToBits $ fromEnum x 

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^fst x) bitspair)
    where bitspair = filter snd (zip ex bits)
          ex = [maxBits-1, maxBits-2 .. 0]

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
aPoint:: Point3D
aPoint = Triple 1 2 3

-- data Vec n a where
--     Vnil  :: Vec Zero a
--     Vcons :: a -> Vec n a -> Vec (Succ n) a
-- data Zero
-- data Succ a



data List a = Empty | Cons a (List a) deriving Show
myList1 = Cons 1 $ Cons 2 Empty


main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = "sadf" ++ name
    putStrLn statement


reverser :: IO () 
reverser = do
    input <- getContents
    let reversed = take 4 input
    putStrLn reversed

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName

userNameDB = Map.fromList [(1, "xingping"),
                           (2, "likajiang"),
                           (3, "xianggangjizhe")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("xingping", 2000),
                          ("likajiang", 3000)]


creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId = altLookUpCredits . lookupUserName

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits name = Map.lookup name creditsDB

altLookUpCredits :: Maybe UserName -> Maybe PlayerCredits
altLookUpCredits Nothing = Nothing
altLookUpCredits (Just a) = lookupCredits a

creditsFromId' id = lookupUserName id >>= lookupCredits

echo :: IO()
echo = putStrLn "enter a string" >> getLine >>= putStrLn

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PHD deriving (Eq, Ord, Enum, Show, Read)
data Candidate = Candidate 
    {
        candidateID :: Int,
        codeReview :: Grade,
        cultureFit :: Grade,
        education :: Degree
    } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where tests = [ 
            passCoding,
            passedCultureFit,
            educationMin ]
          passCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate > MS

candidate1 = Candidate {candidateID=1, codeReview=A, cultureFit=A, education=PHD}
candidate2 = Candidate {candidateID=2, codeReview=B, cultureFit=C, education=MS}
candidate3 = Candidate {candidateID=2, codeReview=A, cultureFit=C, education=BA}
candidates = [candidate1, candidate2, candidate3]

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

listOfTuples' = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1)
           ,(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)]
    Mo.guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

move3 x = moveKnight x >>= moveKnight >>= moveKnight

newtype Any' = Any' { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance M.Semigroup Any' where
   (<>) (Any' x) (Any' y) = Any' (x || y)

instance M.Monoid Any' where
    mempty = Any' False

ok = [Any' True, Any' False, Any' True]

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (M.Monoid w) => Functor (Writer w) where
    fmap f (Writer (x, v)) = Writer (f x, v)

instance (M.Monoid w) => Applicative (Writer w) where 
    pure x = Writer (x, mempty) 
    (<*>) (Writer (f, v)) (Writer (x, v')) = let y = f x in Writer (y, v `mappend` v')

instance (M.Monoid w) => Mo.Monad (Writer w) where
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got Number" ++ show x])

tell x = Writer ((),x)

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["fuck"]
    return (a * b)

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)

type Stack = [Int]

pop:: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push:: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip1 :: Stack -> (Int, Stack)
stackManip1 stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

newtype State s a = State { runState :: s -> (a, s) }
