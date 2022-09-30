module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

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

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

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
