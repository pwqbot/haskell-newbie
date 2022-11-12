{-# LANGUAGE BlockArguments #-}

module Monad_L where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Maybe (maybeToList)
import System.Random

father :: Sheep -> Maybe Sheep
father n
  | (n - 1) <= 0 = Nothing
  | otherwise = Just (n - 1)

mother :: Sheep -> Maybe Sheep
mother n
  | (n - 2) <= 0 = Nothing
  | otherwise = Just (n - 2)

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = return s >>= mother >>= father

fathersMaternalGrandmother s = return s >>= father >>= mother >>= mother

-- we can also use do-notation to build complicated sequences

type Sheep = Int

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do
  m <- mother s
  gf <- father m
  father gf

parent :: Sheep -> Maybe Sheep
parent s = father s `mplus` mother s

grandparent :: Sheep -> Maybe Sheep
grandparent s = parent s >>= parent

parents :: Sheep -> [Sheep]
parents s = maybeToList (father s) `mplus` maybeToList (mother s)

grandparents :: Sheep -> [Sheep]
grandparents s = do
  p <- parents s
  gp <- parents p
  return gp

traceFamily :: Sheep -> [Sheep -> Maybe Sheep] -> Maybe Sheep
traceFamily s actions = foldM getParent s actions
  where
    getParent s l = l s

allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations f [] = []
allCombinations f (l : ls) = foldr ff l ls
  where
    ff a b =
      ( do
          a' <- a
          b' <- b
          return (f a' b')
      )

allCombinations' :: (a -> a -> a) -> [[a]] -> [a]
allCombinations' f [] = []
allCombinations' f (l : ls) = foldr (liftM2 f) l ls

-- parent' :: (MonadPlus m) >= Sheep -> m Sheep
-- parent' s =

data Parsed = Digit Integer | Hex Integer | Word String deriving (Show)

parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c =
  if isHexDigit c
    then return (Hex (n * 16 + toInteger (digitToInt c)))
    else mzero
parseHexDigit _ _ = mzero

parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c =
  if isDigit c
    then return (Digit (n * 10 + toInteger (digitToInt c)))
    else mzero
parseDigit _ _ = mzero

parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c =
  if isAlpha c
    then return (Word (s ++ [c]))
    else mzero
parseWord _ _ = mzero

parse :: Parsed -> Char -> [Parsed]
parse p c = parseHexDigit p c `mplus` parseDigit p c `mplus` parseWord p c

parseArg :: String -> [Parsed]
parseArg s = do
  init <- return (Hex 0) `mplus` return (Digit 0) `mplus` return (Word "")
  foldM parse init s

data MyType = MT Int Bool Char Int deriving (Show)

makeRandomValue :: StdGen -> (MyType, StdGen)
makeRandomValue g =
  let (n, g1) = randomR (1, 100) g
      (b, g2) = random g1
      (c, g3) = randomR ('a', 'z') g2
      (m, g4) = randomR (-n, n) g3
   in (MT n b c m, g4)

getAny :: (Random a) => State StdGen a
getAny = do
  g <- get
  (x, g') <- return $ random g
  put g'
  return x

getOne :: (Random a) => (a, a) -> State StdGen a
getOne bounds = do
  g <- get
  (x, g') <- return $ randomR bounds g
  put g'
  return x

makeRandomValueST :: StdGen -> (MyType, StdGen)
makeRandomValueST =
  runState
    ( do
        n <- getOne (1, 100)
        b <- getAny
        c <- getOne ('a', 'z')
        m <- getOne (-n, n)
        return (MT n b c m)
    )

main1 :: IO ()
main1 = do
  env <- loadEnv
  let str = func1 env
  print str

data Environment = Environment
  { param1 :: String,
    param2 :: String,
    param3 :: String
  }

loadEnv :: IO Environment
loadEnv = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  return $ Environment s1 s2 s3

func1 :: Environment -> String
func1 env = "Result: " ++ (show (func2 env))

func2 :: Environment -> Int
func2 env = 2 + floor (func3 env)

func3 :: Environment -> Float
func3 env = (fromIntegral $ l1 + l2 + l3) * 2.1
  where
    l1 = length (param1 env)
    l2 = length (param2 env) * 2
    l3 = length (param3 env) * 3

func1' :: Reader Environment String
func1' = do
  res <- func2'
  return ("Result: " ++ show res)

func2' :: Reader Environment Int
func2' = do
  env <- ask
  let res3 = func3 env
  return (2 + floor res3)

-- getOne :: (Random a) => State StdGen a
