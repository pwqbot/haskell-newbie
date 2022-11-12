module Parser where

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser x) = Parser $ \s -> do
    (x', s') <- x s
    return (f x', s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  (Parser f) <*> (Parser x) = Parser $ \s -> do
    (f', s1) <- f s
    (x', s2) <- x s1
    return (f' x', s2)

instance Monad Parser where
  (Parser x) >>= f = Parser $ \s -> do
    (x', s') <- x s
    runParser (f x') s'

instance MonadFail Parser where
  fail _ = Parser $ \s -> Nothing

char :: Char -> Parser Char
char c = Parser charP
  where
    charP [] = Nothing
    charP (x : xs)
      | x == c =
          Just (c, xs)
      | otherwise = Nothing

string :: String -> Parser String
string = mapM char

-- space :: Parser Char
-- space = char ' ' <|> char '\n' <|> char '\r' <|> '\t'
