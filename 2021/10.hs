{-# LANGUAGE LambdaCase #-}
import Text.Parsec
import Data.List

expr :: Parsec String () ()
expr = const () <$> many chunk
  where
    p a b = const () <$> between (char a) (char b) expr
    chunk = const () <$> p '(' ')'
                     <|> p '[' ']'
                     <|> p '{' '}'
                     <|> p '<' '>'

validate = parse expr ""

scoreSyntax :: String -> Int
scoreSyntax a = score $ validate a
  where
    n = length a
    score = \case
      Right () -> 0
      Left e -> if i>n then 0 else points (a!!(i-1))
        where i = sourceColumn $ errorPos e
              points = \case
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137
                _   -> 0

scoreAuto :: String -> Integer
scoreAuto = score . autocomplete
  where
    score = value . map points
      where
        value = foldl (\a b -> 5*a + b) 0
        points = \case
          '(' -> 1
          '[' -> 2
          '{' -> 3
          '<' -> 4
          _   -> 0
    autocomplete = foldl step []
      where step xs x | elem x "([{<" = x:xs
            step xs x | elem x ")]}>" = tail xs

median xs = sort xs !! div n 2
  where n = length xs

fork (f,g) x = (f x, g x)

main = interact $ show . fork (f,g) . lines
  where f = sum . map scoreSyntax
        g = median . map scoreAuto . filter valid
        valid = (0==) . scoreSyntax
