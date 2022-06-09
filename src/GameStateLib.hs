module GameStateLib (Guess (..), prettyPrint, guessToString, is5LetterWord, assignColors) where

import System.Console.Pretty (Color (..), color)

data Guess = Guess
  { getColor :: Color,
    getCh :: Char
  }

class PrintGuess a where
  prettyPrint :: [a] -> String

instance PrintGuess Guess where
  prettyPrint = foldr (\g acc -> printOne g ++ acc) ""
    where
      printOne :: Guess -> String
      printOne g = color (getColor g) [getCh g]

-- Helper --

guessToString :: [(Color, Char)] -> String
guessToString = map snd

is5LetterWord :: String -> Maybe String
is5LetterWord word
  | length word == 5 = Just word
  | otherwise = Nothing

assignColors :: [Char] -> [Char] -> [Guess]
assignColors rw = go 0
  where
    go :: Int -> [Char] -> [Guess]
    go _ [] = []
    go i (c : cs)
      | rw !! i == c = Guess {getColor = Green, getCh = c} : go (i + 1) cs
      | c `elem` rw = Guess {getColor = Blue, getCh = c} : go (i + 1) cs
      | otherwise = Guess {getColor = Black, getCh = c} : go (i + 1) cs