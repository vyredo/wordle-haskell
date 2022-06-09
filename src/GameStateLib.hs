module GameStateLib (Guess (..), guessToString, is5LetterWord, assignColors) where

import GuessLib (Guess (..))
import System.Console.Pretty (Color (..), color)

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
