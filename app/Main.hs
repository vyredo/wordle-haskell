module Main where

import Data.List (elemIndices)
import MyLib (getRandomWords, isWordValid)
import System.Console.Pretty (Color (..), color)

{-
  * Previously I used a List of Tuple which I think simplify the code
    type Guess = [(Color, Char)]
  * However, I decide to use value constructor instead of tuple to demonstrate understanding of typeclass
-}

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

data GameState = GameState
  { randomWords :: String,
    guesses :: [[Guess]]
  }

{-
TODO:
1. if guesses is not valid english word, then continue
   - try to use http-service if word is valid english
-}

main :: IO ()
main = do
  putStrLn (color Green " Green when letter is in correct position")
  putStrLn (color Blue " Blue when the letter of guess is incorrect position but is part of answer")
  putStrLn (color Black " Black when the letter of guess is not part of answer word")
  word <- getRandomWords
  gameLoop $ GameState {randomWords = word, guesses = []}

gameLoop :: GameState -> IO ()
gameLoop gameState
  | length gs >= 5 = putStrLn $ "GameOver" ++ "\n the answer is: " ++ rws
  | otherwise = do
    putStrLn ""
    putStrLn "Enter a guess:"
    guess <- getLine
    result <- isWordValid guess
    case result of
      Left err -> do
        putStrLn $ color Red $ show err
        putStrLn "\n"
        putStrLn "Your guesses are: "
        mapM_ (putStrLn . prettyPrint) gs
        gameLoop gameState
      Right g' -> do
        putStrLn "Your guesses are: "
        mapM_ (putStrLn . prettyPrint) $ reverse gs'
        if isMatch
          then putStrLn "You Win!"
          else gameLoop $ gameState {guesses = gs'}
        where
          guessColored = assignColors rws g'
          isMatch = g' == rws
          allGreen = [map (\char -> Guess {getColor = Green, getCh = char}) rws]
          gs' = if isMatch then guessColored : allGreen else guessColored : gs
  where
    rws = randomWords gameState
    gs = guesses gameState

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
