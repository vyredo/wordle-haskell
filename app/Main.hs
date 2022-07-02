module Main where

import Data.List (elemIndices)
import GameStateLib (assignColors, is5LetterWord)
import GuessLib (Guess (..), prettyPrint)
import RequestLib (isWordValid)
import System.Console.Pretty (Color (Black, Blue, Green, Red), color)
import WordLib (getRandomWords)

data GameState = GameState
  { randomWords :: String,
    guesses :: [[Guess]]
  }

main :: IO ()
main = do
  putStrLn (color Green " Green when letter is in correct position")
  putStrLn (color Blue " Blue when the letter of guess is incorrect position but is part of answer")
  putStrLn (color Black " Black when the letter of guess is not part of answer word")
  word <- getRandomWords
  gameLoop $ GameState {randomWords = word, guesses = []}

gameLoop :: GameState -> IO ()
gameLoop gameState
  | length gs >= 5 = putStrLn $ concat ["GameOver", "\n the answer is: ", rws]
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
