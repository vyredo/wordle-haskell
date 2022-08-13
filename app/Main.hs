{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State
import Data.List (elemIndices)
import GameStateLib (assignColors, is5LetterWord)
import GuessLib (Guess (..), prettyPrint)
import RequestLib (WordValidityError, isWordValid)
import System.Console.Pretty (Color (Black, Blue, Green, Red), color)
import WordLib (getRandomWords)

data GameState = GameState
  { maxAttempt :: Int,
    answer :: String,
    guesses :: [[Guess]],
    getError :: String,
    userInput :: Either WordValidityError String,
    gameOver :: Bool,
    win :: Bool
  }

gs =
  GameState
    { maxAttempt = 5,
      answer = "",
      guesses = [],
      getError = "",
      gameOver = False,
      win = False,
      userInput = Right ""
    }

main :: IO ()
main =
  do
    putStrLn (color Green " Green when letter is in correct position")
    putStrLn (color Blue " Blue when the letter of guess is incorrect position but is part of answer")
    putStrLn (color Black " Black when the letter of guess is not part of answer word")
    word <- getRandomWords -- use Map and big list of random words
    loop gs {answer = word}

loop :: GameState -> IO ()
loop gs = do
  if gameOver gs || win gs
    then logState gs
    else do
      safeInput <- runExceptT getInput
      let (_, s) = runState stateLogic gs {userInput = safeInput}
      logState s
      loop s

getInput :: ExceptT WordValidityError IO String
getInput = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Enter a guess:"
  line <- liftIO $ getLine >>= isWordValid
  case line of
    Left err -> do
      -- mapM_ (putStrLn . prettyPrint) $ "what is line" ++ show err
      throwError err
    Right word -> pure word

stateLogic :: State GameState ()
stateLogic = do
  state <- get
  put (state {getError = ""})
  case userInput state of
    Left err -> do
      modify (\gs -> gs {getError = show err})
    Right guess -> do
      let win = guess == answer state
      let gameOver = length (guesses state) >= maxAttempt state
      let g = checkGuessAndAssignColor guess (answer state)
      modify (\gs -> gs {guesses = guesses gs ++ [g], win = win, gameOver = gameOver})

checkGuessAndAssignColor :: String -> String -> [Guess]
checkGuessAndAssignColor guess answer =
  if guess == answer
    then map (\char -> Guess {getColor = Green, getCh = char}) answer
    else assignColors answer guess

printGuesses :: GameState -> IO ()
printGuesses gs = do
  mapM_ (putStrLn . prettyPrint) $ guesses gs

logState :: GameState -> IO ()
logState s
  | getError s /= [] = do
    putStrLn $ color Red $ show $ getError s
  | gameOver s = putStrLn "Game Over"
  | win s = putStrLn $ "You Win, the answer is " ++ answer s
  | otherwise = do
    printGuesses s
