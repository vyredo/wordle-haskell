module Main where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Identity
import Control.Monad.State
  ( MonadIO (..),
    MonadState (get, put),
    StateT (runStateT),
    evalState,
    execState,
    modify,
    runState,
  )
import Data.List (elemIndices)
import GHC.IO (unsafePerformIO)
import GameStateLib (Guess, assignColors, is5LetterWord)
import GuessLib (Guess (..), prettyPrint)
import RequestLib (WordValidityError, isWordValid)
import System.Console.Pretty (Color (Black, Blue, Green, Red), color)
import WordLib (getRandomWords)
import Prelude

data GameState = GameState
  { maxAttempt :: Int,
    answer :: String,
    guesses :: [[Guess]],
    getError :: String,
    userInput :: Either WordValidityError String,
    gameOver :: Bool,
    win :: Bool
  }

gs :: GameState
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
    word <- getRandomWords
    void $ runStateT loop gs {answer = word}

type GState m = (MonadState GameState m, MonadIO m)

loop :: GState m => m ()
loop = do
  state <- get
  if gameOver state || win state
    then pure ()
    else do
      runExceptT getInput
      stateLogic
      logState
      loop

getInput :: (MonadState GameState m, MonadError WordValidityError m, MonadIO m) => m ()
getInput = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Enter a guess:"
  line <- liftIO $ getLine >>= isWordValid
  let safeInput = do
        case line of
          Left err -> do
            throwError err
          Right word -> pure word
  modify (\gs -> gs {userInput = safeInput})

stateLogic :: GState m => m ()
stateLogic = do
  state <- get
  modify (\gs -> gs {getError = ""})
  case userInput state of
    Left err -> do
      modify (\gs -> gs {getError = show err})
    Right guess -> do
      let win = guess == answer state
      let gameOver = length (guesses state) >= maxAttempt state
      let g = checkGuessAndAssignColor guess (answer state)
      modify (\gs -> gs {guesses = guesses gs ++ [g], win = win, gameOver = gameOver})
  where
    checkGuessAndAssignColor :: String -> String -> [Guess]
    checkGuessAndAssignColor guess answer =
      if guess == answer
        then map (\char -> Guess {getColor = Green, getCh = char}) answer
        else assignColors answer guess

logState :: GState m => m ()
logState = do
  liftIO $ putStrLn ""
  s <- get
  liftIO $ log s
  where
    log s
      | getError s /= [] = do
        putStrLn $ color Red $ show $ getError s
      | gameOver s = do
        putStrLn "Game Over"
        putStrLn $ color Green $ "The answer was: " ++ answer s
      | win s = putStrLn $ "You Win, the answer is " ++ answer s
      | otherwise = do
        printGuesses s
      where
        printGuesses :: GameState -> IO ()
        printGuesses gs = do
          putStrLn "Your guesses:"
          mapM_ (putStrLn . prettyPrint) $ guesses gs
