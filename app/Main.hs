module Main where

import Data.List (elemIndices)
import MyLib (getRandomWords)
import System.Console.Pretty (Color (..), color)

type Guess = [(Color, Char)]

data GameState = GameState
  { randomWords :: String,
    guesses :: [Guess]
  }

{-
TODO:
1. add random words
2. if guesses is not valid english word, then run gameLoop with log
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
    let result = is5LetterWord guess
    case result of
      Just g' ->
        let guessColored = assignColors rws g'
            isMatch = g' == rws
            allGreen = [map (\char -> (Green, char)) rws]
            gs' = if isMatch then guessColored : allGreen else guessColored : gs
         in do
              putStrLn "Your guesses are: "
              mapM_ (putStrLn . prettyPrint) $ reverse gs'
              if isMatch
                then putStrLn "You Win!"
                else gameLoop $ gameState {guesses = gs'}
      Nothing -> do
        print "Make sure the guess is 5 characters long"
        putStrLn "Your guesses are: "
        mapM_ (putStrLn . prettyPrint) gs
        gameLoop gameState
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

prettyPrint :: Guess -> String
prettyPrint = foldr (\g acc -> printOne g ++ acc) ""
  where
    printOne :: (Color, Char) -> String
    printOne g = color (fst g) [(snd g)]

assignColors :: [Char] -> [Char] -> Guess
assignColors rw = go 0
  where
    go :: Int -> [Char] -> Guess
    go _ [] = []
    go i (c : cs)
      | rw !! i == c = (Green, c) : go (i + 1) cs
      | c `elem` rw = (Blue, c) : go (i + 1) cs
      | otherwise = (Black, c) : go (i + 1) cs
