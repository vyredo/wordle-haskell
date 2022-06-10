module GuessLib (Guess (..), prettyPrint) where

import System.Console.Pretty (Color, color)

{-
  * Previously I used a List of Tuple which I think simplify the code like below
        type Guess = [(Color, Char)]
  * However, I decide to use Record instead of tuple to demonstrate understanding of typeclass
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