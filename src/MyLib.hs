module MyLib (getRandomWords) where

import System.Random

randomWords =
  [ "hello",
    "world",
    "faith",
    "pouch",
    "spell",
    "crazy",
    "pizza",
    "enzym",
    "start",
    "tooth",
    "lunch",
    "print",
    "flash",
    "entry",
    "grand",
    "weigh",
    "field",
    "essay",
    "video",
    "motif",
    "trace",
    "evoke",
    "strap",
    "amuse",
    "cheat",
    "noise",
    "chase",
    "image",
    "grass",
    "proof",
    "child",
    "linen",
    "nerve",
    "lodge",
    "agony",
    "pupil",
    "apple",
    "score",
    "money",
    "chest",
    "waste",
    "dozen",
    "liver",
    "stand",
    "slime",
    "plane",
    "sharp",
    "thigh",
    "utter",
    "lover",
    "tight",
    "breed",
    "price",
    "twist",
    "pitch",
    "moral",
    "color",
    "adopt"
  ]

wordsLength :: Int
wordsLength = length randomWords - 1

rollDice = randomRIO (0, wordsLength) :: IO Int

getRandomWords = do
  n <- rollDice
  return $ randomWords !! n