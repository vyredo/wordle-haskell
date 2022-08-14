{-# LANGUAGE OverloadedStrings #-}

module WordLib (getRandomWords) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe
import System.Random

randomWords :: [String]
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
wordsLength = Prelude.length randomWords - 1

wordDic :: Map.Map Int String
wordDic = Map.fromList enumList
  where
    enumList = zip [0 ..] randomWords

getRandomWords :: IO String
getRandomWords = do
  n <- rollDice
  let word = Map.lookup n wordDic
  pure $ Data.Maybe.fromMaybe "" word
  where
    rollDice = randomRIO (0, wordsLength) :: IO Int
