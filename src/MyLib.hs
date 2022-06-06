{-# LANGUAGE OverloadedStrings #-}

module MyLib (getRandomWords, isWordValid) where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client.Conduit
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Simple
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

getRandomWords :: IO String
getRandomWords = do
  n <- rollDice
  return $ randomWords !! n
  where
    rollDice = randomRIO (0, wordsLength) :: IO Int

-- Word Error Type

data WordValidityError = WordNotFound | WordIsEmpty | WordIsNot5

instance Show WordValidityError where
  show WordNotFound = "Provide valid english word"
  show WordIsEmpty = "Provide non empty word"
  show WordIsNot5 = "Provide only 5 letter word"

-- HTTP Request

host :: [Char]
host = "https://api.wordnik.com"

generateUrl :: [Char] -> Request
generateUrl word = parseRequestThrow_ $ "https://api.wordnik.com/v4/word.json/" ++ word ++ "/definitions?limit=200&includeRelated=false&useCanonical=false&includeTags=false&api_key=a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5"

isWordValid :: String -> IO (Either WordValidityError String)
isWordValid word
  | word == "" = return $ Left WordIsEmpty
  | length word /= 5 = return $ Left WordIsNot5
  | otherwise = queryEnglishWord word
  where
    queryEnglishWord :: String -> IO (Either WordValidityError String)
    queryEnglishWord word = do
      let request = httpLBS (generateUrl word)
      eresponse <- try request
      case eresponse of
        Left e -> do
          let error = e :: HttpException
          return $ Left WordNotFound
        Right _ -> return $ Right word