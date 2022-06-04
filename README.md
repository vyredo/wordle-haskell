# WORDLE in Haskell

## How to run
-  Run Main.hs
-  In current version the answer is hardcoded to `hello`
-  There are 3 Color Scheme:
   - Green if the letter of guess is in correct position 
   - Blue if the letter of guess is incorrect position but is part of answer
   - Black if the letter of guess is not part of answer

I start the initial version of this app using Typescript (the code is in `ts-version` folder), I try to make the code as close to Haskell syntax.
But the Typescript version is not stable and no longer maintained.

### TODO:
1. Refactor helper functions into `MyLib.hs`
2. Currently the word is hardcoded to `hello`, should add feature to select any random 5 letters word.
3. Check if guess submitted is a valid english word, plan to use http service for this feature
   - A good exercise to run http request in Haskell

