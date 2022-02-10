module Main where

import Prelude hiding (Word, init, lookup)
import Sauna
import Sauna.Data.Color
import Sauna.Data.Response
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  loop initialize
 where
  loop state = do
    let word = guess state
    print word
    hFlush stdout
    response :: Response <- read <$> getLine
    if response /= Response (pure Green)
      then loop $ next state (word, response)
      else loop initialize
