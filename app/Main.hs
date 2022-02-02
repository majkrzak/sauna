{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sauna
import Sauna.Data
import Sauna.Utils

import Prelude hiding (Word)

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  loop initialState
  where
    loop state = do
      let word = next state
      print word
      hFlush stdout
      response::Response <- read <$> getLine
      if response /= (Response $ pure Green) then
        loop $ update state word response
      else
        loop initialState



