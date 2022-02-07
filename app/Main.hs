{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sauna
import Sauna.Data.Color
import Sauna.Data.Response
import Sauna.Data.State

import Prelude hiding (Word, init, lookup)

import System.IO (hFlush, stdout, stderr,hPutStr, hPrint)
import Data.Wrapper (unwrap)
import Control.Concurrent.ParallelIO



main :: IO ()
main = do
  --print [(word, getSum $ overlapScore init word) | word <- sortBy (overlapOrdering init) (unwrap fullDictionary)]
  loop initialize
  where
    loop state = do
      stats state
      let word = guess state
      print word
      hFlush stdout
      response::Response <- read <$> getLine
      if response /= Response (pure Green) then
        loop $ next state (word,response)
      else
        loop initialize


--
stats :: State -> IO ()
stats state = do
--  hPrint stderr state
    hPutStr stderr "solutionFilter: :"
    hPrint stderr $ length $ filter (solutionFilter state) $ unwrap fullDictionary
    hPrint stderr $ filter (solutionFilter state) $ unwrap fullDictionary
    hPutStr stderr "options:        :"
    hPrint stderr $ options state
    hPutStr stderr "unused:         :"
    hPrint stderr $ unused state
--  hPutStr stderr "presentFilter:  :"
--  hPrint stderr $ length $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPrint stderr $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPutStr stderr "optionsFilter:  :"
--  hPrint stderr $ length $ filter (optionsFilter $ options state) $ unwrap dictionary
--  hPrint stderr $ filter (optionsFilter $ options state) $ unwrap dictionary

