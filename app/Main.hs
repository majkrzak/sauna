{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sauna
import Sauna.Data.Color
import Sauna.Data.Response

import Prelude hiding (Word, init)

import System.IO (hFlush, stdout, hPutStrLn, stderr,hPutStr, hPrint)
import Data.Wrapper (unwrap)

main :: IO ()
main = do
  loop init
  where
    loop state = do
      --stats state
      let word = next state
      print word
      hFlush stdout
      response::Response <- read <$> getLine
      if response /= (Response $ pure Green) then
        loop $ update state word response
      else
        loop init


--
--stats :: State -> IO ()
--stats state = do
--  hPrint stderr state
--  hPutStr stderr "solutionFilter: :"
--  hPrint stderr $ length $ filter (solutionFilter state) $ unwrap dictionary
--  hPrint stderr $ filter (solutionFilter state) $ unwrap dictionary
--  hPutStr stderr "presentFilter:  :"
--  hPrint stderr $ length $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPrint stderr $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPutStr stderr "optionsFilter:  :"
--  hPrint stderr $ length $ filter (optionsFilter $ options state) $ unwrap dictionary
--  hPrint stderr $ filter (optionsFilter $ options state) $ unwrap dictionary