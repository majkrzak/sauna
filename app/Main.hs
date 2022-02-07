{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sauna
import Sauna.Data.Color
import Sauna.Data.Response
import Sauna.Data.State
import Data.Map (empty, lookup, insert)

import Prelude hiding (Word, init, lookup)

import Data.Quintuple
import Sauna.Preprocessed
import System.IO (hFlush, stdout, hPutStrLn, stderr,hPutStr, hPrint)
import Data.Wrapper (unwrap, wrap)
import Data.Monoid (getSum)
import Data.List (sortBy)
import Control.Concurrent (newMVar, readMVar, modifyMVar_)
import GHC.IO (unsafePerformIO)
import Data.Maybe (fromJust)
import Data.Foldable (minimumBy)
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
      if response /= (Response $ pure Green) then
        loop $ update state word response
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

--main :: IO ()
--main = do
--  let
--    states = [State [(fromJust $ preprocessed initialize, response)] |response <- [Response (Quintuple (x1,x2,x3,x4,x5)) | x1 <- [Black ..], x2 <- [Black ..],x3 <- [Black ..],x4 <- [Black ..],x5 <- [Black ..]]]
--    level2 = [minimumBy (\a b -> compare (snd a) (snd b)) (fmap (\word -> (word, eliminationScore state word)) (unwrap fullDictionary)) | state <- states]
--  parallel_ ((
--      \x -> do
--        print x
--        hFlush stdout
--     ) <$> zip states level2)
--  stopGlobalPool
