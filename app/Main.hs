{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sauna
import Sauna.Data.Color
import Sauna.Data.Response
import Sauna.Data.State
import Data.Map (empty, lookup, insert)

import Prelude hiding (Word, init, lookup)

import System.IO (hFlush, stdout, hPutStrLn, stderr,hPutStr, hPrint)
import Data.Wrapper (unwrap, wrap)
import Data.Monoid (getSum)
import Data.List (sortBy)
import Control.Concurrent (newMVar, readMVar, modifyMVar_)
import GHC.IO (unsafePerformIO)

-- https://hackage.haskell.org/package/uglymemo-0.1.0.1/docs/src/Data-MemoUgly.html
memorize :: (Ord a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memorize f = do
    v <- newMVar empty
    let f' x = do
            m <- readMVar v
            case lookup x m of
                Nothing -> do let { r = f x }; modifyMVar_ v (return . insert x r); return r
                Just r  -> return r
    return f'


next' = unsafePerformIO $ memorize next


main :: IO ()
main = do
  --print [(word, getSum $ overlapScore init word) | word <- sortBy (overlapOrdering init) (unwrap fullDictionary)]
  loop init
  where
    loop state = do
      stats state
      word <- next' state
      print word
      hFlush stdout
      response::Response <- read <$> getLine
      if response /= (Response $ pure Green) then
        loop $ update state word response
      else
        loop init


--
stats :: State -> IO ()
stats state = do
--  hPrint stderr state
    hPutStr stderr "solutionFilter: :"
    hPrint stderr $ length $ filter (solutionFilter state) $ unwrap fullDictionary
    hPrint stderr $ filter (solutionFilter state) $ unwrap fullDictionary
--  hPutStr stderr "presentFilter:  :"
--  hPrint stderr $ length $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPrint stderr $ filter (presentFilter $ present state) $ unwrap dictionary
--  hPutStr stderr "optionsFilter:  :"
--  hPrint stderr $ length $ filter (optionsFilter $ options state) $ unwrap dictionary
--  hPrint stderr $ filter (optionsFilter $ options state) $ unwrap dictionary