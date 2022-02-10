module Main where

import Sauna
import Sauna.Data.Color
import Sauna.Data.Word
import Sauna.Data.State
import Prelude hiding (Word, init, lookup)
import System.IO (hFlush,stdout)
import Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import Data.Wrapper (unwrap, wrap)

main :: IO ()
main = do
  parallel_ ((
          \x -> do
            print (unwrap $ solve x initialize)
            hFlush stdout
        ) <$> unwrap fullDictionary)
  stopGlobalPool

solve :: Word -> State -> State
solve word state
  | not (null (unwrap state)) && snd (last $ unwrap state) == wrap (pure Green) = state
  | otherwise = solve word (next state (guess state, check word $ guess state))
