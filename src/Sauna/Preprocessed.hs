module Sauna.Preprocessed where

import Sauna.Data.Word (Word)
import Sauna.Data.Response (Response)
import Sauna.Data.State (State(State))
import Data.Maybe (Maybe, listToMaybe)
import Data.Function (($))
import System.IO (readFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (read)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Tuple (fst)
import Data.String (lines)
import Data.Functor ((<$>))
import Control.Monad (return)

cache :: [[(Word,Response)]]
cache =  read <$> lines (unsafePerformIO $ readFile "cache.txt")

-- | Return preprocessed guess for given State.
preprocessed :: State -> Maybe Word
preprocessed (State s) = do
  cached <- find (isPrefixOf s) cache
  items <- stripPrefix s cached
  item <- listToMaybe items
  return $ fst item
