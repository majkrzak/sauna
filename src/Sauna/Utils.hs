module Sauna.Utils where

import Sauna.Data (Dictionary(..))
import System.IO (IO, readFile)
import Text.Read (read)
import Control.Applicative ((<$>))

readDictionary :: IO Dictionary
readDictionary = read <$> readFile "dict.txt"
