module Sauna.Utils where

import System.IO (IO, readFile)
import Text.Read (read)
import Control.Applicative ((<$>))
import Sauna.Data.Dictionary (Dictionary)

readDictionary :: IO Dictionary
readDictionary = read <$> readFile "dict.txt"
