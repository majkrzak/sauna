module Sauna.Utils where

import Sauna.Data (Dictionary(..))

readDictionary :: IO Dictionary
readDictionary = read <$> readFile "dict.txt"
