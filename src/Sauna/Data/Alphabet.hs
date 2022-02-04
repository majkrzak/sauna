module Sauna.Data.Alphabet where

import Sauna.Data.Letter (Letter)
import Text.Read (Read)
import Text.Show (Show)
import Data.Wrapper (Wrapper)
import Data.Eq (Eq)


newtype Alphabet = Alphabet [Letter]
  deriving (Eq, Show, Read)

instance Wrapper Alphabet [Letter]

