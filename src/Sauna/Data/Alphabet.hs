module Sauna.Data.Alphabet where

import Data.Eq (Eq)
import Data.Wrapper (Wrapper)
import Sauna.Data.Letter (Letter)
import Text.Read (Read)
import Text.Show (Show)


newtype Alphabet = Alphabet [Letter]
  deriving (Eq, Show, Read)

instance Wrapper Alphabet [Letter]

