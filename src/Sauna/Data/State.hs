module Sauna.Data.State where

import Text.Show (Show)
import Sauna.Data.Word (Word)
import Sauna.Data.Response (Response)
import Data.Eq (Eq)
import Text.Read (Read)
import Data.Wrapper (Wrapper)

newtype State = State [(Word, Response)]
  deriving (Eq, Show, Read)

instance Wrapper State [(Word, Response)]
