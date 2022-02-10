module Sauna.Data.State where

import Data.Eq (Eq)
import Data.Function.Memoize (deriveMemoizable)
import Data.Ord (Ord)
import Data.Wrapper (Wrapper)
import Sauna.Data.Response (Response)
import Sauna.Data.Word (Word)
import Text.Read (Read)
import Text.Show (Show)


newtype State = State [(Word, Response)]
  deriving (Eq,Ord, Show, Read)

deriveMemoizable ''State

instance Wrapper State [(Word, Response)]
