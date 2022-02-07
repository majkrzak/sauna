module Sauna.Data.State where

import Text.Show (Show)
import Sauna.Data.Word (Word)
import Sauna.Data.Response (Response)
import Data.Eq (Eq)
import Text.Read (Read)
import Data.Wrapper (Wrapper)
import Data.Ord (Ord)
import Data.Function.Memoize (deriveMemoizable)

newtype State = State [(Word, Response)]
  deriving (Eq,Ord, Show, Read)


deriveMemoizable ''State

instance Wrapper State [(Word, Response)]
