module Sauna.Data.State where

import Sauna.Data.Letter


import Data.Quintuple (Quintuple)
import Text.Show (Show)


data State = State
  { options :: Quintuple [Letter]
  , present :: [Letter]
  , unused :: [Letter]
  }
  deriving Show

