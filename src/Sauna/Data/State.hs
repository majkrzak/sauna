module Sauna.Data.State where

import Sauna.Data (Letter)


import Data.Quintuple (Quintuple)


data State = State
  { options :: Quintuple [Letter]
  , present :: [Letter]
  }

