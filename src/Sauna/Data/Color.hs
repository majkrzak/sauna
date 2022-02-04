module Sauna.Data.Color where

import Data.Eq (Eq)
import Text.Read (Read(readsPrec))
import Text.Show (Show(show))

data Color = Black | Yellow | Green
  deriving Eq

instance Show Color where
  show Black = "B"
  show Yellow = "Y"
  show Green = "G"

instance Read Color where
  readsPrec _ ('B':r) = [(Black,r)]
  readsPrec _ ('Y':r) = [(Yellow,r)]
  readsPrec _ ('G':r) = [(Green,r)]
  readsPrec _ _ = []
