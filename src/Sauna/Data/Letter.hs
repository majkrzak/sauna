module Sauna.Data.Letter where

import Prelude (Enum,Bounded)
import Text.Read (Read)
import Text.Show (Show)
import Data.Eq (Eq)
import Data.Ord (Ord)


data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|Ä|Ö
  deriving (Eq, Show, Read, Enum, Bounded, Ord)
