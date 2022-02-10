module Sauna.Data.Letter where

import Data.Eq (Eq)
import Data.Function.Memoize (deriveMemoizable)
import Data.Ord (Ord)
import Prelude (Bounded, Enum)
import Text.Read (Read)
import Text.Show (Show)


data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|Ä|Ö
  deriving (Eq, Show, Read, Enum, Bounded, Ord)

deriveMemoizable ''Letter
