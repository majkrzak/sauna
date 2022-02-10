module Sauna.Data.Dictionary where

import Data.Eq (Eq)
import Data.Foldable (foldl)
import Data.Function ((.))
import Data.Monoid ((<>))
import Data.Wrapper (Wrapper, unwrap)
import Sauna.Data.Word (Word)
import Text.Read (Read(readsPrec), reads)
import Text.Show (Show(show))


newtype Dictionary = Dictionary [Word]
  deriving (Eq)

instance Wrapper Dictionary [Word]

instance Show Dictionary where
  show = foldl (\ws w -> ws <> "\n" <> show w) "" . unwrap

instance Read Dictionary where
  readsPrec _ s = case reads s of
    [(x, ""       )] -> [(Dictionary [x], "")]
    [(x, "\n"     )] -> [(Dictionary [x], "")]
    [(x, '\n' : _s)] -> case reads _s of
      [(Dictionary xs, _)] -> [(Dictionary ([x] <> xs), "")]
      _                    -> []
    _ -> []
