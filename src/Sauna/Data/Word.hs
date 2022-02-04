module Sauna.Data.Word where

import Sauna.Data.Letter (Letter)
import Data.Quintuple (Quintuple(Quintuple))
import Data.Eq (Eq)
import Data.Wrapper (Wrapper, unwrap)
import Text.Show (Show(show))
import Text.Read (Read(readsPrec), reads)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String)
import Data.Foldable (foldMap, toList)
import Control.Monad (return)
import Data.Function ((.))


newtype Word = Word (Quintuple Letter)
  deriving (Eq)

instance Wrapper Word (Quintuple Letter)

instance Show Word where
  show = foldMap show . toList . unwrap

instance Read Word where
  readsPrec _ (l1:l2:l3:l4:l5:r) = case (do
      _l1 <- readMaybe [l1]
      _l2 <- readMaybe [l2]
      _l3 <- readMaybe [l3]
      _l4 <- readMaybe [l4]
      _l5 <- readMaybe [l5]
      return (Word (Quintuple (_l1, _l2, _l3, _l4, _l5)))
    ) of
      Just w -> [(w,r)]
      Nothing -> []
    where readMaybe :: (Read a) => String -> Maybe a
          readMaybe s = case reads s of
                        [(x, "")] -> Just x
                        _ -> Nothing
  readsPrec _ _ = []



