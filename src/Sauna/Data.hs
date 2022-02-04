

{-# LANGUAGE MultiParamTypeClasses #-}

module Sauna.Data where

import Data.Quintuple

import Prelude hiding (Word)
import Data.Foldable (toList)
import Data.Wrapper
import Sauna.Data.Letter
import Sauna.Data.Word
import Sauna.Data.Dictionary




data Color = Black | Gray | Yellow | Green
  deriving Eq

newtype Response = Response (Quintuple Color)
  deriving Eq




instance Show Color where
  show Black = "B"
  show Gray = "g"
  show Yellow = "Y"
  show Green = "G"

instance Read Color where
  readsPrec _ ('B':r) = [(Black,r)]
  readsPrec _ ('g':r) = [(Gray,r)]
  readsPrec _ ('Y':r) = [(Yellow,r)]
  readsPrec _ ('G':r) = [(Green,r)]

instance Show Response where
  show (Response w) = foldMap show $ toList w

instance Read Response where
  readsPrec _ (l1:l2:l3:l4:l5:r) = case (do
      _l1 <- readMaybe [l1]
      _l2 <- readMaybe [l2]
      _l3 <- readMaybe [l3]
      _l4 <- readMaybe [l4]
      _l5 <- readMaybe [l5]
      return $ Response $ Quintuple (_l1, _l2, _l3, _l4, _l5)
    ) of
      Just w -> [(w,r)]
      Nothing -> []
    where readMaybe :: (Read a) => String -> Maybe a
          readMaybe s = case reads s of
                        [(x, "")] -> Just x
                        _ -> Nothing
  readsPrec _ _ = []




instance Wrapper Response (Quintuple Color)

