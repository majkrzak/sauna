

module Sauna.Data where

import Sauna.Data.Quintuple

import Prelude hiding (Word)
import Data.Foldable (toList)

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|Aumlaut|Oumlaut
  deriving Eq

newtype Word = Word (Quintuple Letter)

newtype Dictionary = Dictionary [Word]

data Color = Black | Gray | Yellow | Green

newtype Response = Response (Quintuple Color)

instance Show Letter where
  show A = "A"
  show B= "B"
  show C="C"
  show D="D"
  show E="E"
  show F="F"
  show G="G"
  show H="H"
  show I="I"
  show J="J"
  show K="K"
  show L="L"
  show M="M"
  show N="N"
  show O="O"
  show P="P"
  show Q="Q"
  show R="R"
  show S="S"
  show T="T"
  show U="U"
  show V="V"
  show W="W"
  show X="X"
  show Y="Y"
  show Z="Z"
  show Aumlaut="Ä"
  show Oumlaut="Ö"

instance Read Letter where
  readsPrec _ ('A':r) = [(A,r)]
  readsPrec _ ('B':r) = [(B,r)]
  readsPrec _ ('C':r) = [(C,r)]
  readsPrec _ ('D':r) = [(D,r)]
  readsPrec _ ('E':r) = [(E,r)]
  readsPrec _ ('F':r) = [(F,r)]
  readsPrec _ ('G':r) = [(G,r)]
  readsPrec _ ('H':r) = [(H,r)]
  readsPrec _ ('I':r) = [(I,r)]
  readsPrec _ ('J':r) = [(J,r)]
  readsPrec _ ('K':r) = [(K,r)]
  readsPrec _ ('L':r) = [(L,r)]
  readsPrec _ ('M':r) = [(M,r)]
  readsPrec _ ('N':r) = [(N,r)]
  readsPrec _ ('O':r) = [(O,r)]
  readsPrec _ ('P':r) = [(P,r)]
  readsPrec _ ('Q':r) = [(Q,r)]
  readsPrec _ ('R':r) = [(R,r)]
  readsPrec _ ('S':r) = [(S,r)]
  readsPrec _ ('T':r) = [(T,r)]
  readsPrec _ ('U':r) = [(U,r)]
  readsPrec _ ('V':r) = [(V,r)]
  readsPrec _ ('W':r) = [(W,r)]
  readsPrec _ ('X':r) = [(X,r)]
  readsPrec _ ('Y':r) = [(Y,r)]
  readsPrec _ ('Z':r) = [(Z,r)]
  readsPrec _ ('Ä':r) = [(Aumlaut,r)]
  readsPrec _ ('Ö':r) = [(Oumlaut,r)]
  readsPrec _ _ = []

instance Show Word where
  show (Word w) = foldMap show $ toList w

instance Read Word where
  readsPrec _ (l1:l2:l3:l4:l5:r) = case (do
      _l1 <- readMaybe [l1]
      _l2 <- readMaybe [l2]
      _l3 <- readMaybe [l3]
      _l4 <- readMaybe [l4]
      _l5 <- readMaybe [l5]
      return $ Word $ Quintuple (_l1, _l2, _l3, _l4, _l5)
    ) of
      Just w -> [(w,r)]
      Nothing -> []
    where readMaybe :: (Read a) => String -> Maybe a
          readMaybe s = case reads s of
                        [(x, "")] -> Just x
                        _ -> Nothing
  readsPrec _ _ = []

instance Show Dictionary where
  show (Dictionary l) = foldl (\ws w -> ws <> "\n" <> show w) "" l

instance Read Dictionary where
  readsPrec _ s = case reads s of
                  [(x,"")] -> [(Dictionary [x],"")]
                  [(x,"\n")] -> [(Dictionary [x],"")]
                  [(x,'\n':_s)] -> case reads _s of
                      [(Dictionary xs,_)] -> [(Dictionary ([x] <> xs ),"")]
                      _ -> []
                  _ -> []

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