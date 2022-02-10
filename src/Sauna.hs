module Sauna where
import Prelude hiding (Word, words)

import Control.Applicative (liftA2, liftA3)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.Foldable (minimumBy, toList)
import Data.Function.Memoize (memoize)
import Data.List ((\\), inits, nub, union)
import Data.Quintuple
import Data.Wrapper
import Sauna.Data.Alphabet
import Sauna.Data.Color
import Sauna.Data.Dictionary
import Sauna.Data.Letter
import Sauna.Data.Response
import Sauna.Data.State
import Sauna.Data.Word
import Sauna.Preprocessed (preprocessed)


fullAlphabet :: Alphabet
fullAlphabet = wrap [A ..]

fullDictionary :: Dictionary
fullDictionary = read $ toString $(embedFile "dict.txt")

-- | Get Alphabet of unused Letters for given State.
unused :: State -> Alphabet
unused =
  wrap
    . (unwrap fullAlphabet \\)
    . foldl union []
    . fmap (toList . unwrap . fst)
    . unwrap

-- | Get Alphabet of letters present in the final solution for given State.
-- Note some letter may occur multiple times!
-- TODO: simplify
present :: State -> Alphabet
present = foldl kernel (wrap []) . unwrap
 where
  kernel :: Alphabet -> (Word, Response) -> Alphabet
  kernel (Alphabet a) (word, response) = Alphabet $ a <> (a' \\ a)
   where
    a' = foldMap
      (\case
        (letter, Green ) -> [letter]
        (letter, Yellow) -> [letter]
        (_     , _     ) -> []
      )
      (liftA2 (,) (unwrap word) (unwrap response))

-- | Get Alphabets of possible letters for each position.
-- TODO: simplify
options :: State -> Quintuple Alphabet
options state = foldl kernel (pure fullAlphabet) (unwrap state)
 where
  kernel :: Quintuple Alphabet -> (Word, Response) -> Quintuple Alphabet
  kernel opts (word, response) =
    kernel' <$> opts <*> unwrap word <*> unwrap response
   where
    letters :: Color -> Alphabet
    letters color = wrap $ foldMap
      (\(letter, color') -> [ letter | color == color' ])
      (liftA2 (,) (unwrap word) (unwrap response))
    blacks  = letters Black
    yellows = letters Yellow
    kernel' :: Alphabet -> Letter -> Color -> Alphabet
    kernel' _                  letter Green = wrap [letter]
    kernel' opt@(Alphabet [_]) _      _     = opt
    kernel' opt letter _ =
      wrap $ (unwrap opt \\ (nub (unwrap blacks) \\ unwrap yellows)) \\ [letter]

-- | Type for filtering dictionaries.
type WordFilter = Word -> Bool

-- | Filters possible solutions.
-- TODO: simplify
solutionFilter :: State -> WordFilter
solutionFilter state word' =
  presentFilter (present state) word' && optionsFilter (options state) word'
 where
  presentFilter :: Alphabet -> WordFilter
  presentFilter alphabet word = coverage (unwrap alphabet) $ toList $ unwrap
    word
   where
    coverage :: [Letter] -> [Letter] -> Bool
    coverage (l : ls) ws = (l `elem` ws) && coverage ls (ws \\ [l])
    coverage []       _  = True
  optionsFilter :: Quintuple Alphabet -> WordFilter
  optionsFilter opts word =
    all (uncurry elem) (liftA2 (,) (unwrap word) (unwrap <$> opts))

-- | Dictionary of valid solutions for given State.
solutionDictionary :: State -> Dictionary
solutionDictionary = memoize $ \case
  State [] -> fullDictionary
  state ->
    wrap $ filter (solutionFilter state) $ unwrap $ solutionDictionary $ prev
      state

-- | Type for ordering dictionaries.
type WordOrdering = Word -> Word -> Ordering

-- | Orders Words by count of worst case solutions.
eliminationOrdering :: State -> WordOrdering
eliminationOrdering state a b =
  compare (eliminationScore state a) (eliminationScore state b)

-- | Counts solutions in worst case scenario.
eliminationScore :: State -> Word -> Int
eliminationScore state word = maximum
  [ length (unwrap (solutionDictionary (next state (word, response))))
  | response <-
    [ Response (Quintuple (x1, x2, x3, x4, x5))
    | x1 <- [Black ..]
    , x2 <- [Black ..]
    , x3 <- [Black ..]
    , x4 <- [Black ..]
    , x5 <- [Black ..]
    ]
  ]

initialize :: State
initialize = wrap []

guess :: State -> Word
guess = memoize $ \state -> if length (unwrap (solutionDictionary state)) > 1
  then case preprocessed state of
    Nothing   -> minimumBy (eliminationOrdering state) (unwrap fullDictionary)
    Just word -> word
  else head (unwrap (solutionDictionary state))


prev :: State -> State
prev = wrap . init . unwrap

next :: State -> (Word, Response) -> State
next state (word, response) = wrap (unwrap state <> [(word, response)])

-- | Checks guess against the solution.
-- TODO: do in the sane way
check :: Word -> Word -> Response
check (Word (Quintuple (a1, a2, a3, a4, a5))) (Word (Quintuple (g1, g2, g3, g4, g5)))
  = Response $ Quintuple (r1, r2, r3, r4, r5)
 where
  r1
    | a1 == g1
    = Green
    | a1 `notElem` [a1, a2, a3, a4, a5]
    = Black
    | length (filter (== g1) [a1, a2, a3, a4, a5])
      > length (filter (== g1) [])
      + length
          (filter (== (g1, Green)) [(g2, r2), (g3, r3), (g4, r4), (g5, r5)])
    = Yellow
    | otherwise
    = Black
  r2
    | a2 == g2
    = Green
    | a2 `notElem` [a1, a2, a3, a4, a5]
    = Black
    | length (filter (== g2) [a1, a2, a3, a4, a5])
      > length (filter (== g2) [g1])
      + length (filter (== (g2, Green)) [(g3, r3), (g4, r4), (g5, r5)])
    = Yellow
    | otherwise
    = Black
  r3
    | a3 == g3
    = Green
    | a3 `notElem` [a1, a2, a3, a4, a5]
    = Black
    | length (filter (== g3) [a1, a2, a3, a4, a5])
      > length (filter (== g3) [g1, g2])
      + length (filter (== (g3, Green)) [(g4, r4), (g5, r5)])
    = Yellow
    | otherwise
    = Black
  r4
    | a4 == g4
    = Green
    | a4 `notElem` [a1, a2, a3, a4, a5]
    = Black
    | length (filter (== g4) [a1, a2, a3, a4, a5])
      > length (filter (== g4) [g1, g2, g3])
      + length (filter (== (g4, Green)) [(g5, r5)])
    = Yellow
    | otherwise
    = Black
  r5
    | a5 == g5
    = Green
    | a5 `notElem` [a1, a2, a3, a4, a5]
    = Black
    | length (filter (== g5) [a1, a2, a3, a4, a5])
      > length (filter (== g5) [g1, g2, g3, g4])
      + length (filter (== (g5, Green)) [])
    = Yellow
    | otherwise
    = Black
