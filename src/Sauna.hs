module Sauna where
import Prelude hiding (Word, words)

import Sauna.Data.Alphabet
import Sauna.Data.Response
import Sauna.Data.Color
import Sauna.Data.State
import Sauna.Data.Letter
import Sauna.Data.Word
import Data.Quintuple
import Data.List (union, (\\), intersect, nub, sort, sortBy)
import Data.Foldable (toList, maximumBy, minimumBy)
import Data.FileEmbed (embedFile)
import Data.ByteString.UTF8 (toString)
import Control.Applicative (liftA3, liftA2)
import Data.Wrapper
import Sauna.Data.Dictionary
import Data.Monoid (Sum(Sum), getSum)
import Data.Function.Memoize (memoize)
import Sauna.Preprocessed (preprocessed)


fullAlphabet :: Alphabet
fullAlphabet = wrap [A ..]

fullDictionary :: Dictionary
fullDictionary = read $ toString $(embedFile "dict.txt")


-- | Get Alphabet of unused Letters for given State.
unused :: State -> Alphabet
unused = wrap . (unwrap fullAlphabet \\) . foldl union [] . fmap (toList . unwrap . fst) . unwrap

-- | Get Alphabet of letters present in the final solution.
-- Note some letter may occur multiple times!
-- TODO: simplify
present :: State -> Alphabet
present = foldl kernel (wrap []) . unwrap
  where
    kernel :: Alphabet -> (Word, Response) -> Alphabet
    kernel (Alphabet a) (word, response) = Alphabet $ a <> (a' \\ a)
      where
        a' = foldMap ( \case
            (letter, Green) -> [letter]
            (letter, Yellow) -> [letter]
            (_,_) -> []
          ) (liftA2 (,) (unwrap word) (unwrap response))

-- | Get Alphabets of possible letters for each position.
-- TODO: simplify
options :: State -> Quintuple Alphabet
options state = foldl kernel (pure fullAlphabet)  (unwrap state)
  where
    kernel :: Quintuple Alphabet -> (Word, Response) -> Quintuple Alphabet
    kernel opts (word, response) = kernel' <$> opts <*> unwrap word <*> unwrap response
      where
        letters :: Color -> Alphabet
        letters color = wrap $ foldMap ( \(letter, color') -> [letter | color == color'] ) (liftA2 (,) (unwrap word) (unwrap response))
        blacks = letters Black
        yellows = letters Yellow
        kernel' :: Alphabet -> Letter -> Color -> Alphabet
        kernel' _ letter Green = wrap [letter]
        kernel' opt@(Alphabet [_]) _ _ = opt
        kernel' opt letter _ =  wrap $ (unwrap opt \\ (nub (unwrap blacks) \\ unwrap yellows)) \\ [letter]


-- | Type for filtering dictionaries.
type WordFilter = Word -> Bool 

-- | Filters possible solutions.
-- TODO: simplify
solutionFilter :: State -> WordFilter
solutionFilter state word' = presentFilter (present state)  word' && optionsFilter (options state) word'
  where
    presentFilter :: Alphabet -> WordFilter
    presentFilter alphabet word = coverage (unwrap alphabet) $ toList $ unwrap word
      where
        coverage :: [Letter] -> [Letter] -> Bool
        coverage (l:ls) ws = (l `elem` ws) && coverage ls (ws\\[l])
        coverage [] _ = True
    optionsFilter :: Quintuple Alphabet -> WordFilter
    optionsFilter opts word = all (uncurry elem) (liftA2 (,) (unwrap word) (unwrap <$> opts))

-- | Dictionary of valid solutions for given State.
solutionDictionary :: State -> Dictionary
solutionDictionary = memoize $ \case
  State [] -> fullDictionary
  state -> wrap $ filter (solutionFilter state) $ unwrap $ solutionDictionary $ prev state

-- | Type for ordering dictionaries.
type WordOrdering = Word -> Word -> Ordering

-- | Orders Words by count of unused Letters in common with solution.
-- TODO: simplify
overlapOrdering :: State -> WordOrdering
overlapOrdering state a b = compare (overlapScore state a) (overlapScore state b)

-- | Counts unused letters form Word in solution Dictionary.
overlapScore :: State -> Word -> Sum Int
overlapScore state word = foldMap score' (filter (solutionFilter state) (unwrap fullDictionary))
  where
    score' :: Word -> Sum Int
    score' word' = foldMap score'' (unwrap (unused state) `intersect` toList (unwrap word'))
      where
        score'' :: Letter -> Sum Int
        score'' l = Sum $ length (filter (l==) (nub (toList (unwrap word))))


-- | Orders Words by count of worst case solutions.
eliminationOrdering :: State -> WordOrdering
eliminationOrdering state a b =  compare (eliminationScore state a) (eliminationScore state b)

-- | Counts solutions in worst case scenario.
eliminationScore :: State -> Word -> Int
eliminationScore state word = maximum [length (unwrap (solutionDictionary (update state word response))) | response <- [Response (Quintuple (x1,x2,x3,x4,x5)) | x1 <- [Black ..], x2 <- [Black ..],x3 <- [Black ..],x4 <- [Black ..],x5 <- [Black ..]]]

initialize :: State
initialize = wrap []

guess :: State -> Word
guess = memoize $ \state -> 
    if length (unwrap (solutionDictionary state)) > 1 then
      case preprocessed state of
        Nothing -> minimumBy (eliminationOrdering state) (unwrap fullDictionary)
        Just word -> word
    else
      head (unwrap (solutionDictionary state))


prev :: State -> State
prev = wrap . init . unwrap

update :: State -> Word -> Response -> State
update state word response = wrap (unwrap state <> [(word,response)])
