module Sauna where
import Prelude hiding (Word, words, init)

import Sauna.Data.Alphabet
import Sauna.Data.Response
import Sauna.Data.Color
import Sauna.Data.State
import Sauna.Data.Letter
import Sauna.Data.Word
import Data.Quintuple
import Data.List (union, (\\), intersect, nub, sort, sortBy)
import Data.Foldable (toList, maximumBy)
import Data.FileEmbed (embedFile)
import Data.ByteString.UTF8 (toString)
import Control.Applicative (liftA3, liftA2)
import Data.Wrapper
import Sauna.Data.Dictionary
import Data.Monoid (Sum(Sum), getSum)


------------
---- Filters
------------
--
--type WordFilter = Word -> Bool
--
---- | Filters Words contains all the Letters.
--presentFilter :: [Letter] -> WordFilter
--presentFilter alphabet word = coverage alphabet $ toList $ unwrap word
--  where
--    coverage :: [Letter] -> [Letter] -> Bool
--    coverage (l:ls) ws = (l `elem` ws) && coverage ls (ws\\[l])
--    coverage [] _ = True
--
---- | Filter Words matching options.
--optionsFilter :: Quintuple [Letter] -> WordFilter
--optionsFilter options word = all (uncurry elem) (liftA2 (,) (unwrap word) options)
--
---- | Filters possible solutions.
--solutionFilter :: State -> WordFilter
--solutionFilter State{..} word = presentFilter present  word && optionsFilter options word
--
---- | Filter words over given alphabet.
--alphabetFilter :: [Letter] -> WordFilter
--alphabetFilter alphabet word = word' == word' `intersect` alphabet
--  where
--    word' = toList $ unwrap word
--
---- | Filters words containing n-diferent letters.
--lengthFilter :: Int -> WordFilter
--lengthFilter n word = n == length (nub word')
--  where
--    word' = toList $ unwrap word
--
--
----------
---- Utils
----------
--
---- | Creates minimal alphabet covering given dictionary.
--dictionaryAlphabet :: Dictionary -> [Letter]
--dictionaryAlphabet (Dictionary (word:words)) = nub $ union word' (dictionaryAlphabet (wrap words))
--  where
--    word' = toList $ unwrap word
--dictionaryAlphabet (Dictionary []) = []
--
--------
---- API
--------
--
--init :: State
--init = State options' present' unused'
--  where
--    options' = pure [A ..]
--    present' = []
--    unused' = [A ..]
--
--next :: State -> Word
--next state@State{unused} = let
--    solutionDictionary = wrap $ filter (solutionFilter state) $ unwrap dictionary
--    alphabet = dictionaryAlphabet solutionDictionary
--    coverageDictionary = wrap $ filter (alphabetFilter (alphabet `intersect` unused)) $ unwrap dictionary
--    coverageDictionary5 = wrap $ filter (lengthFilter 5) $ unwrap coverageDictionary
--    coverageDictionary4 = wrap $ filter (lengthFilter 4) $ unwrap coverageDictionary
--    coverageDictionary3 = wrap $ filter (lengthFilter 3) $ unwrap coverageDictionary
--  in head $ unwrap coverageDictionary5 <> unwrap coverageDictionary4 <> unwrap coverageDictionary3 <> unwrap solutionDictionary
--
--update :: State -> Word -> Response -> State
--update
-- State {..}
-- word
-- response
-- = State options' present' unused'
-- where
--   options' :: Quintuple [Letter]
--   options' = fmap ( \case
--      (letter, Green, _) -> [letter]
--      (_, _, [letter]) -> [letter]
--      (letter, _, option) -> (option \\ ((blacks) \\ yellows)) \\ [letter]
--     ) (liftA3 (,,) (unwrap word) (unwrap response) options)
--   present' = present <> (present'' \\ present)
--   present'' = foldMap ( \case
--      (letter, Green) -> [letter]
--      (letter, Yellow) -> [letter]
--      (_,_) -> []
--    ) (liftA2 (,) (unwrap word) (unwrap response))
--   unused' = unused \\ toList (unwrap word)
--   letters color = foldMap ( \(letter, color') -> [letter | color == color'] ) (liftA2 (,) (unwrap word) (unwrap response))
--   greens = letters Green
--   blacks = letters Black
--   yellows = letters Yellow


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
        kernel' opt letter _ =  wrap $ (unwrap opt \\ (unwrap blacks \\ unwrap yellows)) \\ [letter]


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

init :: State
init = wrap []

next :: State -> Word
next state =
  if length solutionDictionary > 1 then
    if overlapScore state maxOverlap == 0 then
       head solutionDictionary
    else
      maxOverlap
  else
    head solutionDictionary
  where
    maxOverlap = maximumBy (overlapOrdering state) (unwrap fullDictionary)
    solutionDictionary = filter (solutionFilter state) (unwrap fullDictionary)

update :: State -> Word -> Response -> State
update state word response = wrap (unwrap state <> [(word,response)])