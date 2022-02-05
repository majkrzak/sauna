module Sauna where
import Prelude hiding (Word, words, init)

import Sauna.Data.Alphabet
import Sauna.Data.Response
import Sauna.Data.Color
import Sauna.Data.State
import Sauna.Data.Letter
import Sauna.Data.Word
import Data.Quintuple
import Data.List (union, (\\), intersect, nub, sort)
import Data.Foldable (toList)
import Data.FileEmbed (embedFile)
import Data.ByteString.UTF8 (toString)
import Control.Applicative (liftA3, liftA2)
import Data.Wrapper
import Sauna.Data.Dictionary

--data State = State
-- { gray :: [Letter]
-- , yellow :: [Letter]
-- , green :: Quintuple (Maybe Letter)
-- , solution :: Quintuple [Letter]
-- }
--
--initialState = State
-- { gray = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,Aumlaut,Oumlaut]
-- , yellow = []
-- , green = Quintuple (Nothing, Nothing, Nothing, Nothing, Nothing)
-- , solution = pure [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,Aumlaut,Oumlaut]
-- }
--
--

--
--
--update :: State -> Word -> Response -> State
--update
--  State {..}
--  (Word word)
--  (Response response)
--  = State
--  { gray =  gray \\ toList word
--  , yellow = yellow `union` foldMap (\case
--        (x, Yellow) -> [x]
--        _ -> []
--     ) (zip (toList word) (toList response))
--  , green = fmap (\case
--        ( _, _,Just letter) -> Just letter
--        (letter, Green, Nothing) -> Just letter
--        _ -> Nothing
--     ) (liftA3 (,,) word response green)
--  , solution = fmap (\case
--       (_,_,[l]) -> [l]
--       (l,Green,_) -> [l]
--       (l,Yellow,d) -> filter (\l' -> l' `notElem` foldMap (\case
--           (l'', Black) -> [l'']
--           (_, _) -> []
--         ) (liftA2 (,) word response)) (d \\ [l])
--       (_,Black,d) -> filter (\l' -> l' `notElem` foldMap (\case
--           (l'', Black) -> [l'']
--           (_, _) -> []
--         ) (liftA2 (,) word response)) d
--       _ -> []
--     ) (liftA3 (,,) word response solution)
--  }
--
--next :: State -> Word
--next
-- State {..}
-- = head (grayDictionary <> solutionDictionary)
-- where
--   grayDictionary = filter (
--        \(Word w) -> (5 ==) $ length $ intersect gray $ toList w
--     ) $ coerce dictionary
--   solutionDictionary = filter (
--        \(Word w)
--          -> all (\(d,l) -> l `elem` d) (liftA2 (,) solution w)
--          && all (`elem` w) yellow
--     )$ coerce dictionary

--
--dictionary :: Dictionary
--dictionary = read $ toString $(embedFile "dict.txt")
--
--
--
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
