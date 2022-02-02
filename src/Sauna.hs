
module Sauna where
import Prelude hiding (Word)

import Sauna.Data
import Sauna.Data.Quintuple
import Data.List (union, (\\), intersect)
import Data.Foldable (toList)
import Data.FileEmbed (embedFile)
import Data.ByteString.UTF8 (toString)
import Control.Applicative (liftA3, liftA2)
import Control.Monad (filterM)
import Data.Coerce (coerce)


data State = State
 { gray :: [Letter]
 , yellow :: [Letter]
 , green :: Quintuple (Maybe Letter)
 , solution :: Quintuple [Letter]
 }

initialState = State
 { gray = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,Aumlaut,Oumlaut]
 , yellow = []
 , green = Quintuple (Nothing, Nothing, Nothing, Nothing, Nothing)
 , solution = pure [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,Aumlaut,Oumlaut]
 }


dictionary :: Dictionary
dictionary = read $ toString $(embedFile "dict.txt")


update :: State -> Word -> Response -> State
update
  State {..}
  (Word word)
  (Response response)
  = State
  { gray =  gray \\ toList word
  , yellow = yellow `union` foldMap (\case
        (x, Yellow) -> [x]
        _ -> []
     ) (zip (toList word) (toList response))
  , green = fmap (\case
        ( _, _,Just letter) -> Just letter
        (letter, Green, Nothing) -> Just letter
        _ -> Nothing
     ) (liftA3 (,,) word response green)
  , solution = fmap (\case
       (_,_,[l]) -> [l]
       (l,Green,_) -> [l]
       (l,Yellow,d) -> filter (\l' -> l' `notElem` foldMap (\case
           (l'', Black) -> [l'']
           (_, _) -> []
         ) (liftA2 (,) word response)) (d \\ [l])
       (_,Black,d) -> filter (\l' -> l' `notElem` foldMap (\case
           (l'', Black) -> [l'']
           (_, _) -> []
         ) (liftA2 (,) word response)) d
       _ -> []
     ) (liftA3 (,,) word response solution)
  }

next :: State -> Word
next
 State {..}
 = head (grayDictionary <> solutionDictionary)
 where
   grayDictionary = filter (
        \(Word w) -> (5 ==) $ length $ intersect gray $ toList w
     ) $ coerce dictionary
   solutionDictionary = filter (
        \(Word w)
          -> all (\(d,l) -> l `elem` d) (liftA2 (,) solution w)
          && all (`elem` w) yellow
     ) $ coerce dictionary