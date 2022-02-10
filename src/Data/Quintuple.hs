module Data.Quintuple where

import Control.Applicative (Applicative((<*>), pure))
import Data.Eq (Eq)
import Data.Foldable (Foldable(foldMap))
import Data.Function.Memoize (deriveMemoizable)
import Data.Functor (Functor)
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Wrapper (Wrapper)
import Text.Read (Read)
import Text.Show (Show)


newtype Quintuple t = Quintuple (t, t, t, t, t)
  deriving (Functor, Eq, Ord, Show, Read)

instance Wrapper (Quintuple t) (t, t, t, t, t)

instance Foldable Quintuple where
  foldMap f (Quintuple (q1, q2, q3, q4, q5)) =
    f q1 <> f q2 <> f q3 <> f q4 <> f q5

instance Applicative Quintuple where
  pure a = Quintuple (a, a, a, a, a)
  Quintuple (f1, f2, f3, f4, f5) <*> Quintuple (a1, a2, a3, a4, a5) =
    Quintuple (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5)


deriveMemoizable ''Quintuple
