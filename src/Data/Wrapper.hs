module Data.Wrapper where

import Data.Coerce (coerce, Coercible)


class Wrapper a b | a -> b , b -> a where
  unwrap :: a -> b
  default unwrap :: Coercible a b => a -> b
  unwrap = coerce
  wrap :: b -> a
  default wrap :: Coercible b a => b -> a
  wrap = coerce
