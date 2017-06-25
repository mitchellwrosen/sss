module Validation where

import Import

data Validation a b
  = Errors a
  | Success b
  deriving Functor

instance Semigroup a => Applicative (Validation a) where
  pure = Success

  Errors x  <*> Errors y = Errors (x ++ y)
  Errors x  <*> _        = Errors x
  _         <*> Errors y = Errors y
  Success x <*> Success y = Success (x y)
