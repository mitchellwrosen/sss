module Data.Validation
  ( Validation(..)
  ) where

import Prelude
import Data.Semigroup (Semigroup, (<>))

data Validation a b
  = Errors a
  | Success b
  deriving (Eq, Functor, Show)

instance Semigroup a => Applicative (Validation a) where
  pure = Success

  Errors x  <*> Errors y = Errors (x <> y)
  Errors x  <*> _        = Errors x
  _         <*> Errors y = Errors y
  Success x <*> Success y = Success (x y)
