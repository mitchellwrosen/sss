module Import
  ( LByteString
  , (++)
  , map
  , nubOn
  , module X
  ) where

import Data.ByteString as X (ByteString)
import Control.Applicative as X
import Control.Monad as X
import Data.List as X (group)
import Data.Maybe as X
import Data.Semigroup as X (Semigroup)
import Data.Word as X
import GHC.Exts as X (toList)
import Prelude as X hiding ((++), id, map)

import Data.Function
import Data.List (nubBy)
import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy

type LByteString
  = Data.ByteString.Lazy.ByteString

(++) :: Semigroup a => a -> a -> a
(++) = (<>)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)
