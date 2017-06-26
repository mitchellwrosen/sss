module Import
  ( LByteString
  , (++)
  , hush
  , map
  , nubOn
  , throw
  , module X
  ) where

import BytesClasses as X

import Data.ByteString as X (ByteString)
import Data.ByteString64 as X (ByteString64)
import Control.Applicative as X
import Control.Exception as X (Exception, SomeException, evaluate)
import Control.Monad as X
import Data.List as X (group)
import Data.List.NonEmpty as X (NonEmpty)
import Data.Maybe as X
import Data.Semigroup as X (Semigroup)
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)
import Data.Word as X
import GHC.Exts as X (toList)
import Prelude as X hiding ((++), getContents, id, map, putStrLn, readFile)
import System.IO as X (Handle)

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Function
import Data.List (nubBy)
import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy

type LByteString
  = Data.ByteString.Lazy.ByteString

(++) :: Semigroup a => a -> a -> a
(++) = (<>)

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

throw :: MonadThrow m => Exception e => e -> m a
throw = throwM
