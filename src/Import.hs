module Import
  ( LByteString
  , (++)
  , map
  , nubOn
  , HandleIO(..)
  , Lazy(..)
  , Strict(..)
  , module X
  ) where

import Data.ByteString as X (ByteString)
import Control.Applicative as X
import Control.Exception as X (Exception, SomeException, evaluate)
import Control.Monad as X
import Data.List as X (group)
import Data.Maybe as X
import Data.Semigroup as X (Semigroup)
import Data.Text as X (Text)
import Data.Typeable as X (Typeable)
import Data.Word as X
import GHC.Exts as X (toList)
import Prelude as X hiding ((++), getContents, id, map, readFile)
import System.IO as X (Handle)

import Data.Function
import Data.List (nubBy)
import Data.Semigroup ((<>))

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Data.Text.Lazy

type LByteString
  = Data.ByteString.Lazy.ByteString

(++) :: Semigroup a => a -> a -> a
(++) = (<>)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

class HandleIO a where
  getContents :: IO a
  hPut :: Handle -> a -> IO ()
  readFile :: FilePath -> IO a

instance HandleIO ByteString where
  getContents = Data.ByteString.getContents
  hPut = Data.ByteString.hPut
  readFile = Data.ByteString.readFile

instance HandleIO Data.ByteString.Lazy.ByteString where
  getContents = Data.ByteString.Lazy.getContents
  hPut = Data.ByteString.Lazy.hPut
  readFile = Data.ByteString.Lazy.readFile

instance HandleIO Text where
  getContents = Data.Text.IO.getContents
  hPut = Data.Text.IO.hPutStr
  readFile = Data.Text.IO.readFile

class Lazy a b | a -> b where
  lazy :: a -> b

instance Lazy ByteString Data.ByteString.Lazy.ByteString where
  lazy = Data.ByteString.Lazy.fromStrict

instance Lazy Text Data.Text.Lazy.Text where
  lazy = Data.Text.Lazy.fromStrict

class Strict a b | a -> b where
  strict :: a -> b

instance Strict Data.ByteString.Lazy.ByteString ByteString where
  strict = Data.ByteString.Lazy.toStrict
