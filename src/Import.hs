module Import
  ( LByteString
  , module X
  ) where

import Data.ByteString as X (ByteString)
import Control.Applicative as X
import Prelude as X

import qualified Data.ByteString.Lazy as LByteString

type LByteString
  = LByteString.ByteString
