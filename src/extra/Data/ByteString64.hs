module Data.ByteString64
  ( ByteString64(..)
  , decode
  , encode
  , asText
  ) where

import BytesClasses

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Prelude

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8

newtype ByteString64
  = ByteString64 ByteString
  deriving Show

un :: ByteString64 -> ByteString
un (ByteString64 x) = x

decode :: ByteString64 -> ByteString
decode bytes =
  case Base64.decode (un bytes) of
    Left _ -> error "Data.ByteString64.decode: Left"
    Right bytes' -> bytes'

encode :: ByteString -> ByteString64
encode = ByteString64 . Base64.encode

asText :: ByteString64 -> Text
asText = decodeUtf8 . un

instance TextIO ByteString64 where
  putStrLn = Char8.putStrLn . un

instance Unpack ByteString64 [Char] where
  unpack = Char8.unpack . un
