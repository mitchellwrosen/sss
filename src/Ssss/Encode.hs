module Ssss.Encode
  ( encode
  , encodeShare
    -- ** Internal
  , toShare
  ) where

import Import

import Ssss.Exception
import Ssss.Types
import Ssss.Utils (salt)

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Crypto.Hash.SHA256 as SHA
import qualified Crypto.SecretSharing as SSSS
import qualified Crypto.SecretSharing.Internal as SSSS
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString64 as ByteString64

encode :: Word16 -> Word16 -> Secret -> IO [Share]
encode 0 n _ = throw (RequireTooFewShares n)
encode _ n _ | n >= fromIntegral SSSS.prime = throw TooManyShares
encode m n _ | m > n = throw (RequireTooManyShares m n)
encode m n bytes = do
  shares <- SSSS.encode (fromIntegral m) (fromIntegral n) bytes
  maybe (throw EncodingError) pure
    (traverse (toShare (SHA.hashlazy bytes)) shares)

encodeShare :: Share -> EncodedShare
encodeShare (Share d (fromIntegral -> m) (fromIntegral -> n) vs) =
  ByteString64.encode
    (d ++ UTF8.fromString (chr m : chr n : map chr (toList vs)))

toShare :: SecretDigest -> SSSS.Share -> Maybe Share
toShare digest share = do
  s:ss <- pure (SSSS.theShare share)

  let id :: Word16
      id = fromIntegral (SSSS.shareId s)

  pure $ Share
    { shareDigest    = salt id digest
    , shareId        = id
    , shareThreshold = fromIntegral (SSSS.reconstructionThreshold s)
    , shareVals      = map SSSS.shareValue (s:|ss)
    }
