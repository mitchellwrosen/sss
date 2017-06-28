module Sss.Encode
  ( encode
  , encodeShare
    -- ** Internal
  , toShare
  ) where

import Import

import Sss.Exception
import Sss.Types
import Sss.Utils (salt)

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Crypto.Hash.SHA256 as SHA
import qualified Crypto.SecretSharing as SSS
import qualified Crypto.SecretSharing.Internal as SSS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString64 as ByteString64

encode :: Word16 -> Word16 -> Secret -> IO [Share]
encode 0 n _ = throw (RequireTooFewShares n)
encode _ n _ | n >= fromIntegral SSS.prime = throw TooManyShares
encode m n _ | m > n = throw (RequireTooManyShares m n)
encode m n bytes = do
  shares <- SSS.encode (fromIntegral m) (fromIntegral n) bytes
  maybe (throw EncodingError) pure
    (traverse (toShare (SHA.hashlazy bytes)) shares)

encodeShare :: Share -> EncodedShare
encodeShare (Share d (fromIntegral -> m) (fromIntegral -> n) vs) =
  ByteString64.encode
    (d ++ UTF8.fromString (chr m : chr n : map chr (toList vs)))

toShare :: SecretDigest -> SSS.Share -> Maybe Share
toShare digest share = do
  s:ss <- pure (SSS.theShare share)

  let id :: Word16
      id = fromIntegral (SSS.shareId s)

  pure $ Share
    { shareDigest    = salt id digest
    , shareId        = id
    , shareThreshold = fromIntegral (SSS.reconstructionThreshold s)
    , shareVals      = map SSS.shareValue (s:|ss)
    }
