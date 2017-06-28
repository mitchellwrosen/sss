module Sss.Types
  ( Secret
  , SecretDigest
  , Share(..)
  , ShareId
  , ShareDigest
  , EncodedShare
  ) where

import Import

-- | The secret (binary).
type Secret
  = LByteString

-- | SHA256 of the secret (binary).
type SecretDigest
  = ByteString

data Share = Share
  { shareDigest    :: ShareDigest
  , shareId        :: ShareId
  , shareThreshold :: Word16
  , shareVals      :: NonEmpty Int
  } deriving (Eq, Show)

type ShareId
  = Word16

-- | SHA256 of the share ID + secret digest, used as a MAC (binary).
type ShareDigest
  = ByteString

-- | Base64-encoded share.
type EncodedShare
  = ByteString64
