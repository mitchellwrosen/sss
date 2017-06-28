module Sss.Utils
  ( salt
  ) where

import Import

import Sss.Types

import Data.Bits (shiftR)

import qualified Crypto.Hash.SHA256 as SHA

salt :: ShareId -> SecretDigest -> ShareDigest
salt id bytes = SHA.hash (pack [x, y] ++ bytes)
 where
  x, y :: Word8
  x = fromIntegral (id `shiftR` 8)
  y = fromIntegral id
