module Ssss.Exception
  ( SsssException(..)
  ) where

import Import

import Ssss.Types

import Control.Exception

import qualified Crypto.SecretSharing.Internal as SSSS (prime)

data SsssException
  = TooManyShares
  | RequireTooFewShares Word16
  | RequireTooManyShares Word16 Word16
  | TooFewShares Word16 Word16
  | InconsistentShares
  | MalformedShares (NonEmpty Text)
  | MalformedKey
  | EncodingError
  deriving (Show, Typeable)

instance Exception SsssException where
  displayException = \case
    TooManyShares ->
      "A secret can be broken into at most " ++ show (SSSS.prime - 1) ++
        " shares."

    RequireTooFewShares n ->
      "You cannot require 0 of " ++ show n ++ " shares to reconstruct a secret."

    RequireTooManyShares m n ->
      "You cannot require " ++ show m ++ " of " ++ show n ++
        " shares to reconstruct a secret."

    TooFewShares m n ->
      "You provided only " ++ show m ++ " of " ++ show n ++ " shares."

    InconsistentShares ->
      "The shares you provided are not all of the same secret."

    MalformedShares (toList -> ss) ->
      unlines ("The following share(s) are malformed:" : map unpack ss)

    MalformedKey ->
      "The key you provided is malformed."

    EncodingError ->
      "An unexpected error occurred while encoding a share."

