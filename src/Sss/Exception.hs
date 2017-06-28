module Sss.Exception
  ( SssException(..)
  ) where

import Import

import Control.Exception

import qualified Crypto.SecretSharing.Internal as SSS (prime)

data SssException
  = TooManyShares
  | RequireTooFewShares Word16
  | RequireTooManyShares Word16 Word16
  | TooFewShares Word16 Word16
  | InconsistentShares
  | MalformedShares (NonEmpty Text)
  | MalformedKey
  | EncodingError
  | StderrIsTTY
  deriving (Show, Typeable)

instance Exception SssException where
  displayException = \case
    TooManyShares ->
      "A secret can be broken into at most " ++ show (SSS.prime - 1) ++
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

    StderrIsTTY ->
      "Standard error is connected to a terminal; please redirect it to a file."
