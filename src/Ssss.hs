module Ssss where

import Import
import Validation

import Control.Exception (Exception(..), evaluate, throwIO)
import Data.Bits
import Data.Char (chr, ord)
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable (Typeable)

import qualified Crypto.Hash.SHA256 as SHA
import qualified Crypto.SecretSharing as SSSS
import qualified Crypto.SecretSharing.Internal as SSSS
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)

type Secret
  = LByteString

data Share = Share
  { shareDigest    :: ByteString
  , shareId        :: Word16
  , shareThreshold :: Word16
  , shareVals      :: NonEmpty Int
  } deriving (Eq, Show)

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
      unlines ("The following share(s) are malformed:" : map Text.unpack ss)

    MalformedKey ->
      "The key you provided is malformed."

    EncodingError ->
      "An unexpected error occurred while encoding a share."

encode :: Word16 -> Word16 -> Secret -> IO (NonEmpty Share)
encode 0 n _ = throwIO (RequireTooFewShares n)
encode _ n _ | n >= fromIntegral SSSS.prime = throwIO TooManyShares
encode m n _ | m > n = throwIO (RequireTooManyShares m n)
encode m n bytes = do
  shares <- SSSS.encode (fromIntegral m) (fromIntegral n) bytes
  case traverse (toShare (SHA.hashlazy bytes)) shares of
    Nothing -> throwIO EncodingError
    Just [] -> error "encode: empty list" -- impossible
    Just (s:ss) -> pure (s:|ss)

decode :: NonEmpty Text -> IO Secret
decode (toList -> shares) =
  case traverse decodeShare shares of
    Errors shares' -> throwIO (MalformedShares shares')
    Success (nubOn shareId -> ss@((shareThreshold -> n):_)) -> do
      let m = genericLength ss

      when (m < n) (throwIO (TooFewShares m n))

      when (length (group (map shareThreshold ss)) > 1)
        (throwIO InconsistentShares)

      secret <- evaluate (SSSS.decode (map fromShare ss))

      let digest :: ByteString
          digest = SHA.hashlazy secret

          validate :: (Int, Share) -> Maybe Text
          validate (i, s) = do
            guard (salt (shareId s) digest /= shareDigest s)
            pure (shares !! i)

      case mapMaybe validate (zip [0..] ss) of
        [] -> pure secret
        x:xs -> throwIO (MalformedShares (x:|xs))

encodeShare :: Share -> ByteString
encodeShare (Share d (fromIntegral -> m) (fromIntegral -> n) vs) =
  Base64.encode (d ++ UTF8.fromString (chr m : chr n : map chr (toList vs)))

decodeShare :: Text -> Validation (NonEmpty Text) Share
decodeShare share =
  maybe (Errors (pure share)) Success $ do
    Right share' <- pure (Base64.decode (Text.encodeUtf8 share))
    let (d, ds) = ByteString.splitAt 32 share'
    c1:c2:c3:cs <- pure (UTF8.toString ds)
    pure $ Share
      { shareDigest    = d
      , shareId        = fromIntegral (ord c1)
      , shareThreshold = fromIntegral (ord c2)
      , shareVals      = map ord (c3:|cs)
      }

toShare :: ByteString -> SSSS.Share -> Maybe Share
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

fromShare :: Share -> SSSS.Share
fromShare (Share _ (fromIntegral -> m) (fromIntegral -> n) (toList -> vs)) =
  SSSS.Share (map (SSSS.ByteShare m n) vs)

salt :: Word16 -> ByteString -> ByteString
salt (split16 -> (x, y)) bytes = SHA.hash (ByteString.pack [x, y] ++ bytes)

split16 :: Word16 -> (Word8, Word8)
split16 w = (fromIntegral (w `shiftR` 8), fromIntegral w)
