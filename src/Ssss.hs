{-# language LambdaCase          #-}
{-# language ScopedTypeVariables #-}

module Ssss where

import Import

import Control.Exception
  (AssertionFailed, Exception(..), catch, evaluate, throwIO)
import Data.Char (chr, ord)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable (Typeable)

import qualified Crypto.SecretSharing as SSSS
import qualified Crypto.SecretSharing.Internal as SSSS
import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)

type Secret
  = LByteString

data Share = Share
  { shareId        :: Int
  , shareThreshold :: Int
  , shareVals      :: NonEmpty Int
  } deriving (Eq, Show)

data SsssException
  = TooManyShares
  | RequireTooFewShares Int
  | RequireTooManyShares Int Int
  | TooFewShares Int Int
  | DecodingError
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

    DecodingError ->
      "An unexpected error occurred while decoding a share."

    EncodingError ->
      "An unexpected error occurred while encoding a share."


encode :: Int -> Int -> Secret -> IO (NonEmpty Share)
encode 0 n _ = throwIO (RequireTooFewShares n)
encode _ n _ | n >= SSSS.prime = throwIO TooManyShares
encode m n _ | m > n = throwIO (RequireTooManyShares m n)
encode m n bytes = do
  shares <- SSSS.encode m n bytes
  case traverse toShare shares of
    Nothing -> throwIO EncodingError
    Just [] -> error "encode: empty list of shares"
    Just (s:ss) -> pure (s:|ss)

decode :: NonEmpty ByteString -> IO Secret
decode shares =
  case traverse decodeShare shares of
    Nothing -> throwIO DecodingError
    Just (s:|ss) ->
      catch
        (evaluate (SSSS.decode (map fromShare (s:ss))))
        (\(_ :: AssertionFailed) ->
          throwIO (TooFewShares (length ss + 1) (shareThreshold s)))

encodeShare :: Share -> ByteString
encodeShare (Share m n vs) =
  Base64.encode (Text.encodeUtf8
    (Text.pack (chr m : chr n : map chr (NonEmpty.toList vs))))

decodeShare :: ByteString -> Maybe Share
decodeShare share = do
  Right share' <- pure (Base64.decode share)
  c1:c2:c3:cs  <- pure (Text.unpack (Text.decodeUtf8 share'))
  pure (Share (ord c1) (ord c2) (ord <$> (c3:|cs)))

toShare :: SSSS.Share -> Maybe Share
toShare share = do
  s:ss <- pure (SSSS.theShare share)
  pure (Share (SSSS.shareId s) (SSSS.reconstructionThreshold s)
    (SSSS.shareValue <$> (s:|ss)))

fromShare :: Share -> SSSS.Share
fromShare (Share m n vs) =
  SSSS.Share (map (SSSS.ByteShare m n) (NonEmpty.toList vs))
