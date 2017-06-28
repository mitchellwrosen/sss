module Sss.Decode
  ( decode
    -- ** Internal
  , decodeShare
  , fromShare
  ) where

import Import

import Sss.Exception
import Sss.Types
import Sss.Utils (salt)

import Data.Char (ord)
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation

import qualified Crypto.Hash.SHA256 as SHA
import qualified Crypto.SecretSharing as SSSS
import qualified Crypto.SecretSharing.Internal as SSSS
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text.Encoding as Text (encodeUtf8)

decode :: NonEmpty Text -> IO Secret
decode (toList -> shares) =
  case traverse decodeShare shares of
    Errors shares' -> throw (MalformedShares shares')
    Success (nubOn shareId -> ss@((shareThreshold -> n):_)) -> do
      let m = genericLength ss

      when (m < n) (throw (TooFewShares m n))

      when (length (group (map shareThreshold ss)) > 1)
        (throw InconsistentShares)

      secret <- evaluate (SSSS.decode (map fromShare ss))

      let digest :: ByteString
          digest = SHA.hashlazy secret

          validate :: (Int, Share) -> Maybe Text
          validate (i, s) = do
            guard (salt (shareId s) digest /= shareDigest s)
            pure (shares !! i)

      case mapMaybe validate (zip [0..] ss) of
        [] -> pure secret
        x:xs -> throw (MalformedShares (x:|xs))

decodeShare :: Text -> Validation (NonEmpty Text) Share
decodeShare share =
  maybe (Errors (pure share)) Success $ do
    share' <- hush (Base64.decode (Text.encodeUtf8 share))
    let (d, ds) = ByteString.splitAt 32 share'
    c1:c2:c3:cs <- pure (UTF8.toString ds)
    pure $ Share
      { shareDigest    = d
      , shareId        = fromIntegral (ord c1)
      , shareThreshold = fromIntegral (ord c2)
      , shareVals      = map ord (c3:|cs)
      }

fromShare :: Share -> SSSS.Share
fromShare (Share _ (fromIntegral -> m) (fromIntegral -> n) (toList -> vs)) =
  SSSS.Share (map (SSSS.ByteShare m n) vs)
