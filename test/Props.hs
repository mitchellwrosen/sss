{-# language TemplateHaskell #-}

module Main where

import Import
import Sss
import Sss.Decode (decodeShare, fromShare)
import Sss.Encode (toShare)
import Sss.Utils (salt)

import Data.Validation (Validation(Success))
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Monadic

import qualified Crypto.SecretSharing.Internal as SSS
import qualified Data.ByteString as ByteString
import qualified Data.ByteString64 as ByteString64
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as LText (pack)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)

prime :: Word16
prime = fromIntegral SSS.prime

arbitraryShareNum :: Gen Word16
arbitraryShareNum =
  arbitrarySizedNatural `suchThat` (\n -> n > 0 && n < prime)

arbitraryShare :: Gen (ByteString, Share)
arbitraryShare = do
  digest <- ByteString.pack <$> vectorOf 32 arbitrary
  id     <- arbitraryShareNum
  thresh <- arbitraryShareNum
  vals   <- NonEmpty.fromList <$> listOf1 arbitrarySizedNatural

  let share = Share
        { shareDigest    = salt id digest
        , shareId        = id
        , shareThreshold = thresh
        , shareVals      = vals
        }

  pure (digest, share)

arbitrarySecret :: Gen Secret
arbitrarySecret = LText.encodeUtf8 . LText.pack <$> listOf1 arbitrary

prop_encodeDecodeShare :: Property
prop_encodeDecodeShare =
  forAll arbitraryShare
    (\(_, share) ->
      decodeShare (ByteString64.asText (encodeShare share)) === Success share)

prop_toFromShare :: Property
prop_toFromShare =
  forAll arbitraryShare
    (\(digest, share) -> toShare digest (fromShare share) === Just share)

prop_encodeNumShares :: Property
prop_encodeNumShares = monadicIO $ do
  m <- pick arbitraryShareNum
  n <- pick (choose (m, prime - 1))
  bytes  <- pick arbitrarySecret
  shares <- run (encode m n bytes)
  assert (length shares == fromIntegral n)

return []
main :: IO ()
main = do
  _ <- $(forAllProperties) (quickCheckWithResult stdArgs { maxSuccess = 500 })
  pure ()
