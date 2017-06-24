{-# language TemplateHaskell #-}

module Main where

import Ssss

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Crypto.SecretSharing.Internal as SSSS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as LText (pack)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)

arbitraryShareThreshold :: Gen Int
arbitraryShareThreshold = arbitrarySizedNatural `suchThat` (> 0)

arbitraryShareNum :: Gen Int
arbitraryShareNum =
  arbitrarySizedNatural `suchThat` (\n -> n > 0 && n < SSSS.prime)

arbitraryShare :: Gen Share
arbitraryShare = Share
  <$> arbitraryShareThreshold
  <*> arbitraryShareNum
  <*> (NonEmpty.fromList <$> listOf1 arbitrarySizedNatural)

arbitrarySecret :: Gen Secret
arbitrarySecret = LText.encodeUtf8 . LText.pack <$> listOf1 arbitrary

prop_encodeDecodeShare :: Property
prop_encodeDecodeShare =
  forAll arbitraryShare
    (\share -> decodeShare (encodeShare share) === Just share)

prop_toFromShare :: Property
prop_toFromShare =
  forAll arbitraryShare (\share -> toShare (fromShare share) === Just share)

prop_encodeNumShares :: Property
prop_encodeNumShares = monadicIO $ do
  m <- pick arbitraryShareThreshold
  n <- pick (choose (m, SSSS.prime - 1))
  bytes  <- pick arbitrarySecret
  shares <- run (encode m n bytes)
  assert (length shares == n)

return []
main :: IO ()
main = do
  _ <- $(forAllProperties) (quickCheckWithResult stdArgs { maxSuccess = 500 })
  pure ()
