{-# language ScopedTypeVariables #-}

module Main where

import Import
import Ssss

import Control.Exception
  (Exception(..), SomeException, catch, evaluate, throwIO)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Options.Generic
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)

import qualified Crypto.Cipher.Salsa as Salsa
import qualified Crypto.Random as Random
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

data Args
  = Encode Int Int (Maybe Text)
  | Decode (NonEmpty Text)
  | Encrypt Int Int (Maybe Text)
  | Decrypt Text (NonEmpty Text)
  deriving (Generic, Show)

instance ParseRecord Args

pattern Encode' :: Word16 -> Word16 -> Maybe Text -> Args
pattern Encode' m n s <- Encode (fromIntegral -> m) (fromIntegral -> n) s

pattern Encrypt' :: Word16 -> Word16 -> Maybe Text -> Args
pattern Encrypt' m n s <- Encrypt (fromIntegral -> m) (fromIntegral -> n) s

main :: IO ()
main =
  catch
    (getRecord "ssss" >>= main')
    (\e -> do
      hPutStrLn stderr (displayException (e :: SsssException))
      exitFailure)

main' :: Args -> IO ()
main' = \case
  -- Encode stdin
  Encode' m n Nothing -> do
    secret <- getContents
    shares <- encode m n secret
    mapM_ (Char8.putStrLn . encodeShare) shares

  -- Encode a file, or the given bytes if reading the file fails.
  Encode' m n (Just bytes) -> do
    shares <- go1 <|> go2
    mapM_ (Char8.putStrLn . encodeShare) shares
   where
    go1 :: IO (NonEmpty Share)
    go1 = do
      secret <- readFile (unpack bytes)
      encode m n secret

    go2 :: IO (NonEmpty Share)
    go2 = encode m n (encodeUtf8 (lazy bytes))

  -- Decode a list of shares, which are interpreted as filenames, then string
  -- literals.
  Decode shares -> do
    secret <- go1 <|> go2
    hPut stdout secret
    hFlush stdout
   where
    go1 = mapM (readFile . unpack) shares >>= decode
    go2 = decode shares

  -- Encrypt and encode stdin
  Encrypt' m n Nothing -> do
    key <- Random.getRandomBytes 16 :: IO ByteString
    shares <- encode m n (lazy key)

    mapM_ (Char8.putStrLn . encodeShare) shares

    (secret, _) <-
      Salsa.combine (Salsa.initialize rounds key nonce) <$>
        (getContents :: IO ByteString)

    hPut stderr secret
    hFlush stderr

  -- Decrypt a file (or stdin) with a list of shares.
  Decrypt "-" shares -> decrypt getContents shares
  Decrypt path shares -> decrypt (readFile (unpack path)) shares

decrypt :: IO ByteString -> NonEmpty Text -> IO ()
decrypt getSecret shares = do
  key <- decode shares

  state <-
    catch
      (evaluate (Salsa.initialize rounds (strict key) nonce))
      (\(_ :: SomeException) -> throwIO MalformedKey)

  (secret, _) <- Salsa.combine state <$> getSecret

  hPut stdout secret
  hFlush stdout

-- Salsa rounds (8, 12, 20)
rounds :: Int
rounds = 20

-- Salsa nonce (64 or 96 bits)
nonce :: ByteString
nonce = ByteString.replicate 8 0
