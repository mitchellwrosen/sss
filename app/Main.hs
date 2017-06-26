{-# language ScopedTypeVariables #-}

module Main where

import Import
import Ssss

import Control.Exception (Exception(..), catch)
import Crypto.Cipher.Salsa.Streaming (SalsaException)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Options.Generic
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)

import qualified Crypto.Cipher.Salsa.Streaming as Salsa
import qualified Crypto.Random as Random
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Streaming as B

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
    mapM_ (putStrLn . encodeShare) shares

  -- Encode a file, or the given bytes if reading the file fails.
  Encode' m n (Just bytes) -> do
    shares <- go1 <|> go2
    mapM_ (putStrLn . encodeShare) shares
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
    mapM_ (putStrLn . encodeShare) shares

    B.hPut stderr (Salsa.combine rounds key nonce B.stdin)

  -- Decrypt a file (or stdin) with a list of shares.
  Decrypt "-" shares -> decrypt B.getContents shares
  Decrypt path shares -> decrypt (B.readFile (unpack path)) shares

decrypt :: B.ByteString (ResourceT IO) () -> NonEmpty Text -> IO ()
decrypt secret shares = do
  key <- decode shares

  catch
    (runResourceT (B.stdout (Salsa.combine rounds (strict key) nonce secret)))
    (\(_ :: SalsaException) -> throw MalformedKey)

-- Salsa rounds (8, 12, 20)
rounds :: Int
rounds = 20

-- Salsa nonce (64 or 96 bits)
nonce :: ByteString
nonce = ByteString.replicate 8 0
