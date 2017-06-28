{-# language ScopedTypeVariables #-}

module Main where

import Import

import Args (Args(..), parseArgs)
import Sss

import Control.Exception (Exception(..), catch)
import Crypto.Cipher.Salsa.Streaming (SalsaException)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Text.Encoding (encodeUtf8)
import System.IO (hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)
import System.Posix.IO (stdError)
import System.Posix.Terminal (queryTerminal)

import qualified Crypto.Cipher.Salsa.Streaming as Salsa
import qualified Crypto.Random as Random
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Streaming as B

main :: IO ()
main =
  catch
    (parseArgs >>= \case
      Encode  a b c -> doEncode  a b c
      Decode  a     -> doDecode  a
      Encrypt a b c -> doEncrypt a b c
      Decrypt a b   -> doDecrypt a b)
    (\e -> do
      hPutStrLn stderr (displayException (e :: SssException))
      exitFailure)

doEncode :: Word16 -> Word16 -> Maybe Text -> IO ()
doEncode m n = \case
  Nothing -> do
    secret <- getContents
    shares <- encode m n secret
    mapM_ (putStrLn . encodeShare) shares

  Just bytes -> do
    shares <- tryBinaryFile bytes (encode m n . lazy)
    mapM_ (putStrLn . encodeShare) shares

doDecode :: NonEmpty Text -> IO ()
doDecode = tryTextFiles >=> decode >=> hPut stdout

doEncrypt :: Word16 -> Word16 -> Maybe Text -> IO ()
doEncrypt m n mbytes = do
  -- Doesn't make sense to put binary data to the terminal
  tty <- queryTerminal stdError
  if tty
    then throw StderrIsTTY
    else
      case mbytes of
        Nothing -> encrypt B.stdin
        Just path -> tryBinaryStream path encrypt
 where
  encrypt :: B.ByteString (ResourceT IO) () -> IO ()
  encrypt secret = do
    key <- Random.getRandomBytes 16 :: IO ByteString
    runResourceT (B.hPut stderr (Salsa.combine rounds key nonce secret))

    shares <- encode m n (lazy key)
    mapM_ (putStrLn . encodeShare) shares

doDecrypt :: Text -> NonEmpty Text -> IO ()
doDecrypt = \case
  "-"  -> decrypt B.getContents
  path -> decrypt (B.readFile (unpack path))
 where
  decrypt :: B.ByteString (ResourceT IO) () -> NonEmpty Text -> IO ()
  decrypt secret = tryTextFiles >=> go
   where
    go :: NonEmpty Text -> IO ()
    go shares = do
      key <- decode shares
      catch
        (runResourceT
          (B.stdout (Salsa.combine rounds (strict key) nonce secret)))
        (\(_ :: SalsaException) -> throw MalformedKey)

-- Given a 'Text' that might be a file path, either call the continuation with
-- the contents of the file, or else the UTF8-encoded 'Text' itself.
tryBinaryFile :: Text -> (ByteString -> IO r) -> IO r
tryBinaryFile path k = (readFile (unpack path) >>= k) <|> k (encodeUtf8 path)

tryBinaryStream :: Text -> (B.ByteString (ResourceT IO) () -> IO ()) -> IO ()
tryBinaryStream (unpack -> path) k =
  k (B.readFile path) <|> k (B.chunk (pack path))

-- Given a 'Text' that might be a file path, either call the continuation with
-- the contents of the file, or else the 'Text' itself.
tryTextFile :: Text -> CIO Text
tryTextFile path = CIO (\k -> (readFile (unpack path) >>= k) <|> k path)

tryTextFiles :: Traversable t => t Text -> IO (t Text)
tryTextFiles = lower . traverse tryTextFile

-- Salsa rounds (8, 12, 20)
rounds :: Int
rounds = 20

-- Salsa nonce (64 or 96 bits)
nonce :: ByteString
nonce = ByteString.replicate 8 0

--------------------------------------------------------------------------------
-- Codensity IO

newtype CIO a
  = CIO { (>>-) :: forall r. (a -> IO r) -> IO r }

lower :: CIO a -> IO a
lower = (>>- pure)

instance Functor CIO where
  fmap f x = CIO (\k -> x >>- \a -> k (f a))

instance Applicative CIO where
  pure = return
  (<*>) = ap

instance Monad CIO where
  return x = CIO (\k -> k x)
  x >>= f = CIO (\k -> x >>- \a -> f a >>- k)
