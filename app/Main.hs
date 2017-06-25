module Main where

import Import
import Ssss

import Control.Exception (Exception(..), catch)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics
import Options.Generic
import System.IO (hPutStrLn, stderr)
import System.Exit

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.ByteString.Char8 as Char8 (putStrLn)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as LText (fromStrict)
import qualified Data.Text.Lazy.IO as LText (putStrLn)
import qualified Data.Text.Lazy.Encoding as LText (decodeUtf8, encodeUtf8)

data Args
  = Encode Int Int (Maybe Text)
  | Decode (NonEmpty Text)
  deriving (Generic, Show)

instance ParseRecord Args

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
  Encode m n Nothing ->
    LByteString.getContents >>= encode' m n >>=
      mapM_ Char8.putStrLn . fmap encodeShare

  -- Encode a file, or the given bytes if reading the file fails.
  Encode m n (Just bytes) ->
    go1 <|> go2 >>= mapM_ Char8.putStrLn . fmap encodeShare
   where
    go1 = LByteString.readFile (Text.unpack bytes) >>= encode' m n
    go2 = encode' m n (LText.encodeUtf8 (LText.fromStrict bytes))

  -- Decode a list of files, or the given bytes if reading the files fails.
  Decode xs ->
    go1 <|> go2 >>= LText.putStrLn . LText.decodeUtf8
   where
    go1 = mapM (ByteString.readFile . Text.unpack) xs >>= decode
    go2 = decode (Text.encodeUtf8 <$> xs)

encode' :: Int -> Int -> Secret -> IO (NonEmpty Share)
encode' (fromIntegral -> m) (fromIntegral -> n) = encode m n
