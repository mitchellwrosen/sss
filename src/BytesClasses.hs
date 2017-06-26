module BytesClasses where

import Data.Word (Word8)
import Prelude
import System.IO (Handle)

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy

class HandleIO a where
  getContents :: IO a
  hPut :: Handle -> a -> IO ()
  readFile :: FilePath -> IO a

instance HandleIO Data.ByteString.ByteString where
  getContents = Data.ByteString.getContents
  hPut = Data.ByteString.hPut
  readFile = Data.ByteString.readFile

instance HandleIO Data.ByteString.Lazy.ByteString where
  getContents = Data.ByteString.Lazy.getContents
  hPut = Data.ByteString.Lazy.hPut
  readFile = Data.ByteString.Lazy.readFile

instance HandleIO Data.Text.Text where
  getContents = Data.Text.IO.getContents
  hPut = Data.Text.IO.hPutStr
  readFile = Data.Text.IO.readFile

class TextIO a where
  putStrLn :: a -> IO ()

class Lazy a b | a -> b where
  lazy :: a -> b

instance Lazy Data.ByteString.ByteString Data.ByteString.Lazy.ByteString where
  lazy = Data.ByteString.Lazy.fromStrict

instance Lazy Data.Text.Text Data.Text.Lazy.Text where
  lazy = Data.Text.Lazy.fromStrict

class Strict a b | a -> b where
  strict :: a -> b

instance Strict Data.ByteString.Lazy.ByteString Data.ByteString.ByteString where
  strict = Data.ByteString.Lazy.toStrict

class Pack a b where
  pack :: a -> b

instance Pack [Word8] Data.ByteString.ByteString where
  pack = Data.ByteString.pack

class Unpack a b where
  unpack :: a -> b

instance Unpack Data.Text.Text [Char] where
  unpack = Data.Text.unpack
