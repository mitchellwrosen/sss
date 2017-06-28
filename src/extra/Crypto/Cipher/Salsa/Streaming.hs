{-# language ScopedTypeVariables #-}

module Crypto.Cipher.Salsa.Streaming where

import Import

import Control.Monad.Catch (MonadCatch, catch)
import Data.ByteArray (ByteArrayAccess)
import Streaming

import qualified Crypto.Cipher.Salsa as Salsa
import qualified Data.ByteString.Streaming as B

data SalsaException
  = SalsaException SomeException
  deriving (Show, Typeable)

instance Exception SalsaException

combine
  :: forall key m nonce r.
     (ByteArrayAccess key, ByteArrayAccess nonce, MonadCatch m, MonadIO m)
  => Int
  -> key
  -> nonce
  -> B.ByteString m r
  -> B.ByteString m r
combine r k n bytes = do
  -- Tease out any annoying impure exception thrown during 'initialize'
  s0 <-
    catch
      (liftIO (evaluate (Salsa.initialize r k n)))
      (throwM . SalsaException)

  ref <- liftIO (newIORef s0)

  B.chunkMapM
    (\chunk -> do
      s <- liftIO (readIORef ref)
      let (chunk', s') = Salsa.combine s chunk
      liftIO (writeIORef ref s')
      pure chunk')
    bytes
