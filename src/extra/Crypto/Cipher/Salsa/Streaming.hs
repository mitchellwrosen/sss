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
  s <-
    catch
      (liftIO (evaluate (Salsa.initialize r k n)))
      (throwM . SalsaException)

  _ :> x <-
    B.chunkFoldM step (pure s) pure (hoist lift bytes)

  pure x
 where
  step :: Salsa.State -> ByteString -> B.ByteString m Salsa.State
  step s xs = do
    B.chunk xs'
    pure s'
   where
    (xs', s') = Salsa.combine s xs
