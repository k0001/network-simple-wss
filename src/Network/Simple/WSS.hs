{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Network.Simple.WSS
 ( W.Connection
 , send
 , recv
   -- * Client side
 , connect
   -- * Low level
 , connectionFromStream
 , streamFromContext
 ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as Ex
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.String (fromString)

import qualified Network.Simple.TCP.TLS as T
import qualified Network.WebSockets as W
import qualified Network.WebSockets.Stream as W (Stream, makeStream, close)

--------------------------------------------------------------------------------

-- | Connect to the specified Secure WebSockets server server.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => T.ClientSettings  -- ^ TLS settings.
  -> T.HostName
  -- ^ Secure WebSockets server host name (e.g., @\"www.example.com\"@ or IP
  -- address).
  -> T.ServiceName
  -- ^ Secure WebSockets server port (e.g., @\"443\"@ or @\"www"\@).
  -> String
  -- ^ Secure WebSockets resource (e.g., @\"foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(String, String)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr) -> m r)
  -- ^ Computation to run after establishing a Secure WebSockets to the remote
  -- server. Takes the WebSockets connection and remote end address.
  -> m r
connect cs hn sn res hds act = do
  T.connect cs hn sn $ \(ctx, saddr) -> do
     Ex.bracket (streamFromContext ctx) (liftIO . W.close) $ \stream -> do
        conn <- connectionFromStream stream hn sn res hds
        liftIO (W.forkPingThread conn 30)
        act (conn, saddr)

-- | Obtain a 'W.Connection' to the specified 'Uri' over the given 'W.Stream',
-- connected to either a WebSockets server, o a Secure WebSockets server.
connectionFromStream
  :: MonadIO m
  => W.Stream -- ^ Stream on which to establish the WebSockets connection.
  -> T.HostName
  -- ^ WebSockets server host name (e.g., @\"www.example.com\"@ or IP address).
  -> T.ServiceName -- ^ WebSockets server port (e.g., @\"443\"@ or @\"www"\@).
  -> String
  -- ^ WebSockets resource (e.g., @\"foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(String, String)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> m W.Connection -- ^ Established WebSockets connection
connectionFromStream stream hn sn res hds = liftIO $ do
  let hds' :: W.Headers = map (bimap fromString fromString) hds
      res' :: String = '/' : dropWhile (=='/') res
      hnsn :: String = hn ++ ":" ++ sn
      wopts :: W.ConnectionOptions = W.defaultConnectionOptions
        { W.connectionStrictUnicode =
            False -- Slows stuff down. And see 'recv'.
        , W.connectionCompressionOptions =
            W.PermessageDeflateCompression
               W.defaultPermessageDeflate }
  W.newClientConnection stream hnsn res' wopts hds'

-- | Obtain a 'W.Stream' implemented using the given TLS 'T.Context'. You can
-- use the
-- [https://hackage.haskell.org/package/network-simple-tls](network-simple-tls)
-- library to get one of those.
streamFromContext :: MonadIO m => T.Context -> m W.Stream
streamFromContext ctx = liftIO $ do
  W.makeStream (T.recv ctx)
               (traverse_ (traverse_ (T.send ctx) . BL.toChunks))

-- | Receive bytes from the remote end.
--
-- Returns a strict 'BL.ByteString'.
--
-- Returns an empty string when the remote end gracefully closes the connection.

-- Note: The WebSockets protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
recv :: MonadIO m => W.Connection -> m B.ByteString
{-# INLINABLE recv #-}
recv c = liftIO $ fix $ \k -> do
  ea <- Ex.try (W.receiveDataMessage c)
  case ea of
     Right (W.Text "" _) -> k
     Right (W.Text bl _) -> pure (BL.toStrict bl)
     Right (W.Binary "") -> k
     Right (W.Binary bl) -> pure (BL.toStrict bl)
     Left (W.CloseRequest 1000 _) -> pure ""
     Left (W.CloseRequest 1001 _) -> pure ""
     Left e -> Ex.throw e

-- | Send bytes to the remote end.
--
-- Takes a lazy 'BL.ByteString'.

-- Note: The WebSockets protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
send :: MonadIO m => W.Connection -> BL.ByteString -> m ()
{-# INLINABLE send #-}
send c = \bl -> liftIO $ do
  W.sendDataMessages c (map (W.Binary . BL.fromStrict) (BL.toChunks bl))

