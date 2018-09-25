{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Network.Simple.WSS
 ( W.Connection
 , send
 , recv
   -- * Client side
 , connect
   -- * Url
 , Url
 , renderUrl
 ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Catch as Ex
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.String (fromString)

import qualified Network.Simple.TCP.TLS as T
import qualified Network.WebSockets as W
import qualified Network.WebSockets.Stream as W (Stream, makeStream)

--------------------------------------------------------------------------------

-- Secure WebSockets URL (@wss://@).
data Url = Url
  T.HostName
  -- ^ Server host name (e.g., @\"www.example.com\"@ or IP address).
  T.ServiceName
  -- ^ Server port (e.g., @\"443\"@ or @\"www"\@).
  String
  -- ^ WebSocket resource (e.g., @\"/foo?bar=baz\"@).
  --
  -- Leading @\'/'@ is optional.
  deriving (Eq, Ord, Show, Read)

-- Render the 'Url' as a @wss://@ URL.
renderUrl :: Url -> String
renderUrl (Url hn sn res) = mconcat
  [ "wss://", hn, ":", sn, "/", dropWhile (=='/') res ]

--------------------------------------------------------------------------------

connect
  :: (MonadIO m, Ex.MonadMask m)
  => T.ClientSettings  -- ^ TLS settings.
  -> Url -- ^ URL of the Secure WebSocket resource.
  -> [(String, String)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr) -> m r)
  -- ^ Computation to run after establishing a Secure WebSocket to the remote
  -- server. Takes the WebSocket connection and remote end address.
  -> m r
connect cs (Url hn sn res) hds act = do
  let hds' :: W.Headers = map (bimap fromString fromString) hds
      res' :: String = '/' : dropWhile (=='/') res
      hnsn :: String = hn ++ ":" ++ sn
      wopts :: W.ConnectionOptions = W.defaultConnectionOptions
        { W.connectionStrictUnicode =
            False -- Slows stuff down. And see 'recv'.
        , W.connectionCompressionOptions =
            W.PermessageDeflateCompression
               W.defaultPermessageDeflate }
  T.connect cs hn sn $ \(ctx, saddr) -> do
     stream <- streamFromContext ctx
     conn <- liftIO (W.newClientConnection stream hnsn res' wopts hds')
     act (conn, saddr)

streamFromContext :: MonadIO m => T.Context -> m W.Stream
streamFromContext ctx = liftIO $ do
  W.makeStream (T.recv ctx)
               (traverse_ (traverse_ (T.send ctx) . BL.toChunks))

-- | Receive bytes from the remote end.
--
-- Returns a strict 'BL.ByteString'.

-- Note: The WebSocket protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
recv :: MonadIO m => W.Connection -> m B.ByteString
{-# INLINABLE recv #-}
recv c = liftIO $ do
  dm <- W.receiveDataMessage c
  pure (BL.toStrict (case dm of
     W.Text b _ -> b
     W.Binary b -> b))

-- | Send bytes to the remote end.
--
-- Takes a lazy 'BL.ByteString'.

-- Note: The WebSocket protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
send :: MonadIO m => W.Connection -> BL.ByteString -> m ()
{-# INLINABLE send #-}
send c =
  liftIO . W.sendDataMessages c . map (W.Binary . BL.fromStrict) . BL.toChunks

