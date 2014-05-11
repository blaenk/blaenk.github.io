module Site.WebSockets (
  webSocketServer,
  webSocketPipe,
  newChannels
) where

import Site.Types

import Hakyll

-- websockets stuff
import qualified Data.Map as Map

import qualified Data.Text as T
import Control.Exception (fromException, handle)
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as BC

import qualified Network.WebSockets as WS

webSocketServer :: Channels -> IO ()
webSocketServer channels = do
  putStrLn "WebSocket Server Listening on http://0.0.0.0:9160/"
  WS.runServer "0.0.0.0" 9160 $ webSocketHandler channels

-- check map (tmvar) for route's channel
-- if none exists, create one and put it in the map
-- otherwise, duplicate the channel and inc refcount
webSocketHandler :: Channels -> WS.ServerApp
webSocketHandler channels pending = do
  let request = WS.pendingRequest pending
      path    = tail . BC.unpack $ WS.requestPath request

  conn <- WS.acceptRequest pending

  -- needs to be atomic to avoid race conditions
  -- between the read and the update
  chan <- liftIO $ atomically $ do
    chans <- readTVar channels

    case Map.lookup path chans of
      Just (ch, refcount) -> do
        modifyTVar' channels $ Map.insert path (ch, refcount + 1)
        dupTChan ch
      Nothing -> do
        ch <- newBroadcastTChan
        modifyTVar' channels $ Map.insert path (ch, 1)
        dupTChan ch

  -- pipes the data from the channel to the websocket
  handle catchDisconnect . forever . liftIO $ do
    atomically (readTChan chan) >>= WS.sendTextData conn . T.pack

  -- decrement the ref count of the channel
  -- remove it if no listeners
  -- this is probably important, to avoid build-up within the channel
  atomically $ do
    chans <- readTVar channels
    case Map.lookup path chans of
      Just (ch, refcount) -> do
        if (refcount - 1) == 0
          then modifyTVar' channels $ Map.delete path
          else modifyTVar' channels $ Map.insert path (ch, refcount - 1)
      Nothing -> return ()

  where
    catchDisconnect e =
      case fromException e of
        Just WS.ConnectionClosed -> return ()
        _ -> return ()

newChannels :: IO Channels
newChannels = atomically $ newTVar Map.empty

webSocketPipe :: Channels -> Item String -> Compiler (Item String)
webSocketPipe channels item =
  unsafeCompiler $ do
    let path = toFilePath . itemIdentifier $ item
        body = itemBody item

    void . forkIO $ atomically $ do
      chans <- readTVar channels

      case Map.lookup path chans of
        Just (ch, _) -> writeTChan ch body
        Nothing -> return ()

    return item

