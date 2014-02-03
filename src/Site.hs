{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Hakyll hiding (pandocCompiler)

import Site.Types
import Site.Filters
import Site.Fields
import Site.Routes
import Site.Pandoc

import Data.Monoid ((<>))
import GHC.IO.Encoding
import System.Environment
import Control.Monad (when, void)
import System.Exit (exitSuccess)
import System.FilePath (takeFileName)

-- pygments server
import System.IO.Streams.Process (runInteractiveProcess)

-- websockets stuff
import qualified Data.Map as Map

import qualified Data.Text as T
import Control.Exception (fromException, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as BC

import qualified Network.WebSockets as WS

myHakyllConf :: Configuration
myHakyllConf = defaultConfiguration
  { deployCommand = "bash src/deploy.sh deploy"
  , providerDirectory = "provider"
  , destinationDirectory = "generated/deploy/out"
  , storeDirectory = "generated/deploy/cache"
  , tmpDirectory = "generated/deploy/cache/tmp"
  , previewPort = 4000
  , ignoreFile = isIgnored
  }
  where isIgnored path
          | ignoreFile defaultConfiguration $ name = True
          -- 4913 is a file vim creates on windows to verify
          -- that it can indeed write to the specified path
          | name == "4913"                         = True
          | otherwise                              = False
          where name = takeFileName path

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle = "Jorge Israel Peña"
  , feedDescription = "Personal Site"
  , feedAuthorName = "Jorge Israel Peña"
  , feedAuthorEmail = "jorge.israel.p@gmail.com"
  , feedRoot = "http://blaenkdenum.com"
  }

indexCompiler :: String -> Routes -> Pattern -> Rules ()
indexCompiler name path itemsPattern =
  create [fromFilePath $ name ++ ".html"] $ do
    route path
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" (archiveCtx itemsPattern)
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

wsServer :: Channels -> IO ()
wsServer channels = do
  putStrLn "WebSocket Server Listening on http://0.0.0.0:9160/"
  WS.runServer "0.0.0.0" 9160 $ wsHandler channels

-- check map (tmvar) for route's channel
-- if none exists, create one and put it in the map
-- otherwise, duplicate the channel and inc refcount
wsHandler :: Channels -> WS.ServerApp
wsHandler channels pending = do
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

-- check if a channel exists for the underlying route
-- if so, pipe the item body through the channel
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

data Content = Content
  { contentPattern       :: Pattern
  , contentRoute         :: String
  , contentTemplate      :: String
  , contentContext       :: Context String
  , contentLayoutContext :: Context String }

contentCompiler :: Content -> Channels -> Streams -> Rules ()
contentCompiler content channels streams =
  match pattern $ do
    route $ niceRoute routeRewrite
    compile $ getResourceBody
      >>= pandocCompiler streams
      >>= webSocketPipe channels
      >>= loadAndApplyTemplate itemTemplate context
      >>= loadAndApplyTemplate "templates/layout.html" layoutContext
  where pattern       = contentPattern content
        routeRewrite  = contentRoute content
        template      = contentTemplate content
        context       = contentContext content
        layoutContext = contentLayoutContext content
        itemTemplate  = fromFilePath $ "templates/" ++ template ++ ".html"

deletePreviewDirs :: IO ()
deletePreviewDirs = do
  putStrLn "Removing generated/preview..."
  removeDirectory "generated/preview"
  putStrLn "Removing generated/scss..."
  removeDirectory "generated/scss"
  exitSuccess

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  channels <- atomically $ newTVar Map.empty

  (action:args) <- getArgs

  -- establish configuration based on preview-mode
  let previewMode = action == "watch" || action == "preview"
      clean = action == "clean" && (not . null $ args) && ((head args) == "preview")
      hakyllConf =
        if previewMode
          then myHakyllConf
               { destinationDirectory = "generated/preview/out"
               , storeDirectory       = "generated/preview/cache"
               , tmpDirectory         = "generated/preview/cache/tmp" }
          else myHakyllConf
      previewPattern stem =
        let normal = fromGlob $ (stem) ++ "/*"
            drafts = fromGlob $ "drafts/" ++ (stem) ++ "/*"
        in if previewMode then normal .||. drafts else normal
      postsPattern = previewPattern "posts"
      notesPattern = previewPattern "notes"
      pagesPattern = previewPattern "pages"

  -- pygments server
#ifdef mingw32_HOST_OS
  let python = "python"
#else
  let python = "python2"
#endif

  (inp, out, _, _) <- runInteractiveProcess python ["src/pig.py"] Nothing Nothing
  let streams = (inp, out)

  -- live reload
  when previewMode $ void . forkIO $ wsServer channels

  -- clean preview dirs as well
  when clean deletePreviewDirs

  hakyllWith hakyllConf $ do
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    match ("images/**" .||. "js/*" .||. "static/**" .||. "favicon.png" .||. "CNAME") $ do
      route idRoute
      compile copyFileCompiler

    match "scss/**.scss" $ do
      compile getResourceBody

    scssDependencies <- makePatternDependency "scss/**.scss"
    rulesExtraDependencies [scssDependencies] $ do
      create ["css/screen.css"] $ do
        route $ idRoute
        compile $ sassCompiler

    let posts = Content { contentPattern  = postsPattern
                        , contentRoute    = "posts/"
                        , contentTemplate = "post"
                        , contentContext  = (sluggedTagsField "tags" tags <> postCtx previewMode)
                        , contentLayoutContext = postCtx previewMode }
        notes = posts   { contentPattern  = notesPattern
                        , contentRoute    = "notes/"
                        , contentTemplate = "note"
                        , contentContext  = postCtx previewMode }
        pages = notes   { contentPattern  = pagesPattern
                        , contentRoute    = ""
                        , contentTemplate = "page" }

    contentCompiler posts channels streams
    contentCompiler notes channels streams
    contentCompiler pages channels streams

    indexCompiler "index" idRoute        postsPattern
    indexCompiler "notes" (niceRoute "") notesPattern

    niceTags tags $ \tag pattern -> do
      route $ niceRoute "tags/"
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/tags.html" (tagsCtx pattern tag)
          >>= loadAndApplyTemplate "templates/layout.html" (tagsCtx pattern tag)

    create ["404.html"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/404.html" defaultCtx
          >>= loadAndApplyTemplate "templates/layout.html" (customTitleField "Not Found" <> defaultCtx)

    match postsPattern $ version "feed" $
      compile $ getResourceBody
        >>= makeItem . itemBody
        >>= pandocFeedCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        loadAll (postsPattern .&&. hasVersion "feed")
          >>= fmap (take 10) . recentFirst
          >>= renderAtom feedConf (postCtx previewMode <> bodyField "description")

    match "templates/*" $ compile templateCompiler

