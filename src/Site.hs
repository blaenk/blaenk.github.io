{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

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

-- websockets stuff

import qualified Data.Map as Map

import qualified Data.Text as T
import Control.Exception (fromException, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar

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
indexCompiler name route' itemsPattern =
  create [name'] $ do
    route route'
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" (archiveCtx itemsPattern)
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx
  where name' = fromFilePath $ name ++ ".html"

{-
  have a map of route to channel
  when a ws connection is created, check map (TMVar) for route's channel (TChan)
    if one exists, duplicate it
    if none exists, create one and put it in the map
  in the pushToWebSocket compiler
    check if channel exists in map for underlying route
      if one exists, write the contents to it
      if none exists, do nothing
-}

type Channels = TMVar (Map.Map String (TChan String, Integer))

wsApp :: Channels -> WS.ServerApp
wsApp channels pending = do
  let request = WS.pendingRequest pending
      path = tail $ BC.unpack $ WS.requestPath request

  conn <- WS.acceptRequest pending

  -- needs to be atomic to avoid race conditions
  -- between the read and the update
  chan <- liftIO $ atomically $ do
    chans <- readTMVar channels

    case Map.lookup path chans of
      Just existing -> do
        void $ swapTMVar channels $ Map.insert path (fst existing, (snd existing) + 1) chans
        dupTChan (fst existing)
      Nothing -> do
        ch <- newTChan
        void $ swapTMVar channels $ Map.insert path (ch, 1) chans
        return ch

  -- pipes the data from the channel to the websocket
  handle catchDisconnect $ forever $ liftIO $ do
    contents <- atomically $ readTChan chan
    WS.sendTextData conn (T.pack contents)

  -- decrement the ref count of the channel
  -- remove it if no listeners
  -- this is probably important, to avoid build-up within the channel
  void $ atomically $ do
    chans <- readTMVar channels
    case Map.lookup path chans of
      Just existing -> do
        let refcount = (snd existing) - 1
        if refcount == 0
          then void $ swapTMVar channels $ Map.delete path chans
          else void $ swapTMVar channels $ Map.insert path ((fst existing), refcount) chans
      Nothing -> return ()

  return ()
  where catchDisconnect e =
          case fromException e of
            Just WS.ConnectionClosed -> return ()
            _ -> return ()

wsServer :: Channels -> IO ()
wsServer channels = do
  WS.runServer "0.0.0.0" 9160 $ wsApp channels

pushToWebSocket :: Channels -> Item String -> Compiler (Item String)
pushToWebSocket channels item =
  unsafeCompiler $ do
    let path = toFilePath . itemIdentifier $ item
        body = itemBody item

    chans <- atomically $ readTMVar channels

    case Map.lookup path chans of
      Just existing -> do
        atomically $ writeTChan (fst existing) body
      Nothing -> do
        return ()

    return item

data Content = Content
  { contentConfiguration :: Configuration
  , contentPattern       :: Pattern
  , contentRoute         :: String
  , contentTemplate      :: String
  , contentContext       :: Context String
  , contentLayoutContext :: Context String }

contentCompiler :: Content -> Channels -> Rules ()
contentCompiler content channels =
  match pattern $ do
    route $ niceRoute routeRewrite
    compile $ getResourceBody
      >>= pandocCompiler (storeDirectory conf)
      >>= pushToWebSocket channels
      >>= loadAndApplyTemplate itemTemplate context
      >>= loadAndApplyTemplate "templates/layout.html" layoutContext
  where conf          = contentConfiguration content
        pattern       = contentPattern content
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
  putStrLn "Removing generated/pygments..."
  removeDirectory "generated/pygments"
  exitSuccess

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  
  channels <- atomically $ newTMVar Map.empty

  (action:args) <- getArgs

  -- establish configuration based on preview-mode
  let previewMode = action == "watch" || action == "preview"
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

  -- live reload
  when previewMode $ void . forkIO $ wsServer channels

  when (action == "clean" &&
       (not . null $ args) &&
       ((head args) == "preview")) deletePreviewDirs

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

    let posts = Content { contentConfiguration = hakyllConf
                        , contentPattern  = postsPattern
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

    contentCompiler posts channels
    contentCompiler notes channels
    contentCompiler pages channels

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
        let feedCtx = postCtx previewMode <> bodyField "description"
        feedItems <- fmap (take 10) . recentFirst =<< loadAll (postsPattern .&&. hasVersion "feed")
        renderAtom feedConf feedCtx feedItems

    match "templates/*" $ compile templateCompiler

