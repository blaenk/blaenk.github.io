{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

import Site.Types
import Site.Compilers
import Site.Contexts
import Site.Routes
import Site.WebSockets

import Data.Monoid ((<>))
import GHC.IO.Encoding
import System.Environment
import Control.Monad (when, void)
import System.FilePath (takeFileName)
import Control.Concurrent (forkIO)

hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
    deployCommand = "bash src/deploy.sh deploy"
  , providerDirectory = "provider"
  , destinationDirectory = "generated/deploy/out"
  , storeDirectory = "generated/deploy/cache"
  , tmpDirectory = "generated/deploy/cache/tmp"
  , previewHost = "0.0.0.0"
  , previewPort = 4000
  , ignoreFile = isIgnored
  }
  where
    isIgnored path
      | ignoreFile defaultConfiguration $ name = True
      -- 4913 is a file vim creates on windows to verify
      -- that it can indeed write to the specified path
      | name == "4913"                         = True
      | otherwise                              = False
      where name = takeFileName path

feedConf :: FeedConfiguration
feedConf = FeedConfiguration {
    feedTitle = "Jorge Israel Peña"
  , feedDescription = "Personal Site"
  , feedAuthorName = "Jorge Israel Peña"
  , feedAuthorEmail = "jorge.israel.p@gmail.com"
  , feedRoot = "http://www.blaenkdenum.com"
  }

cleanPreview :: IO ()
cleanPreview = do
  remove "generated/preview"
  remove "generated/scss"
  where
    remove dir = do
      putStrLn $ "Removing " ++ dir ++ "..."
      removeDirectory dir

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  channels <- newChannels

  (action:_) <- getArgs

  let preview = action == "watch" || action == "preview"
      clean = action == "clean"
      hakyllConf' =
        if preview
          then hakyllConf
               { destinationDirectory = "generated/preview/out"
               , storeDirectory       = "generated/preview/cache"
               , tmpDirectory         = "generated/preview/cache/tmp" }
          else hakyllConf
      previewPattern stem =
        let normal = fromGlob $ (stem) ++ "/*"
            drafts = fromGlob $ "drafts/" ++ (stem) ++ "/*"
        in if preview then normal .||. drafts else normal
      postsPattern = previewPattern "posts"
      notesPattern = previewPattern "notes"
      pagesPattern = previewPattern "pages"

  streams <- pygmentsServer

  when preview $ do
    void . forkIO $ webSocketServer channels

  when clean cleanPreview

  hakyllWith hakyllConf' $ do
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
                        , contentContext  = (sluggedTagsField "tags" tags <> postCtx preview)
                        , contentLayoutContext = postCtx preview }
        notes = posts   { contentPattern  = notesPattern
                        , contentRoute    = "notes/"
                        , contentTemplate = "note"
                        , contentContext  = postCtx preview }
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
      compile pandocFeedCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        loadAll (postsPattern .&&. hasVersion "feed")
          >>= fmap (take 10) . recentFirst
          >>= renderAtom feedConf (postCtx preview <> bodyField "description")

    match "templates/*" $ compile templateCompiler

