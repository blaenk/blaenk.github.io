{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

import Site.Filters
import Site.Fields
import Site.Routes
import Site.Pandoc

import Data.Monoid ((<>))
import GHC.IO.Encoding
import System.Environment
import Control.Monad (when)
import System.Exit (exitSuccess)
import System.FilePath (takeFileName)

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

contentCompiler :: Configuration ->
                   Pattern ->
                   String ->
                   String ->
                   Context String ->
                   Context String ->
                   Rules ()
contentCompiler conf pattern rewrite contentTmpl contentCtx layoutCtx =
  match pattern $ do
    route $ niceRoute rewrite
    compile $ getResourceBody
      >>= pandocCompiler (storeDirectory conf)
      >>= loadAndApplyTemplate itemTemplate contentCtx
      >>= loadAndApplyTemplate "templates/layout.html" layoutCtx
  where itemTemplate = fromFilePath $ "templates/" ++ contentTmpl ++ ".html"

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  
  (action:args) <- getArgs

  -- establish configuration based on preview-mode
  let previewMode  = action == "preview"
      hakyllConf   = if previewMode
                     then myHakyllConf
                          { destinationDirectory = "generated/preview/out"
                          , storeDirectory = "generated/preview/cache"
                          , tmpDirectory = "generated/preview/cache/tmp"
                          }
                     else myHakyllConf
      previewPattern stem = if previewMode then normal .||. drafts else normal
                            where normal = fromGlob $ (stem) ++ "/*"
                                  drafts = fromGlob $ "drafts/" ++ (stem) ++ "/*"
      postsPattern = previewPattern "posts"
      notesPattern = previewPattern "notes"
      pagesPattern = previewPattern "pages"

  when (action == "clean" &&
       (not . null $ args) &&
       ((head args) == "preview")) $ do
    putStrLn "Removing generated/preview..."
    removeDirectory "generated/preview"
    putStrLn "Removing generated/scss..."
    removeDirectory "generated/scss"
    putStrLn "Removing generated/pygments..."
    removeDirectory "generated/pygments"
    exitSuccess

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

    contentCompiler hakyllConf notesPattern "notes/" "note" postCtx postCtx
    contentCompiler hakyllConf postsPattern "posts/" "post" (sluggedTagsField "tags" tags <> postCtx) postCtx
    contentCompiler hakyllConf pagesPattern ""       "page" postCtx postCtx

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
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst
          =<< loadAll (postsPattern .&&. hasVersion "feed")
        renderAtom feedConf feedCtx posts

    match "templates/*" $ compile templateCompiler

