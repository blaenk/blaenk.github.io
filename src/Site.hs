{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

import Site.Filters
import Site.Fields
import Site.Routes
import Site.Pandoc

import Data.Monoid ((<>))

myHakyllConf :: Configuration
myHakyllConf = defaultConfiguration
  { deployCommand = "bash src/deploy.sh deploy"
  , providerDirectory = "provider"
  , destinationDirectory = "generated/preview"
  , storeDirectory = "generated/cache"
  , tmpDirectory = "generated/cache/tmp"
  }

main :: IO ()
main = hakyllWith myHakyllConf $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match ("images/**" .||. "font/*" .||. "js/*" .||. "static/**" .||. "favicon.png" .||. "CNAME") $ do
    route idRoute
    compile copyFileCompiler

  match "scss/screen.scss" $ do
    route $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
    compile $ sassCompiler

  match "posts/*" $ do
    route $ nicePostRoute
    compile $ getResourceBody
      >>= withItemBody (abbreviationFilter)
      >>= pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags <> postCtx)
      >>= loadAndApplyTemplate "templates/layout.html" postCtx

  match "pages/*" $ do
    route $ nicePageRoute
    compile $ getResourceBody
      >>= withItemBody (abbreviationFilter)
      >>= pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" postCtx
      >>= loadAndApplyTemplate "templates/layout.html" postCtx

  create ["404.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/404.html" defaultCtx
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

  create ["index.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" (archiveCtx "posts/*")
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

  niceTags tags $ \tag pattern -> do
    route nicePostRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" (constField "tag" tag <> tagsCtx tags <> archiveCtx pattern)
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

  match "templates/*" $ compile templateCompiler

