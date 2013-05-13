{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

import Site.Filters
import Site.Fields
import Site.Routes
import Site.PandocCompiler

myHakyllConf :: Configuration
myHakyllConf = defaultConfiguration

main :: IO ()
main = hakyllWith myHakyllConf $ do
  match ("images/**" .||. "font/*" .||. "js/*" .||. "favicon.png") $ do
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
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/layout.html" postCtx

  match "pages/*" $ do
    route $ nicePageRoute
    compile $ getResourceBody
      >>= withItemBody (abbreviationFilter)
      >>= pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" postCtx
      >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

  create ["404.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/404.html" archiveCtx
        >>= loadAndApplyTemplate "templates/layout.html" archiveCtx

  create ["index.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" archiveCtx
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

  match "templates/*" $ compile templateCompiler

