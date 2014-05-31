{-# LANGUAGE OverloadedStrings #-}

module Site.Compilers (
  indexCompiler,
  contentCompiler,
  sassCompiler,
  pandocCompiler,
  pandocFeedCompiler,
  pygmentsServer
) where

import Site.Types
import Site.Pandoc
import Site.Contexts
import Site.Routes
import Site.WebSockets
import Site.Pygments

import Hakyll hiding (pandocCompiler)

indexCompiler :: String -> Routes -> Pattern -> Rules ()
indexCompiler name path itemsPattern =
  create [fromFilePath $ name ++ ".html"] $ do
    route path
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" (archiveCtx itemsPattern)
        >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

contentCompiler :: Content -> Channels -> Streams -> Rules ()
contentCompiler content channels streams =
  match pattern $ do
    route $ niceRoute routeRewrite
    compile $ pandocCompiler streams
      >>= webSocketPipe channels
      >>= loadAndApplyTemplate itemTemplate context
      >>= loadAndApplyTemplate "templates/layout.html" layoutContext
  where pattern       = contentPattern content
        routeRewrite  = contentRoute content
        template      = contentTemplate content
        context       = contentContext content
        layoutContext = contentLayoutContext content
        itemTemplate  = fromFilePath $ "templates/" ++ template ++ ".html"

sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "scss/screen.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter "scss" args)
  where args = ["-s", "-I", "provider/scss/",
                "--cache-location", "generated/scss"]

