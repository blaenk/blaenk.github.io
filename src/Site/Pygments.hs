{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Pygments (
  pygmentsServer,
  pygments
) where

import Site.Types

import Hakyll

import Text.Pandoc
import Text.Pandoc.Walk (walkM)

import Control.Monad ((>=>))

import Data.Maybe (fromMaybe, fromJust)

import qualified System.IO.Streams as S
import System.IO.Streams.Process (runInteractiveProcess)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U8

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pygmentsServer :: IO Streams
pygmentsServer = do
  (inp, out, _, _) <- runInteractiveProcess "python" ["src/pig.py"] Nothing Nothing
  return (inp, out)

pygments :: Streams -> Pandoc -> Compiler Pandoc
pygments streams = walkM (generateCodeBlock streams)

generateCodeBlock :: Streams -> Block -> Compiler Block
generateCodeBlock streams (CodeBlock (_, classes, keyvals) contents) = do
  let lang = fromMaybe (if null classes then "text" else head classes) $ lookup "lang" keyvals

  code <- if lang == "text"
            then return $ renderHtml $ H.toHtml contents
            else pygmentize streams lang contents

  let colored = renderHtml $ H.pre $ H.code ! A.class_ (H.toValue $ "highlight language-" ++ lang) $ do
                  preEscapedToHtml code
      caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) $ lookup "text" keyvals
      composed = renderHtml $ H.figure ! A.class_ "codeblock" $ do
                   preEscapedToHtml $ caption ++ colored

  return $ RawBlock "html" composed
generateCodeBlock _ x = return x

pygmentize :: Streams -> String -> String -> Compiler String
pygmentize (os, is) lang contents = unsafeCompiler $ do
  let lang'     = U8.fromString lang
      contents' = U8.fromString contents
      len       = U8.fromString . show . U8.length $ contents'

      -- REQUEST:  LANG\nLENGTH\nCODE
      request = C.intercalate "\n" [lang', len, contents']

  mapM_ (flip S.write os) $ map Just [request, ""]

  -- RESPONSE: LENGTH\nRESPONSE
  responseLength <- read . U8.toString . fromJust <$> (S.lines >=> S.read) is
  U8.toString <$> S.readExactly responseLength is

