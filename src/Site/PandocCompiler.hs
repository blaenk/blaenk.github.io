{-# LANGUAGE OverloadedStrings #-}

module Site.PandocCompiler (pandocCompiler) where

import Prelude hiding (div, span)

import Hakyll.Web.Pandoc hiding (pandocCompiler)

import Text.Pandoc

import qualified Data.Set as S
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import System.IO.Unsafe
import System.Process
import System.IO (hClose, hGetContents, hPutStr, hSetEncoding, localeEncoding)
import Control.Concurrent (forkIO)
import Data.List hiding (span)
import Text.Regex.TDFA

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (span)

import Control.Applicative((<$>))

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocTransformer readerOptions writerOptions transformer
  where transformer = (bottomUp pygments) . tocTransformer
        tocTransformer ast = let headers = queryWith queryHeaders ast
                             in bottomUp (tableOfContents headers) ast

pandocTransformer :: ReaderOptions -> WriterOptions -> (Pandoc -> Pandoc) -> Item String -> Compiler (Item String)
pandocTransformer ropt wopt f item =
  writePandocWith wopt . fmap f . readPandocWith ropt <$> (return $ item)

queryHeaders :: Block -> [Block]
queryHeaders hdr@(Header level attr text) = [hdr]
queryHeaders _ = []

tableOfContents :: [Block] -> (Block -> Block)
tableOfContents headers = genToc
  where genToc :: Block -> Block
        genToc bl@(BulletList (( (( Plain ((Str "toc"):_)):_)):_)) =
          let toc = map convertHeader headers
          in BulletList toc
          where convertHeader header@(Header level attr text) =
                  case level of
                    _ -> [Plain text]
                convertHeader _ = [Plain [Str "what"]]
        genToc x = x
tableOfContents [] = (\x -> x)

pygments :: Block -> Block
pygments (CodeBlock (_, _, namevals) contents) =
  let lang = case lookup "lang" namevals of
               Just lang_ -> lang_
               Nothing -> "text"
      text = case lookup "text" namevals of
               Just text_ -> text_
               Nothing -> ""
      colored = pygmentize lang contents
      code = numberedCode colored lang
      caption = if text /= ""
                  then renderHtml $ figcaption $ span $ toHtml text
                  else ""
      composed = renderHtml $ figure ! class_ "code" $ preEscapedToHtml $ code ++ caption
  in RawBlock "html" composed
pygments x = x

numberedCode :: String -> String -> String
numberedCode codeHtml lang =
  let codeLines = lines $ extractCode codeHtml
      wrappedCode = unlines $ wrapCodeLines codeLines
      numbers = unlines $ numberLines codeLines
  in renderHtml $ do
      div ! class_ "highlight" $ do
        table $ tr $ do
          td ! class_ "gutter" $ do
            pre ! class_ "line-numbers" $ do
              preEscapedToHtml numbers
          td ! class_ "code" $ do
            pre $ do
              code ! class_ (toValue lang) $ do
                preEscapedToHtml wrappedCode
  where wrapCodeLines codeLines = map wrapCodeLine codeLines
          where wrapCodeLine line = renderHtml $ span ! class_ "line" $ preEscapedToHtml line
        numberLines codeLines =
          let (_, res) = mapAccumL numberLine 1 codeLines
          in res
            where numberLine :: Integer -> String -> (Integer, String)
                  numberLine num _ = (num + 1, renderHtml $ span ! class_ "line-number" $ toHtml $ show num)

extractCode :: String -> String
extractCode pygmentsResult =
  let preRegex = makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt ("<pre>(.+)</pre>" :: String)
      matched = (match preRegex pygmentsResult :: [[String]]) !! 0 !! 1
  in matched

pygmentize :: String -> String -> String
pygmentize lang contents = unsafePerformIO $ do
  let process = (shell ("pygmentize -f html -l " ++ lang ++ " -P encoding=utf-8")) {std_in = CreatePipe, std_out = CreatePipe, close_fds = True}
      writer handle input = do
        hSetEncoding handle localeEncoding
        hPutStr handle input
      reader handle = do
        hSetEncoding handle localeEncoding
        hGetContents handle

  (Just stdin, Just stdout, _, _) <- createProcess process

  _ <- forkIO $ do
    writer stdin contents
    hClose stdin

  reader stdout

readerOptions :: ReaderOptions
readerOptions = def {
  readerSmart = True
  }

writerOptions :: WriterOptions
writerOptions = 
  let extensions = S.fromList [
        Ext_literate_haskell,
        Ext_tex_math_dollars
        -- Ext_abbreviations -- enable once pandoc gets abbreviation support
        ]
  in def {
    writerTableOfContents = True,
    writerHTMLMathMethod = MathJax "",
    writerExtensions = S.union extensions (writerExtensions def)
    }

