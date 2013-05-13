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
import Text.Blaze.Html5 hiding (map, header, contents, caption, input)
import Text.Blaze.Html5.Attributes hiding (span, headers, item, lang)

import Control.Applicative((<$>))

import Data.Tree
import Data.Ord

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocTransformer readerOptions writerOptions transformer
  where transformer = (bottomUp pygments) . tocTransformer . (bottomUp linkImages)
        tocTransformer ast = let headers = queryWith queryHeaders ast
                             in bottomUp (tableOfContents headers) ast

-- this compiler reads the item instead of the resourceBody, allowing
-- it to be preceded by a filter (e.g. abbreviationFilter)
pandocTransformer :: ReaderOptions -> WriterOptions -> (Pandoc -> Pandoc) -> Item String -> Compiler (Item String)
pandocTransformer ropt wopt f item =
  writePandocWith wopt . fmap f . readPandocWith ropt <$> (return $ item)

data TocItem = TocItem {
  tocLevel :: Int,
  tocIdent :: String,
  tocInline :: String
  }

instance Eq TocItem where
  (TocItem a _ _) == (TocItem b _ _) = a == b

instance Ord TocItem where
  compare = comparing tocLevel

normalizeTocs :: [TocItem] -> [TocItem]
normalizeTocs tocs = map normalize tocs
  where minLevel :: Int
        minLevel = subtract 1 . tocLevel . minimum $ tocs
        normalize item@(TocItem level _ _) = item {tocLevel = level - minLevel}

queryHeaders :: Block -> [TocItem]
queryHeaders hdr@(Header level (ident, _, _) text) = [TocItem level ident (writeHtmlString def (Pandoc (Meta [] [] []) [(Plain text)]))]
queryHeaders _ = []

tocTree :: [TocItem] -> Forest TocItem
tocTree x = (map (\(x : xs) -> Node x (tocTree xs)) . groupBy (comp)) x
  where comp (TocItem a _ _) (TocItem b _ _) = a < b

genToc :: Forest TocItem -> String
genToc forest = foldl genStr' "" forest
  where genStr' :: String -> Tree TocItem -> String
        genStr' str (Node root@(TocItem level ident text) subForest) =
          let nest = case subForest of
                       [] -> genToc subForest
                       _ -> "<ul>" ++ (genToc subForest) ++ "</ul>"
          in str ++ "<li><a href='#" ++ ident ++ "'>" ++ text ++ "</a>" ++ nest ++ "</li>"

tableOfContents :: [TocItem] -> (Block -> Block)
tableOfContents [] = (\x -> x)
tableOfContents headers = tocInsert
  where tocInsert :: Block -> Block
        tocInsert bl@(BulletList (( (( Plain ((Str "toc"):_)):_)):_)) =
          (RawBlock "html") . (\list -> "<ul id='markdown-toc'>" ++ list ++ "</ul>") .
          genToc . tocTree . filter (\(TocItem level _ _) -> level <= 2) . normalizeTocs $ headers
        tocInsert x = x

-- doesn't work with raw html <img> tag since that's just RawInline
linkImages :: Inline -> Inline
linkImages img@(Image inlines target) = (Link [img] target)
linkImages x = x

pygments :: Block -> Block
pygments (CodeBlock (_, _, namevals) contents) =
  let lang = case lookup "lang" namevals of
               Just lang_ -> lang_
               Nothing -> "text"
      text = case lookup "text" namevals of
               Just text_ -> text_
               Nothing -> ""
      colored = pygmentize lang contents
      codeHtml = numberedCode colored lang
      caption = if text /= ""
                  then renderHtml $ figcaption $ span $ toHtml text
                  else ""
      composed = renderHtml $ figure ! class_ "code" $ preEscapedToHtml $ codeHtml ++ caption
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
  let process = (shell ("pygmentize -f html -l " ++ lang ++ " -P encoding=utf-8")) {
                  std_in = CreatePipe, std_out = CreatePipe, close_fds = True}
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
