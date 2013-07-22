{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler, pandocFeedCompiler) where

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

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative((<$>))

import Data.Tree
import Data.Ord
import Data.Maybe (fromMaybe)

pandocFeedCompiler :: Item String -> Compiler (Item String)
pandocFeedCompiler = pandocTransformer readerOptions writerOptions' (bottomUp tocRemover)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocTransformer readerOptions writerOptions transformer
  where transformer = (bottomUp pygments) . tocTransformer
        tocTransformer ast = let headers = queryWith queryHeaders ast
                             in bottomUp (tableOfContents headers) ast

-- this compiler reads the item instead of the resourceBody, allowing
-- it to be preceded by a filter (e.g. abbreviationFilter)
pandocTransformer :: ReaderOptions
                  -> WriterOptions
                  -> (Pandoc -> Pandoc)
                  -> Item String
                  -> Compiler (Item String)
pandocTransformer ropt wopt f item =
  writePandocWith wopt . fmap f . readPandocWith ropt <$> (return $ item)

data TocItem = TocItem {
  tocLevel :: Int,
  _tocIdent :: String,
  _tocInline :: String
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
queryHeaders (Header level (ident, _, _) text) = [TocItem level ident (writeHtmlString def (Pandoc (Meta [] [] []) [(Plain text)]))]
queryHeaders _ = []

tocTree :: [TocItem] -> Forest TocItem
tocTree = map (\(x:xs) -> Node x (tocTree xs)) . groupBy (comp)
  where comp (TocItem a _ _) (TocItem b _ _) = a < b

genToc :: String -> Forest TocItem -> String
genToc iter forest = let (_, str) = foldl (genStr' iter) (1, "") forest in str
  where genStr' :: String -> (Integer, String) -> Tree TocItem -> (Integer, String)
        genStr' parent (current, str) (Node (TocItem _level ident text) sub) =
          let num = if parent == "1"
                    then show current ++ "."
                    else parent ++ show current ++ "."
              nest = case sub of
                       [] -> ""
                       _  -> "<ul>" ++ (genToc num sub) ++ "</ul>"
              out = str ++ "<li><span class='toc-section'>"  ++ num ++
                    "</span><a href='#" ++ ident ++ "'>" ++ text ++ "</a>" ++ nest ++ "</li>"
          in (current + 1, out)

tableOfContents :: [TocItem] -> (Block -> Block)
tableOfContents [] = (\x -> x)
tableOfContents headers = tocInsert
  where tocInsert :: Block -> Block
        tocInsert (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) =
          (RawBlock "html") . (\list -> "<ul id='toc' class='right-toc'>" ++ list ++ "</ul>") .
          (genToc "1") . tocTree . normalizeTocs $ headers
        tocInsert (BulletList (( (( Plain ((Str "toc-center"):_)):_)):_)) =
          (RawBlock "html") . (\list -> "<ul id='toc'>" ++ list ++ "</ul>") .
          (genToc "1") . tocTree . normalizeTocs $ headers
        tocInsert x = x

tocRemover :: Block -> Block
tocRemover (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
tocRemover x = x

pygments :: Block -> Block
pygments (CodeBlock (_, _, namevals) contents) =
  let lang = fromMaybe "text" $ lookup "lang" namevals
      text = fromMaybe "" $ lookup "text" namevals
      colored = renderHtml $ H.div ! A.class_ "code-container" $ do
                  preEscapedToHtml $ pygmentize lang contents
      caption = if text /= ""
                then renderHtml $ H.figcaption $ H.span $ preEscapedToHtml text
                else ""
      composed = renderHtml $ H.figure ! A.class_ "code" $ do
                   preEscapedToHtml $ colored ++ caption
  in RawBlock "html" composed
pygments x = x

pygmentize :: String -> String -> String
pygmentize lang contents = unsafePerformIO $ do
  -- ,lineanchors=anchorident,anchorlinenos=True
  let process = (shell ("pygmentize -f html -l " ++ lang ++ " -O linenos=table -P encoding=utf-8")) {
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
    -- writerTableOfContents = True,
    -- writerTOCDepth = 5,
    -- writerTemplate = "$toc$\n$body$",
    -- writerStandalone = True,
    writerHTMLMathMethod = MathJax "",
    writerExtensions = S.union extensions (writerExtensions def)
    }

