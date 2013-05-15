{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler) where

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

import Text.Blaze.Html (preEscapedToHtml, (!), toHtml, toValue)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative((<$>))

import Data.Tree
import Data.Ord

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocTransformer readerOptions writerOptions transformer
  where transformer = (bottomUp pygments) . tocTransformer
        tocTransformer ast = let headers = queryWith queryHeaders ast
                             in bottomUp (tableOfContents headers) ast

-- this compiler reads the item instead of the resourceBody, allowing
-- it to be preceded by a filter (e.g. abbreviationFilter)
pandocTransformer :: ReaderOptions -> WriterOptions -> (Pandoc -> Pandoc) -> Item String -> Compiler (Item String)
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
          (RawBlock "html") . (\list -> "<ul id='toc'>" ++ list ++ "</ul>") .
          (genToc "1") . tocTree . normalizeTocs $ headers
        tocInsert x = x

-- doesn't work with raw html <img> tag since that's just RawInline
_linkImages :: Inline -> Inline
_linkImages img@(Image _inlines target) = (Link [img] target)
_linkImages x = x

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
                  then renderHtml $ H.figcaption $ H.span $ H.toHtml text
                  else ""
      composed = renderHtml $ H.figure ! A.class_ "code" $ preEscapedToHtml $ codeHtml ++ caption
  in RawBlock "html" composed
pygments x = x

numberedCode :: String -> String -> String
numberedCode codeHtml lang =
  let codeLines = lines $ extractCode codeHtml
      wrappedCode = unlines $ wrapCodeLines codeLines
      numbers = unlines $ numberLines codeLines
  in renderHtml $ do
      H.div ! A.class_ "highlight" $ do
        H.table $ H.tr $ do
          H.td ! A.class_ "gutter" $ do
            H.pre ! A.class_ "line-numbers" $ do
              preEscapedToHtml numbers
          H.td ! A.class_ "code" $ do
            H.pre $ do
              H.code ! A.class_ (toValue lang) $ do
                preEscapedToHtml wrappedCode
  where wrapCodeLines codeLines = map wrapCodeLine codeLines
          where wrapCodeLine line = renderHtml $ H.span ! A.class_ "line" $ preEscapedToHtml line
        numberLines codeLines =
          let (_, res) = mapAccumL numberLine 1 codeLines
          in res
            where numberLine :: Integer -> String -> (Integer, String)
                  numberLine num _ = (num + 1, renderHtml $ H.span ! A.class_ "line-number" $ toHtml $ show num)

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

