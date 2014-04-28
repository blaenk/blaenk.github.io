{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler, pandocFeedCompiler) where

import Site.Types

import Hakyll.Web.Pandoc hiding (pandocCompiler)
import Hakyll.Core.Metadata (getMetadataField)

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)

import qualified Data.Set as Set
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import System.IO.Unsafe -- for Pygments
import Data.List hiding (span)
import Data.Function (on)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))
import Control.Monad ((>=>))

import qualified System.IO.Streams as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U8

import Data.Tree
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>), mconcat)
import Text.Regex.TDFA ((=~))
import Text.Regex (mkRegex, subRegex)
import qualified Data.Map as Map

pandocFeedCompiler :: Item String -> Compiler (Item String)
pandocFeedCompiler = pandocTransformer readerOptions writerOptions' (walk tocRemover)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: Streams -> Item String -> Compiler (Item String)
pandocCompiler streams item = do
  alignmentM <- getMetadataField (itemIdentifier item) "toc"

  let abbrs = abbreviationCollector item
      alignment = fromMaybe "right" alignmentM
      tableOfContents' ast =
        if alignment == "off"
          then walk noTocHeaders ast
          else let headers = query collectHeaders ast
               in walk (tableOfContents headers alignment) ast
      transformer =
        tableOfContents' .
        (walk $ pygments streams) .
        (walk quotes) .
        (walk paths) .
        (walk $ abbreviations abbrs)

  pandocTransformer readerOptions writerOptions transformer item

pandocTransformer :: ReaderOptions
                  -> WriterOptions
                  -> (Pandoc -> Pandoc)
                  -> Item String
                  -> Compiler (Item String)
pandocTransformer ropt wopt f item =
  writePandocWith wopt . fmap f . readPandocWith ropt <$> (return $ item)

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

noTocHeaders :: Block -> Block
noTocHeaders (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
noTocHeaders (Header level (ident, classes, params) inline) =
  Header level (ident, "notoc" : classes, params) inline
noTocHeaders x = x

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Block -> H.Html
markupHeader (Node (Header _ (ident, _, keyvals) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where render x  = writeHtmlString writerOptions (Pandoc nullMeta [(Plain x)])
        section   = fromMaybe (render inline) (lookup "toc" keyvals)
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
markupHeader _ = error "what"

markupHeaders :: Forest Block -> H.Html
markupHeaders = mconcat . map markupHeader

createTable :: Forest Block -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    H.p "Contents"
    H.ol $ markupHeaders headers

tableOfContents :: [Block] -> String -> Block -> Block
tableOfContents [] _     x = x
tableOfContents headers alignment x@(BulletList (( (( Plain ((Str "toc"):_)):_)):_))
  | alignment == "right" = render . (! A.class_ "right-toc") . table $ headers
  | alignment == "left"  = render . table $ headers
  | otherwise            = x
  where render = (RawBlock "html") . renderHtml
        table  = createTable . groupByHierarchy
tableOfContents _ _ x = x

quotes :: Block -> Block
quotes (BlockQuote contents) =
  BlockQuote $ [HorizontalRule, quote, HorizontalRule] ++ citation
  where
    (q, citation) = break isFooter contents
    quote = Div ("", ["padded-quote"], []) q
    isFooter x = case x of
                   RawBlock (Format "html") "<footer>" -> True
                   _ -> False
quotes x = x

paths :: Inline -> Inline
paths (Code (_, ["path"], _) code) = Code ("", ["path"], []) $ subRegex pat code "\x200b\&\\0"
  where pat = mkRegex "/|\\.|::"
paths x = x

abbreviationCollector :: Item String -> Abbreviations
abbreviationCollector item =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = (itemBody item) =~ pat :: [[String]]
      definitions = map (\(_:abbr:definition:_) -> (abbr, (mkRegex abbr, definition))) found
  in Map.fromList definitions

abbreviations :: Abbreviations -> Block -> Block
abbreviations abbrs (Para inlines)  = Para  $ walk (substituteAbbreviation abbrs) inlines
abbreviations abbrs (Plain inlines) = Plain $ walk (substituteAbbreviation abbrs) inlines
abbreviations _ x = x

substituteAbbreviation :: Abbreviations -> Inline -> Inline
substituteAbbreviation abbrs (Str content) =
  case find (content =~) (Map.keys abbrs) of
    Just abbr -> replaceWithAbbr content abbr
    Nothing   -> Str content
  where replaceWithAbbr string abbr =
          let Just (pat, definition) = Map.lookup abbr abbrs
              replacement = renderHtml $ H.abbr ! A.title (H.toValue definition) $ preEscapedToHtml abbr
          in RawInline "html" $ subRegex pat string replacement
substituteAbbreviation _ x = x

tocRemover :: Block -> Block
tocRemover (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
tocRemover x = x

pygments :: Streams -> Block -> Block
pygments streams (CodeBlock (_, classes, keyvals) contents) =
  let lang = fromMaybe (if null classes then "text" else head classes) $ lookup "lang" keyvals
      code = if lang == "text"
               then renderHtml $ H.toHtml contents
               else pygmentize streams lang contents
      colored = renderHtml $ H.pre $ H.code ! A.class_ (H.toValue $ "highlight language-" ++ lang) $ do
                  preEscapedToHtml code
      caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) $ lookup "text" keyvals
      composed = renderHtml $ H.figure ! A.class_ "codeblock" $ do
                   preEscapedToHtml $ caption ++ colored
  in RawBlock "html" composed
pygments _ x = x

pygmentize :: Streams -> String -> String -> String
pygmentize (os, is) lang contents = unsafePerformIO $ do
  let lang'     = U8.fromString lang
      contents' = U8.fromString contents
      len       = C.pack . show . C.length $ contents'

      -- REQUEST:  LANG\nLENGTH\nCODE
      request = Just $ C.intercalate "\n" [lang', len, contents']
      flush   = Just ""

  mapM_ (flip S.write os) [request, flush]

  -- RESPONSE: LENGTH\nRESPONSE
  responseLength <- read . C.unpack . fromJust <$> (S.lines >=> S.read) is
  U8.toString <$> S.readExactly responseLength is

readerOptions :: ReaderOptions
readerOptions =
  let extensions = Set.fromList [
        Ext_tex_math_dollars,
        Ext_abbreviations
        ]
  in def {
    readerSmart = True,
    readerExtensions = Set.union extensions (writerExtensions def)
    }

writerOptions :: WriterOptions
writerOptions = def {
  writerHTMLMathMethod = MathJax "",
  writerHighlight = False,
  writerHtml5 = True }

