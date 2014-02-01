{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler, pandocFeedCompiler) where

import Hakyll.Web.Pandoc hiding (pandocCompiler)
import Hakyll.Core.Metadata (getMetadataField)

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)

import qualified Data.Set as S
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Core.Util.String
import System.IO.Unsafe -- for Pygments
import Data.List hiding (span)
import Data.Function (on)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U8

import Data.Tree
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import Text.Regex.TDFA ((=~))
import qualified Data.Map as Map

import System.IO (Handle, hFlush)

abbreviationCollector :: Item String -> Map.Map String String
abbreviationCollector item =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = (itemBody item) =~ pat :: [[String]]
      definitions = map (\(_:abbr:definition:_) -> (abbr, definition)) found
  in Map.fromList definitions

pandocFeedCompiler :: Item String -> Compiler (Item String)
pandocFeedCompiler = pandocTransformer readerOptions writerOptions' (walk tocRemover)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: (Handle, Handle) -> Item String -> Compiler (Item String)
pandocCompiler handles item = do
  alignmentM <- getMetadataField (itemIdentifier item) "toc"

  let abbrs = abbreviationCollector item
      alignment = fromMaybe "right" alignmentM
      tableOfContents' ast =
        if alignment == "off"
          then walk noTocHeaders ast
          else let headers = query collectHeaders ast
               in walk (tableOfContents headers alignment) ast
      transformer =
        (walk quoteRulers) .
        (walk $ abbreviations abbrs) .
        (walk $ pygments handles) .
        tableOfContents'

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

quoteRulers :: Block -> Block
quoteRulers (BlockQuote contents) =
  BlockQuote $ [HorizontalRule, padded, HorizontalRule]
  where padded = Div ("", ["padded-quote"], []) $ contents
quoteRulers x = x

abbreviations :: Map.Map String String -> Block -> Block
abbreviations abbrs (Para inlines)  = Para  $ walk (substituteAbbreviation abbrs) inlines
abbreviations abbrs (Plain inlines) = Plain $ walk (substituteAbbreviation abbrs) inlines
abbreviations _ x = x

substituteAbbreviation :: Map.Map String String -> Inline -> Inline
substituteAbbreviation abbrs (Str content) =
  case findMatch (Map.keys abbrs) content of
    Just abbr -> replaceWithAbbr content abbr
    Nothing -> Str content
  where findMatch (key:keys) text = if (text =~ key :: Bool) then Just key else findMatch keys text
        findMatch [] _ = Nothing
        replaceWithAbbr string abbr =
          let definition = (fromMaybe "ERROR" $ Map.lookup abbr abbrs)
              replacement = const $ renderHtml $ H.abbr ! A.title (H.toValue definition) $ preEscapedToHtml abbr
          in RawInline "html" $ replaceAll abbr replacement string
substituteAbbreviation _ x = x

tocRemover :: Block -> Block
tocRemover (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
tocRemover x = x

pygments :: (Handle, Handle) -> Block -> Block
pygments handles (CodeBlock (_, classes, keyvals) contents) =
  let lang = fromMaybe (if not . null $ classes then head classes else "text") $ lookup "lang" keyvals
      colored = renderHtml $ H.pre $ H.code ! A.class_ (H.toValue $ "highlight language-" ++ lang) $ do
                  preEscapedToHtml $ U8.toString $ pygmentize (U8.fromString lang) (U8.fromString contents) handles
      caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) (lookup "text" keyvals)
      composed = renderHtml $ H.figure ! A.class_ "codeblock" $ do
                   preEscapedToHtml $ colored ++ caption
  in RawBlock "html" composed
pygments _ x = x

-- REQUEST:  LANG\nLENGTH\nCODE
-- RESPONSE: LENGTH\nRESPONSE
pygmentize :: C.ByteString -> C.ByteString -> (Handle, Handle) -> C.ByteString
pygmentize lang contents (pin, pout) = unsafePerformIO $ do
  let len = C.pack . show . C.length $ contents
      request = C.intercalate "\n" [lang, len, contents]

  C.hPutStr pin request >> hFlush pin
  read . C.unpack <$> C.hGetLine pout >>= C.hGet pout

readerOptions :: ReaderOptions
readerOptions =
  let extensions = S.fromList [
        Ext_tex_math_dollars,
        Ext_abbreviations
        ]
  in def {
    readerSmart = True,
    readerExtensions = S.union extensions (writerExtensions def)
    }

writerOptions :: WriterOptions
writerOptions = def {
  writerHTMLMathMethod = MathJax "",
  writerHighlight = False,
  writerHtml5 = True }

