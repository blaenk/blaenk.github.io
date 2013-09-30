{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler, pandocFeedCompiler) where

import Hakyll.Web.Pandoc hiding (pandocCompiler)
import Hakyll.Core.Metadata (getMetadataField)

import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk, query)

import qualified Data.Set as S
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Core.Util.String
import System.IO.Unsafe
import System.Process
import System.IO (hClose, hGetContents, hPutStr, hSetEncoding, localeEncoding)
import Control.Concurrent (forkIO)
import Control.Exception
import Data.List hiding (span)
import Data.Function (on)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Crypto.Hash
import Data.Tree
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import Text.Regex.TDFA ((=~))
import qualified Data.Map as Map

import System.Directory
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

abbreviationCollector :: Item String -> Map.Map String String
abbreviationCollector item =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = (itemBody item) =~ pat :: [[String]]
      definitions = map (\(_:abbr:definition:_) -> (abbr, definition)) found
  in Map.fromList definitions

pandocFeedCompiler :: Item String -> Compiler (Item String)
pandocFeedCompiler = pandocTransformer readerOptions writerOptions' (walk tocRemover)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: FilePath -> Item String -> Compiler (Item String)
pandocCompiler storePath item = do
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
        (walk $ pygments storePath) .
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
markupHeader (Node (Header _ (ident, _, _) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml (stringify inline)
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
quoteRulers (BlockQuote contents) = BlockQuote $ HorizontalRule : contents ++ [HorizontalRule]
quoteRulers x = x

-- add handler for Plain?
abbreviations :: Map.Map String String -> Block -> Block
abbreviations abbrs (Para inlines) = Para $ map substitute inlines
  where substitute (Str string) = case findMatch (Map.keys abbrs) string of
                                    Just abbr -> replaceWithAbbr string abbr
                                    Nothing -> Str string
        substitute x = x
        findMatch (key:keys) text = if (text =~ key :: Bool)
                                      then Just key
                                      else findMatch keys text
        findMatch [] _ = Nothing
        replaceWithAbbr string abbr =
          let definition = (fromMaybe "ERROR" $ Map.lookup abbr abbrs)
              replacement = const $ renderHtml $ H.abbr ! A.title (H.toValue definition) $ preEscapedToHtml abbr
          in RawInline "html" $ replaceAll abbr replacement string
abbreviations _ x = x

tocRemover :: Block -> Block
tocRemover (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
tocRemover x = x

cache :: String -> String -> FilePath -> String
cache code lang storePath = unsafePerformIO $ do
  let pathStem = (takeDirectory . takeDirectory $ storePath) ++ "/pygments/"

  _ <- createDirectoryIfMissing True pathStem

  let path = pathStem ++ "/" ++ newhash
      newhash = sha1 code

  readFile path `catch` handleExists path
  where cacheit path = do
          colored <- pygmentize lang code
          writeFile path colored
          return colored
        sha1 :: String -> String
        sha1 = show . sha1hash . C.pack
          where sha1hash = hash :: C.ByteString -> Digest SHA1
        handleExists :: FilePath -> IOError -> IO String
        handleExists path e
          | isDoesNotExistError e = cacheit path
          | otherwise = throwIO e

pygments :: FilePath -> Block -> Block
pygments storePath (CodeBlock (_, classes, namevals) contents) =
  let lang = case lookup "lang" namevals of
               Just language -> language
               Nothing -> if not . null $ classes
                            then head classes
                            else "text"
      text = lookup "text" namevals
      colored = renderHtml $ H.div ! A.class_ (H.toValue $ "code-container " ++ lang) $ do
                  preEscapedToHtml $ cache contents lang storePath
      caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) text
      composed = renderHtml $ H.figure ! A.class_ "code" $ do
                   preEscapedToHtml $ colored ++ caption
  in RawBlock "html" composed
pygments _ x = x

pygmentize :: String -> String -> IO String
pygmentize lang contents = do
  -- ,lineanchors=anchorident,anchorlinenos=True
  let process = (shell ("pygmentize -f html -l " ++ lang ++ " -O linenos=table -P encoding=utf-8")) {
                  std_in = CreatePipe, std_out = CreatePipe, close_fds = True}
      writer h input = do
        hSetEncoding h localeEncoding
        hPutStr h input
      reader h = do
        hSetEncoding h localeEncoding
        hGetContents h

  (Just stdin, Just stdout, _, _) <- createProcess process

  _ <- forkIO $ do
    writer stdin contents
    hClose stdin

  reader stdout

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

