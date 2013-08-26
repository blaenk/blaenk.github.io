{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (pandocCompiler, pandocFeedCompiler) where

import Hakyll.Web.Pandoc hiding (pandocCompiler)

import Text.Pandoc

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

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as C (ByteString, pack)
import Crypto.Hash
import Data.Tree
import Data.Ord
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import Text.Regex.TDFA ((=~))
import qualified Data.Map as Map

import System.Directory
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)

abbreviationCollector :: String -> Compiler (Map.Map String String)
abbreviationCollector body =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = body =~ pat :: [[String]]
      definitions = map (\(_:abbr:definition:_) -> (abbr, definition)) found
  in return $ Map.fromList definitions

pandocFeedCompiler :: Item String -> Compiler (Item String)
pandocFeedCompiler = pandocTransformer readerOptions writerOptions' (topDown tocRemover)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: FilePath -> Item String -> Compiler (Item String)
pandocCompiler storePath item = do
  abbrs <- withItemBody (abbreviationCollector) item
  pandocTransformer readerOptions writerOptions (transformer $ itemBody abbrs) item
  where transformer abbrs = (topDown quoteRulers) .
                            (topDown $ abbreviations abbrs) .
                            (topDown $ pygments storePath) .
                            tocTransformer
        tocTransformer ast = let headers = queryWith collectHeaders ast
                             in topDown (tableOfContents headers) ast

pandocTransformer :: ReaderOptions
                  -> WriterOptions
                  -> (Pandoc -> Pandoc)
                  -> Item String
                  -> Compiler (Item String)
pandocTransformer ropt wopt f item =
  writePandocWith wopt . fmap f . readPandocWith ropt <$> (return $ item)

data THeader = THeader { headerLevel :: Int, _headerId :: String, _headerText :: String }

instance Eq THeader where
  (THeader a _ _) == (THeader b _ _) = a == b

instance Ord THeader where
  compare = comparing headerLevel

normalizeHeaders :: [THeader] -> [THeader]
normalizeHeaders tocs = map normalize tocs
  where minLevel :: Int
        minLevel = subtract 1 . headerLevel . minimum $ tocs
        normalize item@(THeader level _ _) = item {headerLevel = level - minLevel}

collectHeaders :: Block -> [THeader]
collectHeaders (Header level (ident, _, _) text) =
  let inline = (writeHtmlString def (Pandoc (Meta [] [] []) [(Plain text)]))
  in [THeader level ident inline]
collectHeaders _ = []

groupByHierarchy :: [THeader] -> Forest THeader
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy (comp)
  where comp (THeader a _ _) (THeader b _ _) = a < b

markupHeader :: Tree THeader -> H.Html
markupHeader (Node (THeader _level ident text) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where link = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml text

markupHeaders :: Forest THeader -> H.Html
markupHeaders = mconcat . map markupHeader

createTable :: Forest THeader -> H.Html
createTable = (H.ol ! A.id "toc") . markupHeaders

tableOfContents :: [THeader] -> Block -> Block
tableOfContents [] x = x
tableOfContents headers x@(BulletList (( (( Plain ((Str alignment):_)):_)):_))
  | alignment == "toc"        = render . (! A.class_ "right-toc") . table $ headers
  | alignment == "toc-center" = render . table $ headers
  | otherwise                 = x
  where render = (RawBlock "html") . renderHtml
        table = createTable . groupByHierarchy . normalizeHeaders
tableOfContents _ x = x

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
pygments storePath (CodeBlock (_, _, namevals) contents) =
  let lang = fromMaybe "text" $ lookup "lang" namevals
      text = lookup "text" namevals
      colored = renderHtml $ H.div ! A.class_ "code-container" $ do
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
writerOptions = def { writerHTMLMathMethod = MathJax "" }

