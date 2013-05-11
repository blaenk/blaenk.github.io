module Site.PandocCompiler (pandocCompiler) where

import Hakyll.Web.Pandoc hiding (pandocCompiler)

import Text.Pandoc

import qualified Data.Set as S
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import System.IO.Unsafe
import System.Process
import System.IO (hClose, hGetContents, hPutStr, hSetEncoding, localeEncoding)
import Control.Concurrent (forkIO)
import Data.List
import Text.Regex.TDFA

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
                  then "<figcaption><span>" ++ text ++ "</span></figcaption>"
                  else ""
      composed = "<figure class=\"code\">\n" ++ code ++ caption ++ "</figure>"
  in RawBlock "html" composed
pygments x = x

{-
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H

main :: IO ()
main = hakyll $ do
  create ["index.html"] $ do
    route idRoute
      compile $
        makeItem $ renderHtml $
          H.html $ do
            H.header $
              H.title "An example page"
            H.body $
              H.p "Hello world!" 
-}

numberedCode :: String -> String -> String
numberedCode code lang =
  let codeLines = lines $ extractCode code
      wrappedCode = unlines $ wrapCodeLines codeLines
      numbers = unlines $ numberLines codeLines
  in "<div class='highlight'><table><tr><td class='gutter'><pre class='line-numbers'>" ++
     numbers ++ "</pre></td><td class='code'><pre><code class='" ++ lang ++ "'>" ++ wrappedCode ++
     "</code></pre></td></tr></table></div>"
  where wrapCodeLines codeLines = map wrapCodeLine codeLines
          where wrapCodeLine line = "<span class='line'>" ++ line ++ "</span>"
        numberLines codeLines =
          let (_, res) = mapAccumL numberLine 1 codeLines
          in res
            where numberLine :: Integer -> String -> (Integer, String)
                  numberLine num _ = (num + 1, "<span class='line-number'>" ++ show num ++ "</span>")

extractCode :: String -> String
extractCode pygmentsResult =
  let preRegex = makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt "<pre>(.+)</pre>"
      -- extract [["haystack", "needle"]
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

