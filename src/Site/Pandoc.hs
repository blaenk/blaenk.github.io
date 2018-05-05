{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (
  pandocFeedCompiler,
  pandocCompiler,
  readerOptions,
  writerOptions
) where

import Site.Types
import Site.Pygments
import Site.TableOfContents

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Data.Maybe (fromMaybe)

import Hakyll hiding (pandocCompiler)

import qualified Data.Set as Set

import Data.List (find) -- TODO: necessary?

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Regex.TDFA ((=~))
import Text.Regex (mkRegex, subRegex)
import qualified Data.Map as Map

import Control.Monad ((>=>))

pandocFeedCompiler :: Compiler (Item String)
pandocFeedCompiler =
  pandocCompilerWithTransform readerOptions writerOptions' (walk ignoreTOC . walk removeTOCMarker)
  where writerOptions' = writerOptions { writerHTMLMathMethod = PlainMath }

pandocCompiler :: Streams -> Compiler (Item String)
pandocCompiler streams = do
  alignment <- fromMaybe "right" <$> ((flip getMetadataField) "toc" =<< getUnderlying)
  abbrs <- abbreviationCollector <$> getResourceBody

  let transformer =
        return . abbreviations abbrs
        >=> return . codeBreak
        >=> pygments streams
        >=> return . tableOfContents alignment

  pandocCompilerWithTransformM readerOptions writerOptions transformer

codeBreak :: Pandoc -> Pandoc
codeBreak = walk breakChars

-- | This AST inserts a <http://en.wikipedia.org/wiki/Zero-width_space zero-width space>
-- before every matched delimiter.
--
-- I pattern match on the first two elements to avoid inserting it before the first two
-- characters. This is because regex-tdfa doesn't support negative lookaheads :(
breakChars :: Inline -> Inline
breakChars (Code attrs (x:y:xs)) = Code attrs $ x : y : subRegex pat xs "\x200b\&\\0"
  where pat = mkRegex "/|\\.|::|:|#|,|\\["
breakChars (Code attrs code) = Code attrs code
breakChars x = x

abbreviationCollector :: Item String -> Abbreviations
abbreviationCollector item =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = (itemBody item) =~ pat :: [[String]]
      definitions = map (\(_:abbr:definition:_) -> (abbr, (mkRegex abbr, definition))) found
  in Map.fromList definitions

abbreviations :: Abbreviations -> Pandoc -> Pandoc
abbreviations abbrs = walk (abbreviate abbrs)

abbreviate :: Abbreviations -> Block -> Block
abbreviate abbrs (Para inlines)  = Para  $ walk (substituteAbbreviation abbrs) inlines
abbreviate abbrs (Plain inlines) = Plain $ walk (substituteAbbreviation abbrs) inlines
abbreviate _ x = x

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

