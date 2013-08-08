module Site.Filters (
  abbreviationFilter,
  sassCompiler
) where

import Hakyll
import Text.Regex.TDFA ((=~))

import Site.ShellFilter

abbreviationFilter :: String -> Compiler String
abbreviationFilter input = return $ abbreviationReplace input

abbreviationReplace :: String -> String
abbreviationReplace body =
  let pat = "^\\*\\[(.+)\\]: (.+)$" :: String
      found = body =~ pat :: [[String]]
      definitions = map (\(_:def) -> def) found
      abbrFilter = foldr traverseDoc id definitions
  in abbrFilter body
  where replaceAbbrev abbr def = (replaceAll abbr $ const ("<abbr title='" ++ def ++ "'>" ++ abbr ++ "</abbr>"))
        traverseDoc (abbr:def:_) acc = (replaceAbbrev abbr def) .
                                       (replaceAll ("\\*\\[" ++ abbr ++ "\\]: " ++ def ++ "\n?") $ const "") . acc
        traverseDoc _ acc = (++ "ERROR: abbreviation substitution") . acc

-- TODO: preferably use patterns/rules here
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "scss/screen.scss")
                 >>= makeItem
                 >>= withItemBody (shellFilter "sass -s --scss -I provider/scss/ --cache-location generated/scss")
