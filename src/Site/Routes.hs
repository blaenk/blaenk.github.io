module Site.Routes (
  nicePostRoute,
  nicePageRoute,
  niceTags,
  slugify
) where

import Hakyll
import System.FilePath

import Data.Char
import Data.List

import Control.Monad (forM_)

-- from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
-- avoid </> because it uses os-dependent separator even though this is for a url

niceTags :: Tags -> (String -> Pattern -> Rules ()) -> Rules ()
niceTags tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        create [tagsMakeId tags $ slugify tag] $
            rulesExtraDependencies [tagsDependency tags] $
                rules tag $ fromList identifiers

slugify :: String -> String
slugify = intercalate "-" . words . map (\x -> if x `elem` allowedChars then toLower x else ' ')
  where allowedChars = (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

nicePostRoute :: Routes
nicePostRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      (takeDirectory p) ++ "/" ++ (takeBaseName p) ++ "/index.html"
        where p = toFilePath ident

nicePageRoute :: Routes
nicePageRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      (takeDirectory (takeDirectory p)) ++  "/" ++ (takeBaseName p) ++ "/index.html"
        where p = toFilePath ident
