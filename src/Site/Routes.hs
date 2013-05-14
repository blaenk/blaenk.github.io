module Site.Routes (
  nicePostRoute,
  nicePageRoute,
  slugify
) where

import Hakyll
import System.FilePath

import Data.Char
import Data.List

-- from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
-- avoid </> because it uses os-dependent separator even though this is for a url

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
