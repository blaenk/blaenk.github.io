module Site.Routes (
  nicePostRoute,
  nicePageRoute
) where

import Hakyll
import System.FilePath

-- from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
-- avoid </> because it uses os-dependent separator even though this is for a url
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
