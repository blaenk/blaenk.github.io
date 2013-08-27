module Site.Routes (
  niceRoute,
  niceTags,
  slugify
) where

import Hakyll
import System.FilePath

import Data.Char
import Data.List

import Control.Monad (forM_)

niceTags :: Tags -> (String -> Pattern -> Rules ()) -> Rules ()
niceTags tags rules =
  forM_ (tagsMap tags) $ \(tag, identifiers) ->
    create [tagsMakeId tags $ slugify tag] $
      rulesExtraDependencies [tagsDependency tags] $
        rules tag $ fromList identifiers

slugify :: String -> String
slugify = intercalate "-" . words . map (\x -> if x `elem` allowedChars then toLower x else ' ')
  where allowedChars = (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

niceRoute :: String -> Routes
niceRoute prefix = customRoute $ \ident -> prefix ++ (takeBaseName . toFilePath $ ident) ++ "/index.html"
