{-# LANGUAGE OverloadedStrings #-}

module Site.Fields (
  defaultCtx,
  postCtx,
  archiveCtx,
  niceUrlField,
  gitTag,
  commentsOn,
  commentsJS,
  commentsTag
) where

import Hakyll
import Data.Monoid (mconcat)
import System.Process
import System.FilePath

defaultCtx :: Context String
defaultCtx = mconcat
  [ bodyField "body"
  , metadataField
  , niceUrlField "url"
  , pathField "path"
  , gitTag "git"
  , missingField
  ]

postCtx :: Context String
postCtx = mconcat
  [ dateField "datePost" "%B %e, %Y"
  , dateField "dateArchive" "%b %e"
  , commentsTag "comments"
  , commentsJS "commentsJS"
  , defaultCtx
  ]

archiveCtx :: Context String
archiveCtx = mconcat
  [ field "posts" (\_ -> archivesList recentFirst)
  , constField "title" "Archives"
  , constField "commentsJS" ""
  , defaultCtx
  ]

-- url field without /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key $
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . itemIdentifier
  where removeIndexStr url = case splitFileName url of
          (dir, "index.html") -> dir
          _ -> url

commentsOn :: (MonadMetadata m) => Item a -> m Bool
commentsOn item = do
  commentsMeta <- getMetadataField (itemIdentifier item) "comments"
  return $ case commentsMeta of
    Just "false" -> False
    Just "off" -> False
    _ -> True

-- gets passed the key and the item apparently
commentsTag :: String -> Context String
commentsTag key = field key $ \item -> do
    comments <- commentsOn item
    if comments
      then unsafeCompiler $ readFile "templates/comments.html"
      else return ""

commentsJS :: String -> Context String
commentsJS key = field key $ \item -> do
    comments <- commentsOn item
    if comments
      then unsafeCompiler $ readFile "templates/comments-js.html"
      else return ""

gitTag :: String -> Context String
gitTag key = field key $ \_ -> do
  unsafeCompiler $ do
    sha <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%H"] []
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s"] []
    return ("<a href=\"https://github.com/blaenk/hakyll/commit/" ++ sha ++
           "\" title=\"" ++ message ++"\">" ++ (take 8 sha) ++ "</a>")

--------------------------------------------------------------------------------
archivesList :: ([Item String] -> Compiler [Item String]) -> Compiler String
archivesList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/index-post.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

