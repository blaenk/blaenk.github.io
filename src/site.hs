--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Control.Monad (forM_)
import Hakyll

import Site.ShellFilter
import Site.PandocCompiler

import System.FilePath
import System.Process

--------------------------------------------------------------------------------

myHakyllConf :: Configuration
myHakyllConf = defaultConfiguration

main :: IO ()
main = hakyllWith myHakyllConf $ do
    forM_ ["images/*", "font/*", "js/*"] $ \matched ->
      match matched $ do
          route   idRoute
          compile copyFileCompiler

    match "scss/screen.scss" $ do
        route   $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
        compile $ sassCompiler

    match "posts/*" $ do
        route $ nicePostRoute
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"   postCtx
            >>= loadAndApplyTemplate "templates/layout.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    match "pages/*" $ do
        route $ nicePageRoute
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    postCtx
            >>= loadAndApplyTemplate "templates/layout.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    create ["404.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/404.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout.html" archiveCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    create ["index.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout.html" archiveCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler

nillaCtx :: Context String
nillaCtx = mconcat
  [ bodyField "body"
  , metadataField
  , niceUrlField "url"
  , pathField "path"
  , titleField "title"
  , gitTag "git"
  , missingField
  ]

postCtx :: Context String
postCtx = mconcat
  [ dateField "datePost" "%B %e, %Y"
  , dateField "dateArchive" "%b %e"
  , commentsTag "comments"
  , nillaCtx
  ]

archiveCtx :: Context String
archiveCtx = mconcat
  [ field "posts" (\_ -> archivesList recentFirst)
  , constField "title" "Archives"
  , nillaCtx
  ]

-- url field without /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key $
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . itemIdentifier
  where removeIndexStr url = case splitFileName url of
          (dir, "index.html") -> dir
          _ -> url

-- gets passed the key and the item apparently
commentsTag :: String -> Context String
commentsTag key = field key $ \item -> do
    commentsMeta <- getMetadataField (itemIdentifier item) "comments"
    let comments = case commentsMeta of
                     Just "false" -> False
                     Just "off" -> False
                     _ -> True
    if comments
      then unsafeCompiler $ readFile "templates/comments.html"
      else return ""

gitTag :: String -> Context String
gitTag key = field key $ \_ -> do
  unsafeCompiler $ do
    sha <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%H"] []
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s"] []
    return ("<a href=\"https://github.com/" ++ sha ++ "/commit/" ++ sha ++
           "\" title=\"" ++ message ++"\">" ++ (take 8 sha) ++ "</a>")

--------------------------------------------------------------------------------
archivesList :: ([Item String] -> Compiler [Item String]) -> Compiler String
archivesList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/index-post.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

-- from http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
nicePageRoute :: Routes
nicePageRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory (takeDirectory p) </> takeBaseName p </> "index.html"
        where p = toFilePath ident

nicePostRoute :: Routes
nicePostRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath ident

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") -> dir
        _ -> url

--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody (shellFilter "sass -s --scss -I scss/ --cache-location _cache/sass")

--------------------------------------------------------------------------------

