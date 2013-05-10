--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
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
    match ("images/**" .||. "font/*" .||. "js/*") $ do
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

    match "pages/*" $ do
        route $ nicePageRoute
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    postCtx
            >>= loadAndApplyTemplate "templates/layout.html" postCtx

    create ["404.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/404.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout.html" archiveCtx

    create ["index.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout.html" archiveCtx

    match "templates/*" $ compile templateCompiler

nillaCtx :: Context String
nillaCtx = mconcat
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
  , nillaCtx
  ]

archiveCtx :: Context String
archiveCtx = mconcat
  [ field "posts" (\_ -> archivesList recentFirst)
  , constField "title" "Archives"
  , constField "commentsJS" ""
  , nillaCtx
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

--------------------------------------------------------------------------------
sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody (shellFilter "sass -s --scss -I scss/ --cache-location _cache/sass")

--------------------------------------------------------------------------------

