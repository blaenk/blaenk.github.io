{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Site.Contexts (
  defaultCtx,
  postCtx,
  archiveCtx,
  tagsCtx,
  sluggedTagsField,
  customTitleField
) where

import Hakyll hiding (titleField)
import Data.Monoid (mconcat)
import System.Process
import System.FilePath

-- for groupByYear
import Data.List (sortBy, groupBy, intersperse, intersperse)
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Control.Applicative ((<$>))
import System.Locale (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Calendar

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import Site.Routes

import Data.Function (on)

sluggedTagsField :: String -> Tags -> Context a
sluggedTagsField key tags = field key $ \item -> do
    tags' <- getTags $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags $ slugify tag
        return $ renderLink tag route'

    return $ renderHtml $ mconcat $ intersperse ", " $ catMaybes $ links
  where
    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl $ (takeDirectory filePath) ++ "/") $ toHtml tag

defaultCtx :: Context String
defaultCtx = mconcat
  [ bodyField "body"
  , metadataField
  , titleField
  , niceUrlField "url"
  , pathField "path"
  , constField "commentsJS" ""
  , constField "pushJS" ""
  , constField "title" "Blaenk Denum"
  , missingField
  ]

postCtx :: Bool -> Context String
postCtx preview = mconcat
  [ dateField "datePost" "%B %e, %Y"
  , dateField "dateArchive" "%b %e"
  , commentsTag "comments"
  , commentsJS "commentsJS"
  , gitTag "git"
  , socialTag "social"
  , pushJS preview "pushJS"
  , defaultCtx
  ]

archiveCtx :: Pattern -> Context String
archiveCtx pat = mconcat
  -- [ archives pat
  [ field "archives" (\_ -> yearlyArchives pat) :: Context String
  , constField "commentsJS" ""
  , defaultCtx
  ]

tagsCtx :: Pattern -> String -> Context String
tagsCtx pat tag = mconcat
  [ constField "tag" tag
  , customTitleField $ "Tagged: " ++ tag
  , archiveCtx pat
  ]

titleField :: Context String
titleField = field "pageTitle" $ \item -> do
  title <- getMetadataField (itemIdentifier item) "title"
  maybe (return "Blaenk Denum") (return . (++ " - Blaenk Denum")) title

customTitleField :: String -> Context String
customTitleField value = constField "pageTitle" $ value ++ " - Blaenk Denum"

-- url field without /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key niceItemUrl

niceItemUrl :: Item a -> Compiler String
niceItemUrl =
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . setVersion Nothing . itemIdentifier
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
      then unsafeCompiler $ readFile "provider/templates/comments.html"
      else return ""

commentsJS :: String -> Context String
commentsJS key = field key $ \item -> do
    comments <- commentsOn item
    if comments
      then do
          url <- niceItemUrl item
          tmpl <- loadBody "templates/comments-js.html"
          itm <- makeItem "" :: Compiler (Item String)
          gend <- applyTemplate tmpl (constField "url" url) itm
          return $ itemBody gend
      else return ""

pushOn :: (MonadMetadata m) => Item a -> m Bool
pushOn item = do
  pushMeta <- getMetadataField (itemIdentifier item) "push"
  return $ case pushMeta of
    Just "false" -> False
    Just "off" -> False
    _ -> True

pushJS :: Bool -> String -> Context String
pushJS preview key = field key $ \item -> do
  push <- pushOn item
  if preview && push
    then do
      path <- fmap toFilePath getUnderlying
      tmpl <- loadBody "templates/push-js.html"
      itm <- makeItem "" :: Compiler (Item String)
      gend <- applyTemplate tmpl (constField "path" path) itm
      return $ itemBody gend
    else return ""

socialTag :: String -> Context String
socialTag key = field key $ \item -> do
  let sites = [("hn", "HN"), ("reddit", "Reddit")]
      link name ln = H.a ! A.href (toValue ln) $ toHtml (name :: String)

  links <- fmap (intersperse ", " . catMaybes) . forM sites $ \(site, name) -> do
             fmap (link name) <$> getMetadataField (itemIdentifier item) site

  if null links
    then return ""
    else return .
         renderHtml $ H.div ! A.class_ "meta-component" $ do
                        H.i ! A.class_ "fa fa-comments-o fa-fw" $ ""
                        mconcat $ " " : links

gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
      gitLog format =
        readProcess "git" [
          "log"
        , "-1"
        , "HEAD"
        , "--pretty=format:" ++ format
        , fp
        ] ""

  unsafeCompiler $ do
    sha     <- gitLog "%h"
    message <- gitLog "%s"

    let history = "https://github.com/blaenk/blaenk.github.io/commits/source/" ++ fp
        commit  = "https://github.com/blaenk/blaenk.github.io/commit/" ++ sha

    return $ if null sha
               then "Not Committed"
               else renderHtml $ do
                      H.a ! A.href (toValue history) $ "History"
                      H.span ! A.class_ "hash" $ do
                        toHtml (", " :: String)
                        H.a ! A.href (toValue commit) ! A.title (toValue message) $ toHtml sha

groupedArchives :: Pattern -> Compiler [(Integer, [Item String])]
groupedArchives pat =
  map collect . groupBy ((==) `on` fst) . reverse . sortBy (compare `on` fst)
    <$> (mapM addYear =<< recentFirst =<< loadAll (pat .&&. hasNoVersion))
  where
    collect :: [(Integer, Item String)] -> (Integer, [Item String])
    collect [] = error "what"
    collect items@((year, _):_) = (year, (map snd items))

    addYear :: Item String -> Compiler (Integer, Item String)
    addYear item = do
      year <- yearFromUTC <$> (getItemUTC defaultTimeLocale . itemIdentifier $ item)
      return (year, item)

    yearFromUTC :: UTCTime -> Integer
    yearFromUTC utcTime = let (year, _, _) = toGregorian $ utctDay utcTime
                          in year

yearlyArchives :: Pattern -> Compiler String
yearlyArchives pat = do
  concat <$> (mapM generateArchive =<< groupedArchives pat)
  where generateArchive :: (Integer, [Item String]) -> Compiler String
        generateArchive (year, items) = do
          let ctx = mconcat [constField "year" (show year),
                             listField "items" (postCtx False) (return items),
                             missingField]

          archiveTmpl <- loadBody "templates/archive.html"
          archive <- makeItem ("" :: String)
          itemBody <$> applyTemplate archiveTmpl ctx archive

