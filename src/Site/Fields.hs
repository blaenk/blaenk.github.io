{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Site.Fields (
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
import Data.List (sortBy, groupBy, intersperse)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Control.Monad (liftM, forM)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Calendar

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import Site.Routes

sluggedTagsField :: String
                 -> Tags
                 -> Context a
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
  , iconTag "icon"
  , pushJS preview "pushJS"
  , defaultCtx
  ]

archiveCtx :: Pattern -> Context String
archiveCtx pat = mconcat
  [ field "archives" (\_ -> yearArchives pat) :: Context String
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

iconTag :: String -> Context String
iconTag key = field key $ \item -> do
  fmap (maybe "icon-chevron-right" ("icon-" ++)) $ getMetadataField (itemIdentifier item) "icon"

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

gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
  unsafeCompiler $ do
    sha <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%h", fp] ""
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s", fp] ""
    return $ "<a href='https://github.com/blaenk/blaenk.github.io/commits/source/" ++ fp ++ "'>History</a>" ++
             "<span class='hash'>, <a href='https://github.com/blaenk/blaenk.github.io/commit/" ++ sha ++
             "' title='" ++ message ++ "'>" ++ sha ++ "</a></span>"

yearArchives :: Pattern -> Compiler String
yearArchives pat = do
    thisYear <- unsafeCompiler . fmap yearFromUTC $ getCurrentTime
    posts    <- groupByYear =<< loadAll (pat .&&. hasNoVersion) :: Compiler [(Integer, [Item String])]
    itemTpl  <- loadBody "templates/index-post.html" :: Compiler Template
    archiveTpl <- loadBody "templates/archive.html" :: Compiler Template
    list     <- mapM (genArchives itemTpl archiveTpl thisYear) posts :: Compiler [String]
    return $ concat list :: Compiler String
    where genArchives :: Template -> Template -> Integer -> (Integer, [Item String]) -> Compiler String
          genArchives itemTpl archiveTpl curYear (year, posts) = do
            templatedPosts <- applyTemplateList itemTpl (postCtx False) posts :: Compiler String
            let yearCtx :: Context String
                yearCtx = if curYear == year
                          then constField "year" ""
                          else constField "year" ("<h2 style='margin-bottom: 15px'>" ++ show year ++ "</h2>")
                ctx' :: Context String
                ctx' = mconcat [ yearCtx
                               , constField "posts" templatedPosts
                               , missingField
                               ]
            itm <- makeItem "" :: Compiler (Item String)
            gend <- applyTemplate archiveTpl ctx' itm :: Compiler (Item String)
            return $ itemBody gend

groupByYear :: (MonadMetadata m, Functor m) => [Item a] -> m [(Integer, [Item a])]
groupByYear items =
    groupByYearM . fmap reverse . sortByM (getItemUTC defaultTimeLocale . itemIdentifier) $ items
  where
    sortByM :: (Monad m) => (a -> m UTCTime) -> [a] -> m [(Integer, a)]
    sortByM f xs = -- sort the list comparing the UTCTime
                   liftM (map (\(utc, post) -> (yearFromUTC utc, post)) . sortBy (comparing fst)) $
                   -- get them in a tuple of Item [(UTCTime, Item a)]
                   mapM (\x -> liftM (,x) (f x)) xs

    groupByYearM :: (Monad m) => m [(Integer, a)] -> m [(Integer, [a])]
    groupByYearM xs = liftM (map mapper . groupBy f) xs
      where f a b = (fst a) == (fst b)
            mapper [] = error "what"
            mapper posts@((year, _):_) = (year, (map snd posts))

yearFromUTC :: UTCTime -> Integer
yearFromUTC utcTime = let (year, _, _) = toGregorian $ utctDay utcTime in year

