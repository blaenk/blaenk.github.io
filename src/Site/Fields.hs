{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Site.Fields (
  defaultCtx,
  postCtx,
  archiveCtx,
  tagsCtx
) where

import Hakyll
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

sluggedTagsField :: String                             -- ^ Destination field
                 -> Tags                               -- ^ Tags structure
                 -> Context a                          -- ^ Resulting context
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
  , niceUrlField "url"
  , pathField "path"
  , gitTag "git"
  , constField "title" "Blaenk Denum"
  , constField "commentsJS" ""
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

--[ field "posts" (\_ -> archivesList recentFirst)
archiveCtx :: Pattern -> Context String
archiveCtx pat = mconcat
  [ field "archives" (\_ -> yearArchives pat) :: Context String
  , constField "title" "Archives"
  , constField "commentsJS" ""
  , defaultCtx
  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = sluggedTagsField "tags" tags

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

-- TODO: use Rules/Patterns and toFilePath to read the files instead

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
      then unsafeCompiler $ readFile "provider/templates/comments-js.html"
      else return ""

gitTag :: String -> Context String
gitTag key = field key $ \_ -> do
  unsafeCompiler $ do
    sha <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%H"] []
    message <- readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%s"] []
    return ("<a href=\"https://github.com/blaenk/blaenk.github.io/commit/" ++ sha ++
           "\" title=\"" ++ message ++ "\">" ++ (take 8 sha) ++ "</a>")

yearArchives :: Pattern -> Compiler String
yearArchives pat = do
    thisYear <- unsafeCompiler . fmap yearFromUTC $ getCurrentTime
    posts    <- groupByYear =<< loadAll pat    :: Compiler [(Integer, [Item String])]
    itemTpl  <- loadBody "templates/index-post.html" :: Compiler Template
    archiveTpl <- loadBody "templates/archive.html" :: Compiler Template
    list     <- mapM (genArchives itemTpl archiveTpl thisYear) posts :: Compiler [String]
    return $ concat list :: Compiler String
    where genArchives :: Template -> Template -> Integer -> (Integer, [Item String]) -> Compiler String
          genArchives itemTpl archiveTpl curYear (year, posts) = do
            templatedPosts <- applyTemplateList itemTpl postCtx posts :: Compiler String
            let yearCtx :: Context String
                yearCtx = if curYear == year
                          then constField "year" ""
                          else constField "year" ("<h2>" ++ show year ++ "</h2>")
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

