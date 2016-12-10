{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleContexts #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, filterM, void, forM_)
import Data.Monoid (mappend, mconcat)
import Data.List (isInfixOf, isPrefixOf, intersperse, intercalate,
                  sortBy, reverse)
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Data.Generics (everywhereM, mkM)
import Text.Pandoc (Pandoc, Inline(..), HTMLMathMethod(..), WriterOptions(..),
                    ObfuscationMethod(..))
import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist,
                         getDirectoryContents, createDirectoryIfMissing,
                         renameFile, renameDirectory)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.FilePath ((</>), joinPath, splitDirectories, addExtension,
                        takeDirectory, dropExtension, takeBaseName,
                        replaceExtension, hasExtension)
import System.Process (system, rawSystem)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as B
import Text.Regex.Posix hiding (match)

import Hakyll hiding (teaserField)
import TikZ                     -- TikZ image rendering.


-- | Number of article teasers displayed per sorted index page.
--
articlesPerIndexPage :: Int
articlesPerIndexPage = 10


-- | Set up deployment command.
--
hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
  deployCommand =
     "rsync -ave ssh _site/ " ++
     "iross@www.skybluetrades.net:/srv/http/skybluetrades.net"
  }


-- | Main program: adds "publish" and "unpublish" options for managing
-- drafts.
--
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["publish", p]   -> publishDraft p
    ["unpublish", p] -> unpublishDraft p
    _                -> doHakyll


-- | Main Hakyll processing.
--
doHakyll = hakyllWith hakyllConf $ do
  -- Build tags and build post context allowing us to render a tag
  -- cloud.
  tags <- buildTags postsPattern (fromCapture "blog/tags/*.html")
  pctx <- postCtx tags

  -- Read templates.
  match "templates/*" $ compile templateCompiler

  -- Compress CSS files.
  match "css/*" $ do
    route $ setExtension "css"
    compile sass

  -- Copy JavaScript files.
  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Compile static pages.
  match ("static/*.markdown" .||. "static/**/*.markdown") $ do
    route $ gsubRoute "static/" (const "") `composeRoutes`
      setExtension ".html"
    compile staticCompiler

  -- Copy other static content.
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  -- Copy image files.
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Render blog posts.
  match "posts/*/*/*/*.markdown" $ do
    route   $ postsRoute `composeRoutes` setExtension ".html"
    compile $ postCompiler pctx
  match "posts/*/*/*/*/text.markdown" $ do
    route   $ postsRoute `composeRoutes`
      gsubRoute "text.markdown" (const "index.html")
    compile $ postCompiler pctx

  -- Copy resource files for blog posts.
  match "posts/*/*/*/*/*" $ do
    route   postsRoute
    compile copyFileCompiler

  -- Generate blog index pages: we need to split the articles, sorted
  -- by publication date, into groups for display across the right
  -- number of index pages.
  match "post-list" $ do
    route   postsRoute
    compile copyFileCompiler
  let pldep = [IdentifierDependency (fromFilePath "post-list")]
  mds <- getAllMetadata postsPattern
  let ids = reverse $ map fst $
            sortBy (compare `on` ((lookupString "published") . snd)) mds
      pids = chunk articlesPerIndexPage ids
      indexPages =
        map (\i -> fromFilePath $ "blog/index" ++
                   (if i == 1 then "" else show i) ++ ".html")
        [1..length pids]
      indexes = zip indexPages pids
      nindexes = length indexes
  rulesExtraDependencies pldep $ forM_ indexes $ \(idx, pages) ->
    create [idx] $ do
      route idRoute
      compile $ indexCompiler nindexes tags pctx pages

  -- Add a tag list compiler for every tag used in blog articles.
  tagsRules tags (makeTagList tags)

  -- Render RSS feed for blog.
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let froot = feedRoot feedConfiguration </> "blog"
          feedCtx = simplePostCtx `mappend`
                    rssBodyField froot "description"
      posts <- fmap (take 10) . recentFirst =<<
               loadAllSnapshots postsPattern "content"
      renderRss feedConfiguration feedCtx posts

  -- Render Haskell-only RSS feed for Planet Haskll.
  create ["haskell-rss.xml"] $ do
    route idRoute
    compile $ do
      let froot = feedRoot feedConfiguration </> "blog"
          feedCtx = simplePostCtx `mappend`
                    rssBodyField froot "description"
      posts <- fmap (take 10 . filter (not . null . itemBody)) . recentFirst =<<
               loadAllSnapshots postsPattern "haskell-content"
      renderRss feedConfiguration feedCtx posts

  where
    postsPattern = fromGlob "posts/*/*/*/*.markdown" .||.
                   fromGlob "posts/*/*/*/*/text.markdown"
    postsRoute = gsubRoute "posts/" (const "blog/posts/")


-- | Set up description field for RSS posts.
--
rssBodyField :: String -> String -> Context String
rssBodyField root key = field key $ \item -> do
  let dir = takeDirectory . toFilePath . itemIdentifier $ item
  return $ fixResourceUrls' (root </> dir) (itemBody item)


-- | Process SCSS or CSS.
--
sass :: Compiler (Item String)
sass = getResourceString >>=
       withItemBody (unixFilter "sass" ["-s", "--scss"]) >>=
       return . fmap compressCss


-- | Main post compiler: renders date field, adds tags, page title,
-- extracts teaser, applies templates.  This has to use a slightly
-- lower level approach than calling pageCompiler because it needs to
-- get at the raw Markdown source to pick out TikZ images.
--
postCompiler :: Context String -> Compiler (Item String)
postCompiler ctx = do
  p <- processTikZs >>= readPandocWith defaultHakyllReaderOptions
  p' <- unsafeCompiler $ fixPostLinks $ itemBody p
  let i = writePandocWith writeOptions (itemSetBody p' p)
  post <- saveSnapshot "post" i
    >>= loadAndApplyTemplate "templates/post.html" ctx
  ts <- getUnderlying >>= getTags
  saveSnapshot "haskell-content" $
    if "haskell" `elem` ts then post else post { itemBody = "" }
  saveSnapshot "content" post
    >>= loadAndApplyTemplates ["blog", "default"] ctx
    >>= relativizeUrls
    >>= doSpecials


-- | Process special post-processing steps.
--
doSpecials :: Item String -> Compiler (Item String)
doSpecials item = do
  md <- getMetadata $ itemIdentifier item
  case lookupString "specials" md of
    Nothing -> return item
    Just ss -> do
      let specials = map parseSpecial $ words ss
      return $ foldSpecials item specials
      where foldSpecials i [] = i
            foldSpecials i ((f,as):ss) = case f of
              "angular" -> foldSpecials (angularSpecial i as) ss
              _ -> foldSpecials i ss


-- | Special processing for articles using Angular.
--
angularSpecial :: Item String -> [String] -> Item String
angularSpecial i (app:injects) =
  itemSetBody (unlines . addScript . map xform . lines . itemBody $ i) i
  where xform l = if l =~ ("<html([^>]*)>" :: String)
                  then init l ++ " ng-app=\"" ++ app ++ "\">"
                  else l
        addScript ls = let (before, after) = break (=~ ("</body>" :: String)) ls
                       in before ++ ["<script>", script, "</script>"] ++ after
        script = "angular.module('" ++ app ++ "'" ++
                 if null injects then "" else (", " ++ head injects) ++ ");"


-- | Parse post-processing metadata.
--
parseSpecial :: String -> (String, [String])
parseSpecial s = (f, as)
  where (f, s') = span (/= '(') s
        s'' = drop 1 . take (length s' - 1) $ s'
        as = map trim $ commaSplit s''
        trim = takeWhile (not . isSpace) . dropWhile isSpace
        commaSplit s = case dropWhile (==',') s of
          "" -> []
          s' -> w : commaSplit s''
            where (w, s'') = break (==',') s'


-- | Fix up links to posts that use abbreviated forms
-- (e.g. yyyy/mm/dd/post-name or just post-name).
--
fixPostLinks :: Pandoc -> IO Pandoc
fixPostLinks = everywhereM (mkM fixPostLink)
fixPostLink l@(Link as inl (url, title))
  | url =~ ("^[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]/[^/]+$" :: String) = do
    let mdd = "posts" </> url
        mdf = addExtension mdd "markdown"
    fexist <- doesFileExist mdf
    dexist <- doesDirectoryExist mdd
    case (fexist, dexist) of
      (True, _) ->
        return $ Link as inl (addExtension ("/blog/posts" </> url) "html", title)
      (_, True) ->
        return $ Link as inl ("/blog/posts" </> url </> "index.html", title)
      (_, _) -> return l
  | url =~ ("^[^/]+$" :: String) = do
    mp <- findPosts url
    case mp of
      Nothing -> return l
      Just p -> let url' = if hasExtension p
                           then replaceExtension ("/blog" </> p) "html"
                           else "/blog" </> p </> "index.html"
                in return $ Link as inl (url', title)
  | otherwise = return l
fixPostLink x = return x


-- | Locate post Markdown files or directories by name.
--
findPosts :: FilePath -> IO (Maybe FilePath)
findPosts f = do
  days <- dir "posts" >>= dirs >>= dirs
  let ds = map (</> f) days
      fs = map (\f -> addExtension f "markdown") ds
  existds <- filterM doesDirectoryExist ds
  existfs <- filterM doesFileExist fs
  case (existds, existfs) of
    ([], []) -> return Nothing
    (d:_, _) -> return $ Just d
    (_, f:_) -> return $ Just f
  where notdot i = i /= "." && i /= ".."
        dir d = map (d </>) <$> filter notdot <$> getDirectoryContents d
        dirs ds = concat <$> mapM dir ds


-- | Full context for posts.
--
postCtx :: MonadMetadata m => Tags -> m (Context String)
postCtx t = do
  return $
    mapContext prettify
      (tagsFieldWith getTags render join "prettytags" t) `mappend`
    tagCloudCtx t `mappend`
    functionField "teaser" teaserField `mappend`
    functionField "readmore" readMoreField `mappend`
    functionField "pagetitle" pageTitle `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
  where prettify "" = ""
        prettify s = "<div class=\"tags\">" ++ s ++ "</div>"
        render _ Nothing = Nothing
        render tag (Just filePath) = Just $ H.span ! A.class_ "tag" $
          H.a ! A.href (toValue $ toUrl filePath) $ H.toHtml tag
        join = mconcat . intersperse " "
        pageTitle _ i = do
          m <- getMetadata $ itemIdentifier i
          return $ "Sky Blue Trades | " ++ (fromMaybe "" $ lookupString "title" m)



-- | Simplified context for posts.
--
simplePostCtx :: Context String
simplePostCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext


-- | Tag cloud context.
--
tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudFieldWith "tagcloud" makeLink (intercalate " ") 100 200
  where
    makeLink minSize maxSize tag url count min' max' = renderHtml $
        H.span ! A.class_ "tagcloud" !
        A.style (toValue $ "font-size: " ++ size count min' max') $
        H.a ! A.href (toValue url) $ H.toHtml tag
      where
        -- Show the relative size of one 'count' in percent
        size count min' max' =
          let diff = 1 + fromIntegral max' - fromIntegral min'
              relative = (fromIntegral count - fromIntegral min') / diff
              size' = floor $ minSize + relative * (maxSize - minSize)
          in show (size' :: Int) ++ "%"


-- | Static page compiler: renders date field, adds tags, page title,
-- extracts teaser, applies templates.  This has to use a slightly
-- lower level approach than calling pageCompiler because it needs to
-- get at the raw Markdown source to pick out TikZ images.
--
staticCompiler :: Compiler (Item String)
staticCompiler = pandocCompiler
  >>= loadAndApplyTemplate "templates/default.html" staticCtx
  >>= relativizeUrls
  where staticCtx = constField "pagetitle" "Sky Blue Trades" `mappend`
                    defaultContext


-- | Pandoc writer options.
--
writeOptions :: WriterOptions
writeOptions = defaultHakyllWriterOptions
    { writerEmailObfuscation = NoObfuscation,
      writerHTMLMathMethod   = MathML Nothing }


-- | Auxiliary compiler: generate a post list from a list of given
-- posts, using a particular context.
--
postList :: Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Context String
         -> String
         -> Compiler String
postList pattern preprocess' ctx tmpl = do
  postItemTpl <- loadBody $ fromFilePath $ "templates/" ++ tmpl ++ ".html"
  posts <- loadAll pattern
  processed <- preprocess' posts
  applyTemplateList postItemTpl ctx processed


-- | Auxiliary compiler: set up a tag list page.
--
makeTagList :: Tags -> String -> Pattern -> Rules ()
makeTagList tags tag pattern = do
  let title = "Posts tagged &#8216;" ++ tag ++ "&#8217;"
      pagetitle = "Sky Blue Trades | Tagged &#8216;" ++ tag ++ "&#8217;"
  route idRoute
  compile $ do
    list <- postList pattern recentFirst simplePostCtx "tagitem"
    makeItem ""
      >>= loadAndApplyTemplates ["tags", "blog", "default"]
           (constField "title" title `mappend`
            constField "pagetitle" pagetitle `mappend`
            constField "posts" list `mappend`
            tagCloudCtx tags `mappend`
            defaultContext)
      >>= relativizeUrls


-- | Index page compiler: generate a single index page based on
-- identifier name, with the appropriate posts on each one.
--
indexCompiler :: Int -> Tags -> Context String -> [Identifier]
              -> Compiler (Item String)
indexCompiler n tags ctx ids = do
  pg <- (drop 5 . dropExtension . takeBaseName . toFilePath) <$> getUnderlying
  let i = if pg == "" then 1 else (read pg :: Int)
      older = indexNavLink i 1 n
      newer = indexNavLink i (-1) n
  list <- postList (fromList ids) recentFirst ctx "postitem"
  makeItem ""
    >>= loadAndApplyTemplates ["index", "blog", "default"]
         (constField "title" "Sky Blue Trades" `mappend`
          constField "pagetitle" "Sky Blue Trades" `mappend`
          constField "posts" list `mappend`
          constField "navlinkolder" older `mappend`
          constField "navlinknewer" newer `mappend`
          tagCloudCtx tags `mappend`
          defaultContext)
    >>= relativizeUrls


-- | Generate navigation link HTML for stepping between index pages.
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $
                   (H.preEscapedToMarkup lab)
        lab :: String
        lab = if (d > 0) then "&laquo; OLDER POSTS" else "NEWER POSTS &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "/blog/index.html"
                    _ -> "/blog/index" ++ (show $ n + d) ++ ".html"


-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Sky Blue Trades RSS feed."
    , feedDescription = "RSS feed for the Sky Blue Trades blog."
    , feedAuthorName  = "Ian Ross"
    , feedAuthorEmail = "ian@skybluetrades.net"
    , feedRoot        = "http://www.skybluetrades.net"
    }


-- | Turns body of the page into the teaser: anything up to the
-- <!--MORE--> mark is the teaser, except for text between the
-- <!--NOTEASERBEGIN--> and <!--NOTEASEREND--> marks (useful for
-- keeping images out of teasers).
--
teaserField :: [String] -> Item String -> Compiler String
teaserField _ i = do
  let url = itemIdentifier i
  b <- itemBody<$> loadSnapshot url "post"
  return $ fixResourceUrls' (takeDirectory $ toFilePath url) (extractTeaser b)
    where
      extractTeaser = unlines . (noTeaser . extractTeaser') . lines
      extractTeaser' = takeWhile (/= "<!--MORE-->")

      noTeaser [] = []
      noTeaser ("<!--NOTEASERBEGIN-->" : xs) =
        drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
      noTeaser (x : xs) = x : (noTeaser xs)


-- | Generate "Read more" link for an index entry.
--
readMoreField :: [String] -> Item String -> Compiler String
readMoreField _ i = do
  rte <- getRoute $ itemIdentifier i
  return $ case rte of
    Nothing -> ""
    Just r -> if isInfixOf "<!--MORE-->" (itemBody i)
              then readMoreLink r
              else ""
    where readMoreLink r' =
            renderHtml $ H.div ! A.class_ "readmore" $
            H.a ! A.href (toValue $ "/" ++ r') $
            H.preEscapedToMarkup ("Read more &raquo;"::String)


-- | Fix up resource URLs for index page teasers and RSS feed entries.
--
fixResourceUrls' :: String -> String -> String
fixResourceUrls' path =
  withUrls (\x -> if '/' `elem` x then x else path ++ "/" ++ x)


-- | Publishing a draft:
--
--  1. Determine whether the path to be published exists and whether
--     it's a single file or a directory.
--
--  2. Make sure that the posts/YYYY/MM/DD directory exists for today.
--
--  3. Move the draft article over to the relevant posts
--     sub-directory.
--
--  4. Update the modification time of the moved post to the current
--     time.
--
publishDraft :: String -> IO ()
publishDraft path = do
  fExist <- doesFileExist path
  dExist <- doesDirectoryExist path
  if (not fExist && not dExist)
    then error $ "Neither file nor directory exists: " ++ path
    else do
      postDir <- todaysPostDir
      createDirectoryIfMissing True postDir
      let postPath = joinPath [postDir, last $ splitDirectories path]
      if fExist
        then renameFile path postPath
        else do
        putStrLn (path ++ " -> " ++ postPath)
        renameDirectory path postPath
      err <- rawSystem "touch" [postPath]
      addTimestamp postPath
      updatePostList
      putStrLn $ "Published to " ++ postPath


-- | Unpublishing a draft:
--
unpublishDraft :: String -> IO ()
unpublishDraft path = do
  fExist <- doesFileExist path
  dExist <- doesDirectoryExist path
  if (not fExist && not dExist)
    then error $ "Neither file nor directory exists: " ++ path
    else do
      let unpubDir = "drafts/unpublished"
      createDirectoryIfMissing True unpubDir
      let unpubPath = joinPath [unpubDir, last $ splitDirectories path]
      if fExist
        then renameFile path unpubPath
        else do
        putStrLn (path ++ " -> " ++ unpubPath)
        renameDirectory path unpubPath
      removeTimestamp unpubPath
      updatePostList
      putStrLn $ "Unpublished to " ++ unpubPath


-- | Add a timestamp as metadata for ordering purposes.
--
addTimestamp :: String -> IO ()
addTimestamp postPath = do
  fExist <- doesFileExist postPath
  let modFile = if fExist then postPath else postPath ++ "/text.markdown"
  putStrLn ("Editing " ++ modFile)
  pg <- B.readFile modFile
  t <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
--  utct <- getCurrentTime
--  tz <- getCurrentTimeZone
--  let t = utcToLocalTime tz utct
  let ts = formatTime defaultTimeLocale "%F %T" t
      pg' = addTimestamp' ts (B.unpack pg)
  B.writeFile modFile $ B.pack $ pg'
    where addTimestamp' :: String -> String -> String
          addTimestamp' ts i = init $ unlines ["---", md, "---", body]
            where (m, b) = span (/= "---") . tail . lines $ i
                  md = init $ unlines $ m ++ ["published: " ++ ts]
                  body = init $ unlines $ tail b


-- | Remove a timestamp from metadata.
--
removeTimestamp :: String -> IO ()
removeTimestamp postPath = do
  fExist <- doesFileExist postPath
  let modFile = if fExist then postPath else postPath ++ "/text.markdown"
  putStrLn ("Editing " ++ modFile)
  pg <- B.readFile modFile
  B.writeFile modFile $ B.pack $ removeTimestamp' $ B.unpack pg
    where removeTimestamp' :: String -> String
          removeTimestamp' i = init $ unlines ["---", md, "---", body]
            where (m, b) = span (/= "---") . tail . lines $ i
                  md = init $ unlines $
                       filter (not . ("published: " `isPrefixOf`)) m
                  body = init $ unlines $ tail b


-- | Update post list file (used for managing index dependencies).
--
updatePostList :: IO ()
updatePostList = void $ system "find posts -name \\*.markdown > post-list"


-- | Utility function to generate path to today's posts directory.
--
todaysPostDir :: IO FilePath
todaysPostDir = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay t
  return $ joinPath ["posts", show y, show0 m, show0 d]
  where show0 n = (if n < 10 then "0" else "") ++ show n


-- | String together multiple template compilers.
--
loadAndApplyTemplates :: [String] -> Context String ->
                         Item String -> Compiler (Item String)
loadAndApplyTemplates [c] ctx i =
  loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
loadAndApplyTemplates (c:cs) ctx i = do
  i' <- loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
  loadAndApplyTemplates cs ctx i'


-- | Split list into equal sized sublists.
--
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
