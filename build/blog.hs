{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, Arrows #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, forM)
import Control.Arrow ((&&&))
import Data.Monoid (mappend, mconcat)
import Data.List (isInfixOf, intersperse, intercalate)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..),
                    ObfuscationMethod(..))
import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist,
                         createDirectoryIfMissing,
                         renameFile, renameDirectory)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (toGregorian)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime, formatTime)
import System.FilePath ((</>), joinPath, splitDirectories,
                        takeDirectory, takeExtension, replaceExtension)
import System.Cmd (rawSystem)
import Data.String.Utils (replace)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as B
import Debug.Trace (trace, traceShow)

-- We override some names from Hakyll so we can use a different post
-- naming convention.
-- import Hakyll hiding (chronological, renderDateField, renderDateFieldWith,
--                       renderTagsField, renderTagCloud,
--                       relativizeUrlsCompiler, relativizeUrls, withUrls)
import Hakyll

--import Overrides                -- Overrides of Hakyll functions.
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


-- | Main program: adds a "publish" option to copy a draft out to the
-- main posts area.
--
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["publish", p] -> publishDraft p
    _              -> doHakyll


-- | Main Hakyll processing.
--
doHakyll = hakyllWith hakyllConf $ do
  -- Build tags.
  tags <- buildTags postsPattern (fromCapture "blog/tags/*.html")

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
    compile $ postCompiler (postCtx tags)
  match "posts/*/*/*/*/text.markdown" $ do
    route   $ postsRoute `composeRoutes`
      gsubRoute "text.markdown" (const "index.html")
    compile $ postCompiler (postCtx tags)


  -- Copy resource files for blog posts.
  match "posts/*/*/*/*/*" $ do
    route   postsRoute
    compile copyFileCompiler


  -- Generate blog index pages: we need to calculate and pass
  -- through the total number of articles to be able to split them
  -- across the right number of index pages.
  -- match "index*.html" $ route blogRoute
  -- metaCompile $ requireAll_ postsPattern
  --   >>> arr (chunk articlesPerIndexPage . chronological)
  --   >>^ makeIndexPages


  -- Add a tag list compiler for every tag used in blog articles.
  tagsRules tags (makeTagList tags)


  -- Render RSS feed for blog.
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = simplePostCtx `mappend` bodyField "description"
      posts <- fmap (take 10) . recentFirst =<<
               loadAllSnapshots postsPattern "content"
      renderRss feedConfiguration feedCtx posts
--      >>> mapCompiler (fixRssResourceUrls (feedRoot feedConfiguration))
  where
    postsPattern = fromGlob "posts/*/*/*/*.markdown" .||.
                   fromGlob "posts/*/*/*/*/text.markdown"
    postsRoute = gsubRoute "posts/" (const "blog/posts/")
  --   blogRoute = customRoute (\i -> "blog" </> toFilePath i)

-- fixRssResourceUrls :: String -> Compiler (Page String) (Page String)
-- fixRssResourceUrls root =
--   (arr $ getField "url" &&& id)
--   >>> arr (\(url, p) -> changeField "description"
--                         (fixResourceUrls' (root ++ takeDirectory url)) p)



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
postCompiler :: (Item String -> Compiler (Context String))
             -> Compiler (Item String)
postCompiler cctx = do
  i <- renderPandocWith defaultHakyllReaderOptions writeOptions <$> processTikZs
  ctx <- getResourceBody >>= cctx
  loadAndApplyTemplate "templates/post.html" ctx i
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplates ["blog", "default"] ctx
    >>= relativizeUrls


-- | Full context for posts.
--
postCtx :: Tags -> Item String -> Compiler (Context String)
postCtx t b = do
  m <- getMetadata $ itemIdentifier b
  let pageTitle = "Sky Blue Trades | " ++ (m M.! "title")
  return $
    mapContext prettify
      (tagsFieldWith getTags render join "prettytags" t) `mappend`
    tagCloudCtx t `mappend`
    functionField "teaser" teaserField `mappend`
    constField "pagetitle" pageTitle `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
  where prettify "" = ""
        prettify s = "<div class=\"tags\">" ++ s ++ "</div>"
        render _ Nothing = Nothing
        render tag (Just filePath) = Just $ H.span ! A.class_ "tag" $
          H.a ! A.href (toValue $ toUrl filePath) $ H.toHtml tag
        join = mconcat . intersperse " "



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
        H.a ! A.href (toValue url) $ toHtml tag
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


-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@.
--
postList :: Pattern
         -> ([Item String] -> Compiler [Item String])
         -> String
         -> Compiler String
postList pattern preprocess' tmpl = do
  postItemTpl <- loadBody $ fromFilePath $ "templates/" ++ tmpl ++ ".html"
  posts <- loadAll pattern
  processed <- preprocess' posts
  applyTemplateList postItemTpl simplePostCtx processed


-- | Auxiliary compiler: set up a tag list page.
--
makeTagList :: Tags -> String -> Pattern -> Rules ()
makeTagList tags tag pattern = do
  let title = "Posts tagged &#8216;" ++ tag ++ "&#8217;"
      pagetitle = "Sky Blue Trades | Tagged &#8216;" ++ tag ++ "&#8217;"
  route idRoute
  compile $ do
    list <- postList pattern recentFirst "tagitem"
    makeItem ""
      >>= loadAndApplyTemplates ["tags", "blog", "default"]
           (constField "title" title `mappend`
            constField "pagetitle" pagetitle `mappend`
            constField "posts" list `mappend`
            tagCloudCtx tags `mappend`
            defaultContext)
      >>= relativizeUrls


-- | Helper function for index page metacompilation: generate
-- appropriate number of index pages with correct names and the
-- appropriate posts on each one.
--
-- makeIndexPages :: [[Page String]] ->
--                   [(Identifier (Page String), Compiler () (Page String))]
-- makeIndexPages ps = map doOne (zip [1..] ps)
--   where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
--         maxn = nposts `div` articlesPerIndexPage +
--                if (nposts `mod` articlesPerIndexPage /= 0) then 1 else 0
--         nposts = sum $ map length ps
--         indexIdentifier n = parseIdentifier url
--           where url = "index" ++ (if (n == 1) then "" else show n) ++ ".html"


-- | Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
--
-- makeIndexPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
-- makeIndexPage n maxn posts =
--   constA (mempty, posts)
--   >>> addPostList "templates/postitem.html"
--   >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
--   >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
--   >>> applyTemplateCompilers ["posts", "index", "blog", "default"]
--   >>> relativizeUrlsCompiler


-- | Generate navigation link HTML for stepping between index pages.
--
-- indexNavLink :: Int -> Int -> Int -> String
-- indexNavLink n d maxn = renderHtml ref
--   where ref = if (refPage == "") then ""
--               else H.a ! A.href (toValue $ toUrl $ refPage) $
--                    (H.preEscapedToMarkup lab)
--         lab :: String
--         lab = if (d > 0) then "&laquo; OLDER POSTS" else "NEWER POSTS &raquo;"
--         refPage = if (n + d < 1 || n + d > maxn) then ""
--                   else case (n + d) of
--                     1 -> "blog/index.html"
--                     _ -> "blog/index" ++ (show $ n + d) ++ ".html"


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
  (b, url) <- (itemBody &&& itemIdentifier) <$> getResourceBody
  return $ fixResourceUrls' (takeDirectory $ toFilePath url) $ extractTeaser b
    -- >>> arr (\(p, b) -> setField "readmore"
    --                     (if (isInfixOf "<!--MORE-->" (pageBody p))
    --                      then (readMoreLink p) else "") p)
      where
        extractTeaser = unlines . (noTeaser . extractTeaser') .
                        drop 1 . dropWhile (/= "---") . drop 1 . lines
        extractTeaser' = takeWhile (/= "<!--MORE-->")

        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) =
          drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : (noTeaser xs)

        -- readMoreLink :: Page String -> String
        -- readMoreLink p = renderHtml $ H.div ! A.class_ "readmore" $
        --                  H.a ! A.href (toValue $ getField "url" p) $
        --                  H.preEscapedToMarkup ("Read more &raquo;"::String)

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
      putStrLn $ "Published to " ++ postPath


-- | Add a timestamp as metadata for ordering purposes.
--
addTimestamp :: String -> IO ()
addTimestamp postPath = do
  fExist <- doesFileExist postPath
  let modFile = if fExist then postPath else postPath ++ "/text.markdown"
  putStrLn ("Editing " ++ modFile)
  pg <- B.readFile modFile
  t <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%M-%d %H:%M:%S" t
  let pg' = addTimestamp' ts (B.unpack pg)
  B.writeFile modFile $ B.pack $ pg'
    where addTimestamp' :: String -> String -> String
          addTimestamp' ts i = init $ unlines ["---", md, "---", body]
            where (m, b) = span (/= "---") . tail . lines $ i
                  md = init $ unlines $ m ++ ["published: " ++ ts]
                  body = init $ unlines $ tail b


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
