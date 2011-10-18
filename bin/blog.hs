{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), arr, (&&&), (>>^))
import Control.Category (id)
import Control.Monad (forM_)
import Data.Monoid (mempty, mconcat)
import Data.List (isInfixOf)
import Text.Pandoc (Pandoc, HTMLMathMethod(..), WriterOptions(..), 
                    defaultWriterOptions, ParserState)
import Text.Pandoc.Shared (ObfuscationMethod(..))
import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, 
                         createDirectoryIfMissing, 
                         renameFile, renameDirectory)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (toGregorian)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime, formatTime)
import System.FilePath (joinPath, splitDirectories, takeDirectory)
import System.Cmd (rawSystem)
import Data.String.Utils (replace)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue, preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)

-- We override some names from Hakyll so we can use a different post
-- naming convention.
import Hakyll hiding (chronological, renderDateField, renderDateFieldWith, 
                      renderTagsField, renderTagCloud, 
                      relativizeUrlsCompiler, relativizeUrls, withUrls)

import Overrides                -- Overrides of Hakyll functions.
import TikZ                     -- TikZ image rendering.



-- | Number of article teasers displayed per sorted index page.
--
articlesPerIndexPage :: Int
articlesPerIndexPage = 10


-- | Set up deployment command.
--
hakyllConf = defaultHakyllConfiguration {
  deployCommand = "rsync -ave ssh _site/ iross@www.skybluetrades.net:/var/www"
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
    -- Read templates.
    match "templates/*" $ compile templateCompiler
    
    -- Compress CSS files.
    match "css/*" $ do
      route   $ setExtension "css"
      compile sass


    -- Render simple posts.
    forM_ ["*.markdown", "*.lhs"] $ 
      \p -> match (parseGlob ("posts/*/*/*/" ++ p)) $ do
        route   $ setExtension ".html"
        compile $ postCompiler

    -- Render posts with resources.
    forM_ ["text.markdown", "text.lhs"] $ 
      \p -> match (parseGlob ("posts/*/*/*/*/" ++ p)) $ do
        route   $ gsubRoute "text.markdown" (const "index.html")
        compile $ postCompiler

    -- Copy resource files.
    match "posts/*/*/*/*/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Static files, images and old web site stuff to just be copied
    -- over.
    forM_ [ "files/*", "images/*" ] $
      \p -> match p $ do
        route   idRoute
        compile copyFileCompiler

    -- Static pages.
    match "static/*" $ do
      route $ setExtension ".html"
      compile staticCompiler
      

    -- Generate index pages: we need to calculate and pass through the
    -- total number of articles to be able to split them across the
    -- right number of index pages.
    match "index*.html" $ route idRoute
    metaCompile $ requireAll_ postsPattern
      >>> (arr length &&& 
           arr (chunk articlesPerIndexPage . chronological))
      >>^ makeIndexPages
      

    -- Extract tags.
    create "tags" $
      requireAll postsPattern (\_ ps -> readTags ps :: Tags String)
    
    -- Add a tag list compiler for every tag.
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
      >>^ tagsMap
      >>^ (map (\(t, p) -> (fromCapture "tags/*" t, makeTagList t p)))


    -- Import blogroll.
    match "resources/blogroll.html" $ compile getResourceString


    -- Render RSS feed.
    match "rss.xml" $ route idRoute
    create "rss.xml" $
      requireAll_ postsPattern
      >>> mapCompiler (arr $ copyBodyToField "description")
      >>> renderRss feedConfiguration
  where
    postsPattern :: Pattern (Page String)
    postsPattern = predicate (\i -> matches "posts/*/*/*/*.markdown" i || 
                                    matches "posts/*/*/*/*/text.markdown" i)


-- | Process SCSS or CSS.
--
sass :: Compiler Resource String
sass = getResourceString >>> unixFilter "sass" ["-s", "--scss"]
                         >>^ compressCss


-- | Main post compiler: renders date field, adds tags, page title,
-- extracts teaser, applies templates.  This has to use a slightly
-- lower level approach than calling pageCompiler because it needs to
-- get at the raw Markdown source to pick out TikZ images.
--
postCompiler :: Compiler Resource (Page String)
postCompiler = readPageCompiler 
  >>> processTikZs
  >>> addDefaultFields >>> arr applySelf
  >>> pageReadPandocWith defaultHakyllParserState
  >>> arr (fmap (writePandocWith articleWriterOptions))
  >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
  >>> renderTagsField "prettytags" "<div class=\"tags\">" "</div>" "" 
      (fromCapture "tags/*")
  >>> addPageTitle >>> addTeaser
  >>> applyTemplateCompiler "templates/post.html"
  >>> applyTemplateCompiler "templates/onecol.html"
  >>> applyTemplateCompiler "templates/default.html"
  >>> relativizeUrlsCompiler


-- | Pandoc writer options.
--
articleWriterOptions :: WriterOptions
articleWriterOptions = defaultWriterOptions
    { writerEmailObfuscation = NoObfuscation, 
      writerHTMLMathMethod   = MathML Nothing, 
      writerLiterateHaskell  = True }


-- | Add a page title field.
--
addPageTitle :: Compiler (Page String) (Page String)
addPageTitle = (id &&& arr (getField "title")) 
               >>> arr (\(p, t) -> setField "pagetitle" 
                                   ("Sky Blue Trades | " ++ t) p)


-- | Static page compiler: page title, applies templates.
--
staticCompiler :: Compiler Resource (Page String)
staticCompiler = pageCompiler 
  >>> addPageTitle
  >>> applyTemplateCompiler "templates/static.html"
  >>> applyTemplateCompiler "templates/onecol.html"
  >>> applyTemplateCompiler "templates/default.html"
  >>> relativizeUrlsCompiler


-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@.
--
addPostList :: String -> Compiler (Page String, [Page String]) (Page String)
addPostList tmp = setFieldA "posts" $
    arr chronological
        >>> require (parseIdentifier tmp) (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody


-- | Auxiliary compiler: set up a tag list page.
--
makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList "templates/tagitem.html"
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> arr (setField "pagetitle" 
                 ("Sky Blue Trades | Tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud)
        >>> requireA "resources/blogroll.html" (setFieldA "blogroll" renderBlogRoll)
        >>> applyTemplateCompiler "templates/tags.html"
        >>> applyTemplateCompiler "templates/onecol.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler


-- | Helper function to fix up link categories in blogroll.
--
renderBlogRoll :: Compiler String String
renderBlogRoll = arr (replace "<a" "<a class=\"blogrolllink\"" . 
                      replace "<div" "<div class=\"blogrollcategory\"")


-- | Helper function for index page metacompilation: generate
-- appropriate number of index pages with correct names and the
-- appropriate posts on each one.
--
makeIndexPages :: (Int, [[Page String]]) -> 
                  [(Identifier (Page String), Compiler () (Page String))]
makeIndexPages (nposts, ps) = map doOne (zip [1..] ps)
  where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
        maxn = nposts `div` articlesPerIndexPage +
               if (nposts `mod` articlesPerIndexPage /= 0) then 1 else 0
        indexIdentifier n = parseIdentifier url
          where url = if (n == 1) 
                      then "index.html" 
                      else "index" ++ (show n) ++ ".html"


-- | Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
--
makeIndexPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
makeIndexPage n maxn posts = 
  constA (mempty, posts)
  >>> addPostList "templates/postitem.html"
  >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
  >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
  >>> arr (setField "pagetitle" "Sky Blue Trades")
  >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud)
  >>> requireA "resources/blogroll.html" (setFieldA "blogroll" renderBlogRoll)
  >>> applyTemplateCompiler "templates/posts.html"
  >>> applyTemplateCompiler "templates/index.html"
  >>> applyTemplateCompiler "templates/twocol.html"
  >>> applyTemplateCompiler "templates/default.html"
  >>> relativizeUrlsCompiler


-- | Generate navigation link HTML for stepping between index pages.
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n dir maxn = renderHtml ref
  where ref = if (refPage == "")
              then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $ 
                   (preEscapedString dirlabel)
        dirlabel = if (dir > 0) 
                   then "&laquo; OLDER POSTS" 
                   else "NEWER POSTS &raquo;"
        refPage = if (n + dir < 1 || n + dir > maxn)
                  then ""
                  else case (n + dir) of
                    1 -> "index.html"
                    _ -> "index" ++ (show $ n + dir) ++ ".html"
  

-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Sky Blue Trades RSS feed."
    , feedDescription = "RSS feed for the Sky Blue Trades blog."
    , feedAuthorName  = "Ian Ross"
    , feedRoot        = "http://www.skybluetrades.net/blog"
    }


-- | Turns body of the page into the teaser: anything up to the
-- <!--MORE--> mark is the teaser, except for text between the
-- <!--NOTEASERBEGIN--> and <!--NOTEASEREND--> marks (useful for
-- keeping images out of teasers).
--
addTeaser :: Compiler (Page String) (Page String) 
addTeaser = arr (copyBodyToField "teaser")
    >>> arr (changeField "teaser" extractTeaser)
    >>> (arr $ getField "url" &&& id) 
    >>> fixTeaserResourceUrls
    >>> (id &&& arr pageBody)
    >>> arr (\(p, b) -> setField "readmore" 
                        (if (isInfixOf "<!--MORE-->" (pageBody p)) 
                         then (readMoreLink p) else "") p)
      where
        extractTeaser :: String -> String
        extractTeaser = unlines . (noTeaser . extractTeaser') . lines
        
        extractTeaser' :: [String] -> [String]
        extractTeaser' = takeWhile (/= "<!--MORE-->")
        
        noTeaser :: [String] -> [String]
        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) = 
          drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : (noTeaser xs)
        
        readMoreLink :: Page String -> String
        readMoreLink p = renderHtml $ H.div ! A.class_ "readmore" $ 
                         H.a ! A.href (toValue $ getField "url" p) $ 
                         preEscapedString "Read more &raquo;"
                         
        fixTeaserResourceUrls :: Compiler (String, (Page String)) (Page String)
        fixTeaserResourceUrls = arr $ (\(url, p) -> fixResourceUrls' url p)
          where fixResourceUrls' url p = 
                  changeField "teaser" (fixResourceUrls'' (takeDirectory url)) p

        fixResourceUrls'' :: String -> String -> String
        fixResourceUrls'' path = withUrls ["src", "href", "data"] rel
          where
            rel x = if '/' `elem` x then x else path ++ "/" ++ x


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
  let ts = formatTime defaultTimeLocale "%H:%M:%S" t
  B.writeFile modFile $ B.pack $ addTimestamp' (B.unpack pg) ts
    where addTimestamp' pg ts = writePage $ setField "timestamp" ts $ readPage pg
          writePage :: Page String -> String
          writePage pg = "---\n" ++ renderMetadata (pageMetadata pg) ++ 
                         "---\n" ++ (pageBody pg)
          renderMetadata md = unlines $ map (\(k, d) -> k ++ ": " ++ d) $ M.toList md
        

-- | Utility function to generate path to today's posts directory.
--
todaysPostDir :: IO FilePath
todaysPostDir = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay t
  return $ joinPath ["posts", show y, show0 m, show0 d]
  where show0 n = (if n < 10 then "0" else "") ++ show n
  

-- | Split list into equal sized sublists.
--
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
