{-# LANGUAGE OverloadedStrings, Arrows #-}

-- All the functions in this module are basically copied wholesale
-- from the Hakyll source and slightly modified, either to fit in with
-- the article naming convention I use (YYYY/MM/DD/NAME...) or to
-- handle some small issues that are hard to work around otherwise
-- (e.g. I use the HTML <OBJECT> tag to include SVG images, which
-- means that I need to relativize URLs in DATA attributes, as well as
-- in SRC and HREF).

module Overrides where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr, (&&&), (<<^), (>>^), returnA)
import Control.Category (id)
import Data.List (intercalate, sortBy, isPrefixOf)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Ord (comparing)
import System.FilePath (takeFileName, takeDirectory, 
                        joinPath, splitDirectories, dropExtension)
import System.Locale (TimeLocale, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze ((!), toHtml, toValue)
import Text.HTML.TagSoup (Tag (..), renderTags, parseTags)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Debug.Trace (trace, traceShow)

-- We override some names from Hakyll so we can use a different post
-- naming convention.
import Hakyll hiding (chronological, renderDateField, renderDateFieldWith, 
                      renderTagsField, renderTagCloud, 
                      relativizeUrlsCompiler, relativizeUrls, withUrls)



-- | Compiler form of 'relativizeUrls' which automatically picks the right root
-- path
--
relativizeUrlsCompiler :: Compiler (Page String) (Page String)
relativizeUrlsCompiler = getRoute &&& id >>^ uncurry relativize
  where
    relativize Nothing = id
    relativize (Just r) = fmap (relativizeUrls $ toSiteRoot r)

-- | Relativize URLs in HTML
--
relativizeUrls :: String  -- ^ Path to the site root
               -> String  -- ^ HTML to relativize
               -> String  -- ^ Resulting HTML
relativizeUrls root = withUrls ["src", "href", "data"] 
                      (\x -> if "/" `isPrefixOf` x then root ++ x else x)

-- | Apply a function to each URL on a webpage
--
withUrls :: [String] -> (String -> String) -> String -> String
withUrls urls f = renderTags . map tag . parseTags
  where
    tag (TagOpen s a) = TagOpen s $ map attr a
    tag x = x
    attr (k, v) = (k, if k `S.member` refs then f v else v)
    refs = S.fromList urls


-- | Render a tag cloud in HTML
--
renderTagCloud :: Compiler (Tags String) String
renderTagCloud =
    tagRenderer (fromCapture "tags/*") makeLink (intercalate " ")
  where
    (minSize, maxSize) = (100, 200)
    
    makeLink tag url count min' max' = renderHtml $
        H.span ! A.class_ "tagcloud" ! 
        A.style (toValue $ "font-size: " ++ size count min' max') $
        H.a ! A.href (toValue url) $ toHtml tag

    -- Show the relative size of one 'count' in percent
    size count min' max' =
        let diff = 1 + fromIntegral max' - fromIntegral min'
            relative = (fromIntegral count - fromIntegral min') / diff
            size' = floor $ minSize + relative * (maxSize - minSize)
        in show (size' :: Int) ++ "%"


-- | Render tags in HTML
--
tagRenderer :: (String -> Identifier (Page a))
           -- ^ Produce a link
           -> (String -> String -> Int -> Int -> Int -> String)
           -- ^ Produce a tag item: tag, url, count, min count, max count
           -> ([String] -> String)
           -- ^ Join items
           -> Compiler (Tags a) String
           -- ^ Tag cloud renderer
tagRenderer makeUrl makeItem concatItems = proc (Tags tags) -> do
    -- In tags' we create a list: [((tag, route), count)]
    tags' <- mapCompiler ((id &&& (getRouteFor <<^ makeUrl)) *** arr length)
                -< tags

    let -- Absolute frequencies of the pages
        freqs = map snd tags'

        -- The minimum and maximum count found
        (min', max')
            | null freqs = (0, 1)
            | otherwise = (minimum &&& maximum) freqs

        -- Create a link for one item
        makeItem' ((tag, url), count) =
            makeItem tag (toUrl $ fromMaybe "/" url) count min' max'

    -- Render and return the HTML
    returnA -< concatItems $ map makeItem' tags'


-- | When the metadata has a field called @path@ in a
-- @folder/yyyy/mm/dd/title[.extension]@ format this function can
-- render the date.
--
-- > renderDate "date" "%B %e, %Y" "Date unknown"
--
-- Will render something like @January 32, 2010@.
--
renderDateField :: String  -- ^ Key in which the rendered date should be placed
                   -> String  -- ^ Format to use on the date
                   -> String  -- ^ Default value, in case the date cannot be parsed
                   -> Page a  -- ^ Page on which this should be applied
                   -> Page a  -- ^ Resulting page
renderDateField = renderDateFieldWith defaultTimeLocale


-- | Render tags with links
--
renderTagsField :: String                       -- ^ Destination key
                   -> (String -> Identifier a)    -- ^ Create a link for a tag
                   -> Compiler (Page a) (Page a)  -- ^ Resulting compiler
renderTagsField = renderTagsFieldWith getTags


renderTagsFieldWith :: (Page a -> [String])          -- ^ Function to get the tags
                       -> String                   -- ^ Destination key
                       -> (String -> Identifier a)    -- ^ Create a link for a tag
                       -> Compiler (Page a) (Page a)  -- ^ Resulting compiler
renderTagsFieldWith tags destination makeUrl =
    id &&& arr tags >>> setFieldA destination renderTags'
  where
    -- Compiler creating a comma-separated HTML string for a list of tags
    renderTags' :: Compiler [String] String
    renderTags' = arr (map $ id &&& makeUrl)
                >>> mapCompiler (id *** getRouteFor)
                >>> arr (map $ uncurry renderLink)
                >>> arr surround

    surround :: [Maybe H.Html] -> String
    surround ms = if (length links > 0)
                  then "<div class=\"tags\">" ++ links ++ "</div>"
                  else ""
                      where links = intercalate " " $ map renderHtml $ catMaybes ms
    
    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.span ! A.class_ "tag" $ 
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


-- | Obtain tags from a page
--
getTags :: Page a -> [String]
getTags = map trim . splitAll "," . getField "tags"


-- | This is an extended version of 'renderDateField' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'renderDateField'.
--
renderDateFieldWith :: TimeLocale  -- ^ Output time locale
                       -> String      -- ^ Destination key
                       -> String      -- ^ Format to use on the date
                       -> String      -- ^ Default value
                       -> Page a      -- ^ Target page
                       -> Page a      -- ^ Resulting page
renderDateFieldWith locale key format defaultValue =
  renderField' ["path", "timestamp"] key renderDate'
  where
    renderDate' [filePath, ts] = fromMaybe defaultValue $ do
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d %H:%M:%S"
                          (dateString filePath ++ " " ++ 
                           if (ts == "") then "00:00:00" else ts) :: Maybe UTCTime
        return $ formatTime locale format time
    dateString filePath = intercalate "-" $ take 3
                          $ drop 1 $ splitDirectories filePath

renderField' :: [String] -> String -> ([String] -> String) -> Page a -> Page a
renderField' srcs dst f page = 
  setField dst (f $ map (fromMaybe "") values) page
    where values = map ((flip M.lookup) (pageMetadata page)) srcs

renderField src dst f page = case M.lookup src (pageMetadata page) of
    Nothing    -> page
    Just value -> setField dst (f value) page

-- | Sort pages chronologically. This function assumes that the pages have a
-- @year/month/day/title[.extension]@ naming scheme.
--
chronological :: [Page String] -> [Page String]
chronological = reverse . (sortBy $ comparing pageSortKey)


-- | Generate a sort key for ordering entries on the index page.
--
pageSortKey :: Page String -> String
pageSortKey pg =  datePart ++ "/" ++ (if ts /= "" then ts else namePart)
  where path = getField "path" pg
        ts = getField "timestamp" pg
        datePart = joinPath $ take 3 $ drop 1 $ splitDirectories path
        namePart = case (takeFileName path) of
            "text.markdown" -> last $ splitDirectories $ takeDirectory path
            _               -> dropExtension (takeFileName path)
