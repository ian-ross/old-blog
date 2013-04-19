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
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import Text.HTML.TagSoup (Tag (..), renderTags, parseTags)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Debug.Trace (trace, traceShow)

-- We override some names from Hakyll so we can use a different post
-- naming convention.
import Hakyll hiding (chronological, renderTagsField,
                      renderTagsFieldWith, renderTagCloud)



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
        H.a ! A.href (toValue url) $ H.toHtml tag

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
