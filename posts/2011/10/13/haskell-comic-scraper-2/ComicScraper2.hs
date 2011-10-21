import Data.List
import Data.Char
import Data.Time
import Network.HTTP
import System.Directory
import Text.HTML.TagSoup
import Text.Regex.Posix

data Comic = Comic { coName :: String,
                     coURL :: String,
                     coRegex :: String,
                     coPrefix :: String }
             deriving Show

-- Possible error modes (malformed URL, HTTP error) are dealt with by
-- exceptions raised from the Network.HTTP module.

fetchURL :: String -> IO String
fetchURL url = simpleHTTP (getRequest url) >>= getResponseBody

-- The only error mode we need to deal with here is the possiblity
-- that there is no match for the regex in the page we retrieve.

imageURL :: Comic -> IO String
imageURL c = do
  pg <- fetchURL (coURL c)
  let re = pg =~ (coRegex c) :: String
  case re of
    "" -> return (error $ "no match for regular expression for " ++ coName c)
    otherwise -> return (coPrefix c ++ re)

fileSuffix :: String -> String
fileSuffix = reverse . fst . break (=='.') . reverse

normaliseName :: String -> String
normaliseName = map fix_spaces . map toLower
  where fix_spaces ' ' = '_'
        fix_spaces ch = ch

-- All error modes here are handled by exceptions.
        
writeImageToFile :: Comic -> IO ()
writeImageToFile c = do
  putStrLn $ "Retrieving: " ++ (coName c)
  url <- imageURL c
  img <- fetchURL url
  writeFile (normaliseName $ coName c ++ "." ++ fileSuffix url) img

getToday :: IO Day
getToday = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  return (localDay $ utcToLocalTime tz tm)
  
makeDateName :: String -> IO String
makeDateName base = do
  date <- getToday
  return (base ++ "/" ++ showGregorian date)
  
-- All error modes here are handled by exceptions.  

processAll :: String -> [Comic] -> IO ()
processAll bd cs = do
  dateDirectory <- makeDateName bd
  createDirectoryIfMissing True dateDirectory
  setCurrentDirectory dateDirectory
  mapM_ writeImageToFile cs

getConfig :: FilePath -> IO (String, [Comic])
getConfig cfg_path = do
  fc <- readFile cfg_path
  let tags = parseTags fc
  let bdtags = filter (isTagOpenName "baseDirectory") tags
  case bdtags of
    [] -> return (error "baseDirectory field missing from comics.xml")
    otherwise -> do
      let bd = fromAttrib "name" $ head bdtags
      let cs = map makeComic $ filter (isTagOpenName "comic") tags
      return (bd, cs)
  where makeComic t = if n == "" || u == "" || r == "" || p == "" then
                         error "attribute missing in comic tag"
                         else Comic n u r p 
          where n = fromAttrib "name" t 
                u = fromAttrib "url" t
                r = fromAttrib "regex" t 
                p = fromAttrib "prefix" t

main :: IO ()
main = do
  (bd, cs) <- getConfig "comics.xml"
  processAll bd cs

  
