import Data.Char
import System.FilePath
import System.IO
import Network.HTTP
import Text.Regex.Posix
import qualified Data.ByteString as B

data Comic = Comic { coName :: String,
                     coURL :: String,
                     coRegex :: String,
                     coPrefix :: String }
             deriving Show

comics :: [Comic]
comics = [ Comic "Girl Genius"
                 "http://www.girlgeniusonline.com/comic.php"
                 "ggmain/strips/ggmain[^']+"
                 "http://www.girlgeniusonline.com/",
           Comic "XKCD"
                 "http://www.xkcd.com/"
                 "comics/.+png"
                 "http://imgs.xkcd.com/",
           Comic "Girls With Slingshots"
                 "http://www.daniellecorsetto.com/gws.html"
                 "images/gws/GWS[^\";]+"
                 "http://www.daniellecorsetto.com/" ]

fetchURL :: String -> IO String
fetchURL url = simpleHTTP (getRequest url) >>= getResponseBody

imageURL :: Comic -> IO String
imageURL c = do
  pg <- fetchURL (coURL c)
  let re = pg =~ (coRegex c) :: String
  case re of
    "" -> return (error $ "no match for regular expression for " ++ coName c)
    otherwise -> return (coPrefix c ++ re)

normaliseName :: String -> String
normaliseName = map fix_spaces . map toLower
  where fix_spaces ' ' = '_'
        fix_spaces ch = ch

writeImageToFile :: Comic -> IO ()
writeImageToFile c = do
  putStrLn $ "Retrieving: " ++ (coName c)
  url <- imageURL c
  img <- fetchURL url
  writeBinary (normaliseName $ coName c ++ takeExtension url) img
  
writeBinary :: String -> String -> IO ()
writeBinary f s = do
  h <- openFile f WriteMode
  hSetBinaryMode h True
  hPutStr h s
  hClose h

processAll :: [Comic] -> IO ()
processAll cs = mapM_ writeImageToFile cs

  
