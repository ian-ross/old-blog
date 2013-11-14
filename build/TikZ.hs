-- Centre TikZs by including a style of
--
--   { display: block; margin-left: auto; margin-right: auto; }
--
-- Can also use
--
--   { float: left; margin-right: 10px; }
--
-- or
--
--   { float: right; margin-left: 10px; }
--
-- or something to flow text around images.

module TikZ (processTikZs) where

import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, renameFile,
                         createDirectoryIfMissing, removeDirectoryRecursive,
                         getCurrentDirectory, setCurrentDirectory)
import System.IO (openFile, hPutStrLn, hClose, IOMode(..))
import System.FilePath (addExtension)
import System.Cmd (system)
import System.Exit
import Data.Digest.Pure.MD5
import qualified Text.HTML.TagSoup as TS
import qualified Data.ByteString.Lazy.Char8 as C8

import Hakyll

-- | Main TikZ processing compiler: transforms TikZ images within
-- Markdown text, extracting image bodies and MD5 digests for
-- processing into SVG files.
--
processTikZs :: Compiler (Item String)
processTikZs = do
  b <- fmap itemBody getResourceBody
  ts <- unsafeCompiler (generateTikZs b)
  makeItem (xformTikZs b ts)

-- | Simple type for representing information about TikZ images:
-- includes MD5 digest (used for filename, image width and height in
-- pixels, plus style information.
--
data TikZInfo = TikZInfo { digest :: String,
                           w :: Int, h :: Int,
                           style :: String } deriving Show


-- | Replace TikZ images with HTML for getting SVG and PNG rendered
-- versions.
--
xformTikZs :: String -> [TikZInfo] -> String
xformTikZs p tikzs = unlines $ concatMap flattenChunk fixedChunks
  where chunks = extractChunks $ lines p
        fixedChunks = replacePictures chunks htmls
        htmls = map (Text . tikZHtmlRep) tikzs

        tikZHtmlRep :: TikZInfo -> [String]
        tikZHtmlRep (TikZInfo md5 w h style) =
          ["<object type=\"image/svg+xml\" data=\"/blog/tikzs/" ++
           (addExtension md5 "svg") ++
           "\" width=" ++ (show w) ++
           " height=" ++ (show h) ++
           (if (style == "") then "" else (" style=\"" ++ style ++ "\"")) ++
           "></object>"]


-- | Generate image files from TikZ representations.
--
generateTikZs :: String -> IO [TikZInfo]
generateTikZs p = forM pics renderSVG
  where pics = filter isPicture chunks
        chunks = extractChunks $ lines p


-- | Render a TikZ to an SVG file.
--
renderSVG :: Chunk -> IO TikZInfo
renderSVG (Picture attr tikz) = do
  createDirectoryIfMissing True "_site/blog/tikzs"
  pwd <- getCurrentDirectory
  setCurrentDirectory "_site/blog/tikzs"
  let md5 = makeDigest tikz
      svgf = addExtension md5 "svg"
  exists <- doesFileExist svgf
  if exists
    then return ()
    else do
    createDirectoryIfMissing True "tmp"
    setCurrentDirectory "tmp"
    writeTikzTmp "tmp.tex" $ unlines tikz
    system "htlatex tmp.tex 2>&1 > /dev/null"
    status <- doesFileExist "tmp-1.svg"
    setCurrentDirectory ".."
    if status
      then renameFile "tmp/tmp-1.svg" svgf
      else return ()
    removeDirectoryRecursive "tmp"
  (w, h) <- getSVGDimensions svgf
  setCurrentDirectory pwd
  return (TikZInfo md5 w h attr)


-- | Extract dimensions from first line of TikZ-rendered SVG file and
-- convert from points to pixels.
--
getSVGDimensions :: String -> IO (Int, Int)
getSVGDimensions svgf = do
  cont <- readFile svgf
  let svgtags = filter (TS.isTagOpenName "svg") $ TS.parseTags cont
  case svgtags of
    [] -> error "Failed to find \"svg\" tag in TikZ-rendered SVG file"
    _ -> return $ extractDimensions $ head svgtags
  where extractDimensions :: TS.Tag String -> (Int, Int)
        extractDimensions tag = (w, h)
          where ws = (fst . head . lex) $ TS.fromAttrib "width" tag
                hs = (fst . head . lex) $ TS.fromAttrib "height" tag
                w = floor (scale * read ws :: Float)
                h = floor (scale * read hs :: Float)
                scale = 1.35


-- | Write temporary LaTeX file for TikZ SVG rendering.
--
writeTikzTmp :: String -> String -> IO ()
writeTikzTmp f tikz = do
  h <- openFile f WriteMode
  hPutStrLn h "\\nonstopmode"
  hPutStrLn h "\\documentclass{minimal}"
  hPutStrLn h "\\def\\pgfsysdriver{pgfsys-tex4ht.def}"
  hPutStrLn h "\\usepackage{tikz}"
  hPutStrLn h "\\usetikzlibrary{arrows}"
  hPutStrLn h "\\begin{document}"
  hPutStrLn h "\\begin{tikzpicture}"
  hPutStrLn h tikz
  hPutStrLn h "\\end{tikzpicture}"
  hPutStrLn h "\\end{document}"
  hClose h


-- | Generate MD5 digest of TikZ image source.
--
makeDigest :: [String] -> String
makeDigest p = show $ md5 $ C8.pack $ strip $ concat p


-- | Simple chunk type used to pick out TikZ images.
--
data Chunk = Text [String] | Picture String [String] deriving Show


-- | Distinguish between picture (i.e. TikZ) and text chunks.
--
isPicture :: Chunk -> Bool
isPicture (Text _) = False
isPicture (Picture _ _) = True


-- | Chunk a Markdown input with possible embedded TikZ images
-- (delimited by lines with @@@) into "text" and "picture" elements.
--
extractChunks :: [String] -> [Chunk]
extractChunks [] = []
extractChunks (l:ls)
  | "@@@" `isPrefixOf` l = (Picture (attr l) ls') : extractChunks (tail rest)
  | otherwise            = (Text (l:ls')) : extractChunks rest
    where (ls', rest) = break ("@@@" `isPrefixOf`) ls
          attr = strip . stripBrackets . strip . dropWhile (=='@')
          stripBrackets =
            reverse . dropWhile (=='}') . reverse . dropWhile (=='{')


-- | Turn chunks back into flat text.
--
flattenChunk :: Chunk -> [String]
flattenChunk (Text ts) = ts
flattenChunk (Picture a ts) = ts


-- | Replace Picture chunks for TikZ images with object and image tag
-- text from a list.
--
replacePictures :: [Chunk] -> [Chunk] -> [Chunk]
replacePictures [] _ = []
replacePictures (c@(Text _):cs) tikzs = c : replacePictures cs tikzs
replacePictures (c@(Picture _ _):cs) (tikz:tikzs) =
  tikz : replacePictures cs tikzs

-- | Strip leading and trailing whitespace.
--
strip :: String -> String
strip = trim1 . trim1
  where trim1 = dropWhile isWs . reverse
        isWs c = c `elem` " \t\r\n"
