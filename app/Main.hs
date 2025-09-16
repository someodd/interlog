{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs, getEnvironment)
import System.Exit (die)
import System.Directory
  ( doesDirectoryExist
  , listDirectory
  , getModificationTime
  , doesFileExist
  )
import System.FilePath ((</>), takeExtension)
import System.IO
  ( IOMode(AppendMode, WriteMode)
  , withFile
  , hPutStr
  , hSetEncoding
  , utf8
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (sortOn, isPrefixOf, isSuffixOf, isInfixOf)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Posix.Files (fileSize, getFileStatus)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)

--------------------------------------------------------------------------------
-- Config via environment with defaults

readEnvInt :: [(String,String)] -> String -> Int -> Int
readEnvInt env k def =
  case lookup k env of
    Just v -> case reads v of
                [(n,"")] -> n
                _        -> def
    Nothing -> def

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  case args of
    (root:"index":rest) -> do
      ensureDir root
      let host     = getFlag "--host" rest `orDie` "index: --host=<host> is required"
          port     = fromMaybe "70" (getFlag "--port" rest)
          baseSel  = fromMaybe ""   (getFlag "--base-selector" rest)
          logsSel  = getFlag "--logs-selector" rest
      gm <- buildIndex env root host port baseSel logsSel
      putStr gm

    (root:"new":rest) -> do
      ensureDir root
      let (confirmTxt, msgParts) = getAndStripFlag "--confirm-text" rest
      msg <- joinMessage msgParts
      enforceNoNewlines msg
      let appendMax = toInteger $ readEnvInt env "APPEND_MAX_SIZE" 262144 -- 256 KiB default
      let logMax    = toInteger $ readEnvInt env "LOG_MAX_SIZE"    1048576 -- 1 MiB default
      if BS.length (BSC.pack msg) > fromIntegral appendMax
         then die "new: message exceeds APPEND_MAX_SIZE"
         else do
           fname <- createNewLog root msg logMax
           putStrLn (formatConfirm confirmTxt fname)

    (root:"append":lid:rest) -> do
      ensureDir root
      let (confirmTxt, msgParts) = getAndStripFlag "--confirm-text" rest
      msg <- joinMessage msgParts
      enforceNoNewlines msg
      let appendMax = toInteger $ readEnvInt env "APPEND_MAX_SIZE" 262144
      let logMax    = toInteger $ readEnvInt env "LOG_MAX_SIZE"    1048576
      if BS.length (BSC.pack msg) > fromIntegral appendMax
         then die "append: message exceeds APPEND_MAX_SIZE"
         else do
           fname <- appendToLog root lid msg logMax
           putStrLn (formatConfirm confirmTxt fname)

    _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  interlog <interlogs/> index --host=<host> [--port=<port>] [--base-selector=<sel>] [--logs-selector=<sel>]"
  putStrLn "  interlog <interlogs/> new [--confirm-text=<text>] \"message goes here\""
  putStrLn "  interlog <interlogs/> append <id> [--confirm-text=<text>] \"message goes here\""
  putStrLn ""
  putStrLn "Notes:"
  putStrLn "  --confirm-text may contain %s which will be replaced with the filename."
  putStrLn ""
  putStrLn "Environment:"
  putStrLn "  LOG_MAX_SIZE      (bytes, default 1048576)"
  putStrLn "  APPEND_MAX_SIZE   (bytes, default 262144)"
  putStrLn "  FIRST_N_BYTES     (bytes, default 512)"
  putStrLn "  LAST_N_BYTES      (bytes, default 512)"
  putStrLn ""

--------------------------------------------------------------------------------
-- Helpers

ensureDir :: FilePath -> IO ()
ensureDir p = do
  ok <- doesDirectoryExist p
  if ok then pure () else die ("root directory not found: " ++ p)

-- Get a flag value from args (first match), like --name=value
getFlag :: String -> [String] -> Maybe String
getFlag name = go
  where
    go [] = Nothing
    go (x:xs)
      | (name ++ "=") `isPrefixOf` x = Just (drop (length name + 1) x)
      | otherwise = go xs

-- Extract a flag and return (value, argsWithoutThatFlag)
getAndStripFlag :: String -> [String] -> (Maybe String, [String])
getAndStripFlag name xs = go xs Nothing []
  where
    pfx = name ++ "="
    go [] mv acc = (mv, reverse acc)
    go (y:ys) mv acc
      | pfx `isPrefixOf` y
      , mv == Nothing
      = go ys (Just (drop (length pfx) y)) acc
      | otherwise = go ys mv (y:acc)

-- Format confirm text with optional %s -> filename substitution.
formatConfirm :: Maybe String -> String -> String
formatConfirm Nothing fname = fname
formatConfirm (Just t) fname =
  if "%s" `isInfixOf` t
     then replaceAll "%s" fname t
     else t

-- Simple replace-all (non-overlapping)
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll needle repl = go
  where
    nlen = length needle
    go [] = []
    go s@(x:xs)
      | needle `isPrefixOf` s = repl ++ go (drop nlen s)
      | otherwise             = x : go xs

orDie :: Maybe a -> String -> a
orDie (Just x) _ = x
orDie Nothing  m = error m

joinMessage :: [String] -> IO String
joinMessage [] = die "missing message"
joinMessage xs = pure (unwords xs)

enforceNoNewlines :: String -> IO ()
enforceNoNewlines s =
  if any (`elem` ("\r\n" :: String)) s
     then die "message may not contain newline characters"
     else pure ()

readFileSize :: FilePath -> IO Integer
readFileSize fp = do
  st <- getFileStatus fp
  pure (fromIntegral (fileSize st) :: Integer)

safeReadHeadTail :: FilePath -> Int -> Int -> IO (BS.ByteString, BS.ByteString)
safeReadHeadTail fp firstN lastN = do
  exists <- doesFileExist fp
  if not exists
    then pure (BS.empty, BS.empty)
    else do
      bs <- BS.readFile fp
      let len = BS.length bs
      let h = BS.take (min firstN len) bs
      let t = if len <= firstN
                 then BS.empty
                 else let ln = min lastN (len - firstN)
                       in BS.drop (len - ln) bs
      pure (h,t)

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

--------------------------------------------------------------------------------
-- Gopher helpers (info lines and previews)

-- Make a single info line (type 'i') with all gopher fields and CRLF.
infoLine :: String -> String
infoLine display =
  "i" <> sanitizeDisplay display <> "\tnull\terror.host\t1\r\n"

-- Sanitize the display text for gophermap (replace tabs/CR).
sanitizeDisplay :: String -> String
sanitizeDisplay = map (\c -> case c of
                               '\t' -> ' '
                               '\r' -> ' '
                               _    -> c)

-- Render a multi-line preview block as a series of 'i' lines.
renderInfoBlock :: String -> String
renderInfoBlock s
  | null s    = infoLine "(empty)"
  | otherwise = concatMap infoLine (lines s)

-- Build the labeled preview section.
previewSection :: String -> BS.ByteString -> String
previewSection label bs =
  infoLine ("--- " <> label <> " ---")
  <> renderInfoBlock (BSC.unpack bs)

--------------------------------------------------------------------------------
-- Index (gophermap)

buildIndex :: [(String,String)]
           -> FilePath -> String -> String -> String -> Maybe String
           -> IO String
buildIndex env root host port baseSel mLogsSel = do
  files <- listDirectory root
  let threadFiles = filter (\f -> takeExtension f == ".txt" && '_' `elem` f) files
  -- Sort by mtime descending
  withTimes <- mapM (\f -> do
                        t <- getModificationTime (root </> f)
                        pure (f,t)) threadFiles
  let sorted = reverse (sortOn snd withTimes)
  let firstN = readEnvInt env "FIRST_N_BYTES" 512
  let lastN  = readEnvInt env "LAST_N_BYTES"  512

  let logsLink = case mLogsSel of
        Nothing   -> ""
        Just lsel ->
          "1Raw logs (autoindex)\t"
          <> selector (pathJoin [baseSel, lsel])
          <> "\t" <> host <> "\t" <> port <> "\r\n"

  let header = mconcat
        [ "iinterlog index\tnull\terror.host\t1\r\n"
        , "hinterlog's project home\tURL:https://github.com/someodd/interlog\terror.host\t1\r\n"
        , "iThis is an experimental forum.\tnull\terror.host\t1\r\n"
        , logsLink
        , "7Create new log\t"
          <> selector (pathJoin [baseSel,"new"]) <> "\t" <> host <> "\t" <> port <> "\r\n"
        , "i \tnull\terror.host\t1\r\n"
        ]

  entries <- mapM (entry firstN lastN) sorted
  pure $ header <> mconcat entries <> ".\r\n"
  where
    entry :: Int -> Int -> (FilePath, UTCTime) -> IO String
    entry firstN lastN (fn, mt) = do
      let (lid,_,title) = splitFilename fn
      let display = if null title then fn else title
      let path    = root </> fn
      (h,t) <- safeReadHeadTail path firstN lastN
      szI   <- readFileSize path
      let sz :: Int
          sz = fromIntegral szI
      let selView   = selector (pathJoin [baseSel,"log",fn])
      let selAppend = selector (pathJoin [baseSel,"append",lid])

      let preview =
            if sz <= firstN
               then previewSection "content" h
               else previewSection "first bytes" h <> previewSection "last bytes ----" t

      pure $ mconcat
        [ "0", display, "  [", fmtTime mt, "]\t", selView, "\t", host, "\t", port, "\r\n"
        , preview
        , "7Append…\t", selAppend, "\t", host, "\t", port, "\r\n"
        , "i \tnull\terror.host\t1\r\n"
        ]

    selector :: String -> String
    selector s = if null s then "/" else s

    pathJoin :: [String] -> String
    pathJoin parts =
      let cleaned = filter (not . null) parts
      in "/" <> foldl (\acc p -> if null acc then p else acc <> "/" <> p) "" cleaned

-- Split "id_epoch_title.txt" => (id, epoch, titleWithoutExt)
splitFilename :: FilePath -> (String,String,String)
splitFilename fn =
  let base = if ".txt" `isSuffixOf` fn then take (length fn - 4) fn else fn
      parts = split '_' base
  in case parts of
       (a:b:rest) -> (a,b,joinWith '_' rest)
       _          -> (base,"",base)
  where
    split :: Char -> String -> [String]
    split _ "" = [""]
    split c s  =
      let (h, r) = break (==c) s
      in case r of
           []      -> [h]
           (_:r')  -> h : split c r'
    joinWith :: Char -> [String] -> String
    joinWith _ [] = ""
    joinWith c (x:xs) = x ++ concatMap (c:) xs

--------------------------------------------------------------------------------
-- Create new log

createNewLog :: FilePath -> String -> Integer -> IO String
createNewLog root msg logMax = do
  now <- getPOSIXTime
  let epoch :: Integer
      epoch = floor now
  let titleLine = takeWhile (/= '\n') msg
  let titleSlug = slug (trim titleLine)
  lid <- genId
  let fname = lid ++ "_" ++ show epoch ++ "_" ++ titleSlug ++ ".txt"
  let path  = root </> fname
  exists <- doesFileExist path
  if exists
     then die "new: collision creating log (try again)"
     else do
       let sz = BS.length (BSC.pack msg)
       if toInteger sz > logMax
          then die "new: log would exceed LOG_MAX_SIZE"
          else do
            withFile path WriteMode $ \h -> do
              hSetEncoding h utf8
              hPutStr h msg
            pure fname

-- Simple ID from current POSIX time fractions (no crypto; PoC)
genId :: IO String
genId = do
  t <- getPOSIXTime
  let n = floor (t * 1000000) :: Integer
  pure (take 10 (hex n))
  where
    hex :: Integer -> String
    hex 0 = "0"
    hex x = let go 0 acc = acc
                go y acc = go (y `div` 16) (digits !! fromIntegral (y `mod` 16) : acc)
            in go x ""
    digits = "0123456789abcdef"

slug :: String -> String
slug = take 80 . map toAllowed
  where
    toAllowed c
      | c `elem` ['a'..'z'] = c
      | c `elem` ['A'..'Z'] = toLower c
      | c `elem` ['0'..'9'] = c
      | c == '-' || c == '_' = c
      | c == ' ' = '-'
      | otherwise = '-'
    toLower ch = if ch >= 'A' && ch <= 'Z' then toEnum (fromEnum ch + 32) else ch

--------------------------------------------------------------------------------
-- Append

appendToLog :: FilePath -> String -> String -> Integer -> IO String
appendToLog root lid msg logMax = do
  -- Find a file that begins with "<id>_"
  files <- listDirectory root
  let candidates = filter (\f -> (lid ++ "_") `isPrefixOf` f && takeExtension f == ".txt") files
  case candidates of
    []      -> die "append: no matching log id"
    (fn:_)  -> do
      let path = root </> fn
      size0 <- readFileSize path
      let toWrite =
            if size0 > 0
              then '\n' : msg   -- ensure a newline separator
              else msg
      let newSize = size0 + toInteger (BS.length (BSC.pack toWrite))
      if newSize > logMax
         then die "append: log would exceed LOG_MAX_SIZE"
         else do
           withFile path AppendMode $ \h -> do
             hSetEncoding h utf8
             hPutStr h toWrite
           pure fn
