{-# LANGUAGE MultiWayIf #-}

module Lib
    ( convert
    ) where

import Control.Monad           (forM_)
import Data.List               (isSuffixOf, nub, sort)
import GHC.IO.Exception        (ExitCode)
import System.Cmd              (system)
import System.Directory        (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile)
import System.Environment      (getArgs)
import System.FilePath         ((</>), dropExtension)
import Text.Printf             (printf)

data Codec = Flac | Wav | Mp3

instance Show Codec where
    show Flac = "flac"
    show Wav  = "wav"
    show Mp3  = "mp3"

extension :: Codec -> String
extension c = "." ++ show c

decode :: Codec -> FilePath -> IO ExitCode
decode Flac = runCommand "flac -d"
decode _    = undefined

encode :: Codec -> FilePath -> IO ExitCode
encode Mp3 = runCommand "lame -b 320"
encode _   = undefined

runCommand :: String -> FilePath -> IO ExitCode
runCommand c a = system $ printf "%s \"%s\"" c a

getFlacs :: FilePath -> IO [FilePath]
getFlacs path = do
                isDir  <- doesDirectoryExist path
                isFile <- doesFileExist      path
                files  <- if | isDir     -> getDirectoryContents path >>= mapM (canonicalizePath . (path </>))
                             | isFile    -> fmap (:[]) $ canonicalizePath path
                             | otherwise -> return []
                return $ filter (isSuffixOf $ extension Flac) files

convert :: IO ()
convert = do
       flacs <- (return . sort . nub . concat) =<< mapM getFlacs =<< getArgs
       forM_ flacs $ decode Flac
       let wavs = map (\flac -> dropExtension flac ++ extension Wav) flacs
       forM_ wavs $ encode Mp3
       forM_ wavs removeFile
