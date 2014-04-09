{-# LANGUAGE MultiWayIf #-}

import Control.Monad           (forM_, liftM)
import Data.List               (isSuffixOf)
import GHC.IO.Exception        (ExitCode)
import System.Cmd              (system)
import System.Directory        (doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile)
import System.Environment      (getArgs)
import System.FilePath         (combine, dropExtension)

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

runCommand :: String -> String -> IO ExitCode
runCommand c a = system $ c ++ " " ++ wrapArg a
                 where wrapArg x = "\"" ++ x ++ "\""

filterBy :: String -> [FilePath] -> [FilePath]
filterBy ext = reverse . filter (isSuffixOf ext)

getFilesFiltered :: FilePath -> String -> IO [FilePath]
getFilesFiltered path ext = do
                            isDir  <- doesDirectoryExist path
                            isFile <- doesFileExist      path
                            files  <- if | isDir     -> liftM (map (combine path)) $ getDirectoryContents path
                                         | isFile    -> return $ [path]
                                         | otherwise -> return $ []
                            return $ filterBy ext files

main :: IO ()
main = do
       path  <- liftM head getArgs
       flacs <- getFilesFiltered path $ extension Flac
       forM_ flacs $ decode Flac
       let wavs = map (\flac -> dropExtension flac ++ extension Wav) flacs
       forM_ wavs $ encode Mp3
       forM_ wavs removeFile
