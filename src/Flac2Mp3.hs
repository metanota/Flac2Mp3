import Control.Monad           (forM_, liftM)
import Data.List               (isSuffixOf)
import GHC.IO.Exception        (ExitCode)
import System.Cmd              (system)
import System.Directory        (getDirectoryContents, removeFile)
import System.Environment      (getArgs)
import System.FilePath         (combine)

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

filterBy :: String -> FilePath -> [FilePath] -> [FilePath]
filterBy ext dir = reverse . map (combine dir) . filter (isSuffixOf ext)

getFilesFiltered :: FilePath -> String -> IO [FilePath]
getFilesFiltered path ext = do
                            files <- getDirectoryContents path
                            return $ filterBy ext path files

main :: IO ()
main = do
       path  <- liftM head getArgs
       flacs <- getFilesFiltered path $ extension Flac
       forM_ flacs $ decode Flac
       wavs  <- getFilesFiltered path $ extension Wav
       forM_ wavs $ encode Mp3
       forM_ wavs removeFile
