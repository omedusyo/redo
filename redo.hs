
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception ( catch, catchJust, IOException )
import Control.Monad ( filterM, unless, guard )
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 ( md5 )
import Data.Map.Lazy ( insert, fromList, toList, adjust )
import Data.Maybe ( listToMaybe )
import Data.Typeable ( typeOf )
import Debug.Trace ( traceShow )
import GHC.IO.Exception ( IOErrorType(..) )
import System.Directory ( renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing )
import System.Environment ( getArgs, getEnvironment, getProgName, lookupEnv )
import System.Exit ( ExitCode(..) )
import System.FilePath ( hasExtension, replaceBaseName, takeBaseName, (</>) )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( ioeGetErrorType, isDoesNotExistError )
import System.Process ( createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..) )

traceShow' arg = traceShow arg arg

metaDir = ".redo"

main :: IO ()
main = do
    getArgs >>= mapM_ redo
    progName <- getProgName
    redoTarget' <- lookupEnv "REDO_TARGET"
    case (progName, redoTarget') of
      ("redo-ifchange", Just redoTarget) ->
        getArgs >>= mapM_ (writeMD5 redoTarget)
      ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
      _ -> return ()
  where writeMD5 redoTarget dep = md5' dep >>= writeFile (metaDir </> redoTarget </> dep) 

redo :: String -> IO ()
redo target = do
    upToDate' <- upToDate metaDepsDir
    unless upToDate' (redoPath target >>= maybe missingDo redo')
  where missingDo :: IO ()
        missingDo = do 
          exists <- doesFileExist target
          unless exists (error ("No .do file found for target '" ++ target ++ "'"))

        tmp = target ++ "---redoing"
        metaDepsDir = metaDir </> target

        command path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

        redo' :: FilePath -> IO ()
        redo' path = do
          catchJust (guard . isDoesNotExistError)
                    (removeDirectoryRecursive metaDepsDir)
                    (const (return ()))
          createDirectoryIfMissing True metaDepsDir
          md5' path >>= writeFile (metaDepsDir </> path)

          oldEnv <- getEnvironment
          let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv

          (_, _, _, ph) <- createProcess $ (shell (command path)) { env = Just newEnv }
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> renameFile tmp target
            ExitFailure code -> do hPutStrLn stderr ("Redo script exited with non-zero exit code: " ++ show code)
                                   removeFile tmp

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe <$> filterM doesFileExist candidates
  where candidates = (target ++ ".do") : [replaceBaseName target "defaults" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate metaDepsDir = catch
    (do deps <- getDirectoryContents metaDepsDir
        (traceShow' . and) <$> mapM depUpToDate deps)
    (\(_ :: IOException) -> return False)
  where depUpToDate :: FilePath -> IO Bool
        depUpToDate dep = catch
          (do oldMD5 <- upToSpace <$> readFile (metaDepsDir </> dep)
              newMD5 <- md5' dep
              return $ oldMD5 == newMD5)
          (\e -> return (ioeGetErrorType e == InappropriateType))

        upToSpace :: String -> String
        upToSpace = takeWhile (/= ' ')

md5' :: FilePath -> IO String
md5' path = (show . md5) <$> BL.readFile path

