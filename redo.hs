
import Control.Monad ( filterM )
import Data.Maybe ( listToMaybe )
import System.Directory ( renameFile, removeFile, doesFileExist )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(..) )
import System.FilePath ( hasExtension, replaceBaseName, takeBaseName )
import System.IO ( hPutStrLn, stderr )
import System.Process ( createProcess, waitForProcess, shell )

main :: IO ()
main = getArgs >>= mapM_ redo

redo :: String -> IO ()
redo target = redoPath target >>= maybe printMissing redo'
  where printMissing :: IO ()
        printMissing = error ("No .do file found for target '" ++ target ++ "'")

        tmp = target ++ "---redoing"

        command path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

        redo' :: FilePath -> IO ()
        redo' path = do
          (_, _, _, ph) <- (createProcess . shell) (command path)
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> renameFile tmp target
            ExitFailure code -> do hPutStrLn stderr ("Redo script exited with non-zero exit code: " ++ show code)
                                   removeFile tmp

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe <$> filterM doesFileExist candidates
  where candidates = (target ++ ".do") : if hasExtension target
                                         then [replaceBaseName target "default" ++ ".do"]
                                         else []

