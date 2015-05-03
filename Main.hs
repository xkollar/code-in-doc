module Main where

import Control.Exception.Base (bracket_)
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Text.Pandoc

rules :: [(String, FilePath -> IO ())]
rules =
    [ ("haskell", \ filename -> do
            d <- getHomeDirectory
            putStr "Compiling... "
            runCheck "ghc" ["-v0", "-Wall", "-Werror", "-fwarn-tabs", filename] >>= printResult
            putStr "Hlint... "
            runCheck "hlint" ["--color", "--hint", d </> ".ghc/HLint.hs", filename] >>= printResult'
      )
    ]

data MyFile = MyFile Attr String deriving Show

runCheck :: FilePath -> [String] -> IO Bool
runCheck c a = do
    (_, _, _, ph) <- createProcess (proc c a)
    r <- waitForProcess ph
    return $ r == ExitSuccess

printResult :: Bool -> IO ()
printResult b = if b then putStrLn "OK" else putStrLn "FAIL" >> exitFailure

printResult' :: Bool -> IO ()
printResult' b = unless b $ putStrLn "FAIL" >> exitFailure

getCode :: Block -> [MyFile]
getCode (CodeBlock ctx content) = [MyFile ctx content]
getCode _ = []

maybeDo :: Monad m => Maybe t -> (t -> m ()) -> m ()
maybeDo (Just x) f = f x
maybeDo _ _ = return ()

inDirectory :: FilePath -> IO a -> IO a
inDirectory d f = do
    dir <- getCurrentDirectory
    bracket_ (setCurrentDirectory d) (setCurrentDirectory dir) f

process :: MyFile -> IO ()
process (MyFile (_, s, m) content) =
    maybeDo (lookup "file" m) $ \ filename -> do
        putStr $ "Extracting " ++ show filename ++ "... "
        when (isAbsolute filename) $ putStrLn "Won't extract file to absolute path!" >> exitFailure
        createDirectoryIfMissing True (takeDirectory filename)
        writeFile filename content
        putStrLn "done"
        forM_ s $ \ x -> maybeDo (lookup x rules) $ \ rule -> rule filename

main :: IO ()
main = do
    [filename] <- getArgs
    files <- fmap (queryWith getCode . readMarkdown def) $ readFile filename
    hSetBuffering stdout NoBuffering
    withSystemTempDirectory "doc-build-test" $ \ d -> inDirectory d $ mapM_ process files
