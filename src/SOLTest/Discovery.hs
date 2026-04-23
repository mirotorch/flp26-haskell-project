-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import Control.Monad (filterM, forM)
import SOLTest.Types
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  -- implementation
  let tests = filter (\p -> takeExtension p == ".test") fullPaths
  currentDirTests <- mapM findCompanionFiles tests -- map tests in current directory, mapM won't shut down the program in case of error
  if recursive
    then do
      dirs <- filterM doesDirectoryExist fullPaths -- filter directories only
      nested' <- forM dirs (discoverTests True) -- recursive call
      return $ currentDirTests ++ concat nested'
    else
      return currentDirTests

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
