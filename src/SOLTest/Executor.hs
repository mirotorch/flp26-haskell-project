-- | Executing test cases by running external parser and interpreter processes.
--
-- Each test case is executed according to its 'TestCaseType':
--
-- * 'ParseOnly': run the parser with source on stdin, check exit code.
-- * 'ExecuteOnly': write XML to a temp file, run the interpreter, check
--   exit code, optionally diff stdout against @.out@.
-- * 'Combined': run the parser first (must exit 0), write its output to a
--   temp file, then run the interpreter as in 'ExecuteOnly'.
module SOLTest.Executor
  ( executeTest,
    runParser,
    runInterpreter,
    runDiff,
  )
where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe)
import SOLTest.Types
import System.Directory (doesFileExist, executable, getPermissions)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Execute a single test case and return a @TestCaseReport@.
--
-- Returns @Left UnexecutedReason@ when execution cannot proceed (e.g.
-- the required executable is missing or not executable).
executeTest ::
  -- | Path to the parser executable (required for 'ParseOnly' and 'Combined').
  Maybe FilePath ->
  -- | Path to the interpreter executable (required for 'ExecuteOnly' and 'Combined').
  Maybe FilePath ->
  TestCaseDefinition ->
  IO (Either UnexecutedReason TestCaseReport)
executeTest mParser mInterp test =
  case tcdTestType test of
    ParseOnly ->
      withExecutable mParser $ \parserPath ->
        Right <$> executeParseOnly parserPath test
    ExecuteOnly ->
      withExecutable mInterp $ \interpPath ->
        Right <$> executeExecuteOnly interpPath test
    Combined ->
      withExecutable mParser $ \parserPath ->
        withExecutable mInterp $ \interpPath ->
          Right <$> executeCombined parserPath interpPath test

-- ---------------------------------------------------------------------------
-- Per-type execution
-- ---------------------------------------------------------------------------

-- | Execute a 'ParseOnly' test case.
executeParseOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeParseOnly parserPath test = do
  (exitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test)
  let code = exitCodeToInt exitCode
      result
        | code `elem` expectedCodes = Passed
        | otherwise = ParseFail
      expectedCodes = fromMaybe [] (tcdExpectedParserExitCodes test)
  return
    TestCaseReport
      { tcrResult = result,
        tcrParserExitCode = Just code,
        tcrInterpreterExitCode = Nothing,
        tcrParserStdout = Just pOut,
        tcrParserStderr = Just pErr,
        tcrInterpreterStdout = Nothing,
        tcrInterpreterStderr = Nothing,
        tcrDiffOutput = Nothing
      }

-- | Execute an 'ExecuteOnly' test case.
executeExecuteOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeExecuteOnly interpPath test =
  withTempSource (tcdSourceCode test) $ \tmpPath -> do
    (exitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
    let code = exitCodeToInt exitCode
        expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
    (result, diffOut) <- checkInterpreterResult code expectedCodes iOut (tcdExpectedStdoutFile test)
    return
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Nothing,
          tcrInterpreterExitCode = Just code,
          tcrParserStdout = Nothing,
          tcrParserStderr = Nothing,
          tcrInterpreterStdout = Just iOut,
          tcrInterpreterStderr = Just iErr,
          tcrDiffOutput = diffOut
        }

-- | Execute a 'Combined' test case.
executeCombined :: FilePath -> FilePath -> TestCaseDefinition -> IO TestCaseReport
executeCombined parserPath interpPath test =
  withTempSource (tcdSourceCode test) $
    \tmpPath -> do
      (pExitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test) -- run parser first
      let pCode = exitCodeToInt pExitCode
          pExpectedCodes = fromMaybe [] (tcdExpectedParserExitCodes test)

      if not $ pCode `elem` pExpectedCodes -- if parser failed we end execution
        then
          return
            TestCaseReport
              { tcrResult = ParseFail,
                tcrParserExitCode = Just pCode,
                tcrInterpreterExitCode = Nothing,
                tcrParserStdout = Just pOut,
                tcrParserStderr = Just pErr,
                tcrInterpreterStdout = Nothing,
                tcrInterpreterStderr = Nothing,
                tcrDiffOutput = Nothing
              }
        else do
          (iExitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test) -- run interpreter
          let iCode = exitCodeToInt iExitCode
              iExpectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
          (result, diffOut) <- checkInterpreterResult iCode iExpectedCodes iOut (tcdExpectedStdoutFile test)
          return
            TestCaseReport -- return combined report
              { tcrResult = result,
                tcrParserExitCode = Just pCode,
                tcrInterpreterExitCode = Just iCode,
                tcrParserStdout = Just pOut,
                tcrParserStderr = Just pErr,
                tcrInterpreterStdout = Just iOut,
                tcrInterpreterStderr = Just iErr,
                tcrDiffOutput = diffOut
              }

-- ---------------------------------------------------------------------------
-- Process wrappers
-- ---------------------------------------------------------------------------

-- | Run the SOL26 parser by feeding @sourceCode@ on its stdin.
--
-- Returns @(exitCode, stdout, stderr)@.
runParser :: FilePath -> String -> IO (ExitCode, String, String)
runParser parserPath = readCreateProcessWithExitCode (proc parserPath [])

-- | Run the interpreter with @--source \<xmlFile\>@ and, optionally,
-- @--input \<stdinFile\>@.
--
-- Returns @(exitCode, stdout, stderr)@.
runInterpreter ::
  FilePath ->
  FilePath ->
  Maybe FilePath ->
  IO (ExitCode, String, String)
runInterpreter interpPath xmlFile mInputFile = do
  let args = ["--source", xmlFile] ++ maybe [] (\f -> ["--input", f]) mInputFile
  readCreateProcessWithExitCode (proc interpPath args) ""

-- | Run GNU @diff@ between two files (no additional flags).
--
-- Returns @(exitCode, diffOutput)@. Exit code 0 means no differences;
-- exit code 1 means differences were found.
runDiff :: FilePath -> FilePath -> IO (ExitCode, String)
runDiff actualFile expectedFile = do
  (exitCode, out, _) <- readCreateProcessWithExitCode (proc "diff" [actualFile, expectedFile]) ""
  return (exitCode, out)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check the interpreter's result and optionally run diff.
--
-- Runs diff only when the interpreter exited with code 0 AND a @.out@ file
-- is present.
checkInterpreterResult ::
  -- | Actual interpreter exit code.
  Int ->
  -- | Expected interpreter exit codes.
  [Int] ->
  -- | Interpreter stdout.
  String ->
  -- | Path to the @.out@ file, if present.
  Maybe FilePath ->
  IO (TestResult, Maybe String)
-- case when we need to run diff
checkInterpreterResult 0 expectedCodes iOut (Just mOutFile) =
  if 0 `notElem` expectedCodes then return (IntFail, Nothing) else runDiffOnOutput iOut mOutFile
-- otherwise simply check the code
checkInterpreterResult actualCode expectedCodes _ _
  | actualCode `elem` expectedCodes = return (Passed, Nothing)
  | otherwise = return (IntFail, Nothing)

-- | Write a string to a temporary file and pass its path to an action.
-- The file is deleted when the action returns.
withTempSource :: String -> (FilePath -> IO a) -> IO a
withTempSource content action =
  withSystemTempFile "sol-source.xml" $ \tmpPath tmpHandle -> do
    hPutStr tmpHandle content
    hClose tmpHandle
    action tmpPath

-- | Write the interpreter stdout to a temp file and diff it against @.out@.
-- The file is deleted when the action returns.
runDiffOnOutput :: String -> FilePath -> IO (TestResult, Maybe String)
runDiffOnOutput iOut outFile = withSystemTempFile "sol-source.xml" $ \tmpPath tmpHandle -> do
  hPutStr tmpHandle iOut
  hClose tmpHandle -- create and write tmp file just like in withTempSource
  (code, out) <- runDiff tmpPath outFile -- run diff on our files
  if exitCodeToInt code == 0 then return (Passed, Just out) else return (DiffFail, Nothing)

-- | Ensure an executable path is provided and the file is executable,
-- then run an action with it.  Returns 'Left' 'CannotExecute' if the
-- path is missing or the file is not executable.
withExecutable ::
  Maybe FilePath ->
  (FilePath -> IO (Either UnexecutedReason TestCaseReport)) ->
  IO (Either UnexecutedReason TestCaseReport)
withExecutable Nothing _ =
  return
    ( Left
        UnexecutedReason
          { urCode = CannotExecute,
            urMessage = Just "Required executable path was not provided"
          }
    )
withExecutable (Just path) action = do
  check <- checkExecutable path
  case check of
    Just reason -> return (Left reason)
    Nothing -> action path

-- | Check that a file exists and has its executable bit set.
-- The IO action returns 'Nothing' if the file is usable, or 'Just'
-- an 'UnexecutedReason' describing the problem.
checkExecutable :: FilePath -> IO (Maybe UnexecutedReason)
checkExecutable path = do
  result <- try (doesFileExist path) :: IO (Either IOException Bool) -- does file exist?
  case result of
    Left err -> return (Just (UnexecutedReason CannotExecute (Just (show err)))) -- error, returning exception description
    Right False -> return (Just (UnexecutedReason CannotExecute (Just "File does not exist"))) -- doesn't exist, return CannotExecute
    Right True -> do
      -- file exists
      permissions <- getPermissions path -- get file permissions and check executable bit
      if executable permissions then return Nothing else return (Just (UnexecutedReason CannotExecute (Just "Missing permission")))

-- | Convert 'ExitCode' to an 'Int'.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
