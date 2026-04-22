-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions results = foldl' addTestToMap Map.empty definitions
  where
    addTestToMap acc def =
      let category = tcdCategory def
          points = tcdPoints def

          mReport = Map.lookup (tcdName def) results

          passedPoints = case mReport of
            Just r | tcrResult r == Passed -> points
            _ -> 0

          newEntry =
            CategoryReport
              { crTotalPoints = points,
                crPassedPoints = passedPoints,
                crTestResults = maybe Map.empty (Map.singleton (tcdName def)) mReport
              }
       in Map.insertWith combineReports category newEntry acc

    combineReports new old =
      CategoryReport
        { crTotalPoints = crTotalPoints new + crTotalPoints old,
          crPassedPoints = crPassedPoints new + crPassedPoints old,
          crTestResults = Map.union (crTestResults new) (crTestResults old)
        }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults =
  TestStats
    { tsFoundTestFiles = foundCount,
      tsLoadedTests = loadedCount,
      tsSelectedTests = selectedCount,
      tsPassedTests = totalPassed,
      tsHistogram = histogram
    }
  where
    catMap = fromMaybe Map.empty mCategoryResults
    totalPassed = Map.foldl' passedInCat 0 catMap
      where
        passedInCat acc report =
          let results = crTestResults report
              numPassed = Map.size $ Map.filter (\r -> tcrResult r == Passed) results
           in acc + numPassed
    histogram = computeHistogram catMap

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram = Map.foldl' updateHistogram bins -- eta reduced
  where
    bins = Map.fromList [("0." ++ show i, 0) | i <- [(0 :: Integer) .. 9]]
    updateHistogram acc report =
      let results = crTestResults report
          total = Map.size results
          passed = Map.size $ Map.filter (\r -> tcrResult r == Passed) results

          rate =
            if total == 0
              then 0.0
              else fromIntegral passed / fromIntegral total

          bin = rateToBin rate
       in Map.adjust (+ 1) bin acc

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
