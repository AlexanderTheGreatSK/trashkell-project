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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SOLTest.Types
    ( CategoryReport(..),
      TestCaseDefinition(tcdPoints, tcdName, tcdCategory),
      TestCaseReport(tcrResult),
      TestReport(..),
      TestStats (TestStats, tsFoundTestFiles, tsLoadedTests, tsSelectedTests, tsPassedTests, tsHistogram),
      UnexecutedReason,
      TestResult(Passed)
    )

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
-- This function was partly made by LLM, but I coded parts that were missing, and then it obv was not working
-- so I was debugging it with my LLM helper - really nasty function
groupByCategory :: [TestCaseDefinition] -> Map String TestCaseReport -> Map String CategoryReport
groupByCategory definitions = Map.foldlWithKey' accumulate Map.empty
  where
    defMap = Map.fromList [(tcdName d, d) | d <- definitions]
    accumulate acc testName report = 
      case Map.lookup testName defMap of
        Nothing  -> acc
        Just def -> Map.insertWith (merge def report testName) (tcdCategory def) (fresh def report testName) acc
      
    fresh def report testName = CategoryReport
      { 
        crTotalPoints  = tcdPoints def,
        crPassedPoints = points def report,
        crTestResults  = Map.singleton testName report
      }
    merge def report testName _ old = old
      { 
        crTotalPoints  = crTotalPoints old + tcdPoints def,
        crPassedPoints = crPassedPoints old + points def report,
        crTestResults  = Map.insert testName report (crTestResults old)
      }
    points def report = if tcrResult report == Passed then tcdPoints def else 0

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
-- This function was done by me :)
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
    {
      tsFoundTestFiles = foundCount,
      tsLoadedTests = loadedCount,
      tsSelectedTests = selectedCount,
      tsPassedTests = maybe 0 (sum . map crPassedPoints . Map.elems) mCategoryResults,
      tsHistogram = maybe Map.empty computeHistogram mCategoryResults
    }

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
--
-- This function is all done by LLM friend. I just pasted this here so I am NOT author. It was not one shot print, I helped to debug this, but I am not he author.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram = Map.foldl' accumulate emptyHistogram
  where
    emptyHistogram = Map.fromList [(show 0 ++ "." ++ show n, 0) | n <- [0..9]]
    binFor report = rateToBin rate
      where
        total  = crTotalPoints report
        passed = crPassedPoints report
        rate   = if total == 0
                   then 0.0
                   else fromIntegral passed / fromIntegral total :: Double
    accumulate :: Map String Int -> CategoryReport -> Map String Int
    accumulate hist report = Map.insertWith (+) (binFor report) 1 hist


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
