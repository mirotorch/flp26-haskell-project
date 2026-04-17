-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests FilterSpec {fsIncludes, fsExcludes} (x : xs) = undefined
filterTests FilterSpec {fsIncludes, fsExcludes} [] = undefined

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- FLP: Implement this function. If you're not implementing the regex matching
-- bonus extension, you can either remove the first argument and update the usages,
-- or you can simply ignore the value.
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion _ TestCaseDefinition {tcdCategory = cat} (ByCategory fc) = cat == fc
matchesCriterion _ TestCaseDefinition {tcdTags = tags} (ByTag fc) = fc `elem` tags
matchesCriterion _ TestCaseDefinition {tcdCategory = cat, tcdTags = tags, tcdName = name} (ByAny fc) = and [cat == fc, name == fc, fc `elem` tags]

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
