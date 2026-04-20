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
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests FilterSpec {fsIncludes, fsExcludes} = foldr filter' ([], []) -- implementation with foldr, could also be done in a dumber way with inner recursive fn
  where
    filter' x (ys, zs) = if matchesAny False fsIncludes x && not (matchesAny False fsExcludes x) then (x : ys, zs) else (ys, x : zs)

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion _ TestCaseDefinition {tcdCategory = cat} (ByCategory fc) = cat == trimFilterId fc
matchesCriterion _ TestCaseDefinition {tcdTags = tags} (ByTag fc) = trimFilterId fc `elem` tags
matchesCriterion _ TestCaseDefinition {tcdCategory = cat, tcdTags = tags, tcdName = name} (ByAny fc) = and [cat == tfc, name == tfc, tfc `elem` tags]
  where
    tfc = trimFilterId fc

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
