library(epos)
context("test_genDictListFromRawFreq")

test_that("Test function genDictListFromRawFreq()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  tepso <- genDictListFromRawFreq(rawDrugNamesCoOcEpSO)
  expect_that(length(tepso), equals(4745))
})