library(epos)
context("test_filterApprovedDrugs")

test_that("Test function filterApprovedDrugs()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
    system.file("extdata", "db-atc.map", package = "epos"), "\t")
  tepso <- rawDrugNamesCoOcEpSO
  fepso <- filterApprovedDrugs(tepso, atchashda)
  expect_that(length(fepso), equals(2973))
})