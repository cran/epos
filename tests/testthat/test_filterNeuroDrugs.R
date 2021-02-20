library(epos)
context("test_filterNeuroDrugs")

test_that("Test function filterNeuroDrugs()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
    system.file("extdata", "db-atc.map", package = "epos"), "\t")
  tepso <- rawDrugNamesCoOcEpSO
  nepso <- filterNeuroDrugs(tepso, atchashda)
  expect_that(length(nepso), equals(465))
})