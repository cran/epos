library(epos)
context("test_createNeuroTable")

test_that("Test function createNeuroTable()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  utils::data(rawDrugNamesCoOcESSO, package="epos")
  utils::data(rawDrugNamesCoOcEPILONT, package="epos")
  nt <- createNeuroTable(coocepso = rawDrugNamesCoOcEpSO,
    coocesso=rawDrugNamesCoOcESSO,
    coocepi=rawDrugNamesCoOcEPILONT)
  expect_that(length(nt), equals(12))
})