library(epos)
context("test_createBaseTable")

test_that("Test function createBaseTable()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  utils::data(rawDrugNamesCoOcESSO, package="epos")
  utils::data(rawDrugNamesCoOcEPILONT, package="epos")
  neurotable <- createBaseTable(coocepso = rawDrugNamesCoOcEpSO[1:150],
    coocesso=rawDrugNamesCoOcESSO[1:150],
    coocepi=rawDrugNamesCoOcEPILONT[1:150])
  expect_that(length(neurotable), equals(14))
})