library(epos)
context("test_createBaseTable")

test_that("Test function createBaseTable()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  utils::data(rawDrugNamesCoOcESSO, package="epos")
  utils::data(rawDrugNamesCoOcEPILONT, package="epos")
  utils::data(rawDrugNamesCoOcEPISEM, package="epos")
  utils::data(rawDrugNamesCoOcFENICS, package="epos")
  neurotable <- createBaseTable(coocepso = rawDrugNamesCoOcEpSO[1:150],
    coocesso=rawDrugNamesCoOcESSO[1:150],
    coocepi=rawDrugNamesCoOcEPILONT[1:150],
    coocepisem=rawDrugNamesCoOcEPISEM[1:150],
    coocfenics=rawDrugNamesCoOcFENICS[1:150])
  expect_that(length(neurotable), equals(16))
})