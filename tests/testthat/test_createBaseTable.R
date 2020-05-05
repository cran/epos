library(epos)
context("test_createBaseTable")

test_that("Test function createBaseTable()", {
  utils::data(rawDrugBankCoOcEpSO, package="epos")
  utils::data(rawDrugBankCoOcESSO, package="epos")
  utils::data(rawDrugBankCoOcEPILONT, package="epos")
  atchashda <-
  readAtcMapIntoHashMapDrugNamesAtcCodes(
    system.file("extdata", "db-atc.map", package = "epos"), "\t")
  atchashaa <-
    readAtcMapIntoHashMapAtcCodesAtcNames(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  atchashsec <-
    readSecondLevelATC(
      system.file("extdata", "drugbankatc-secondlevel.map", package = "epos"), "\t")
  tepso <- rawDrugBankCoOcEpSO
  tesso <- rawDrugBankCoOcESSO
  tepi <- rawDrugBankCoOcEPILONT
  lepso <- genDictListFromRawFreq(tepso)
  neuroepso <- filterNeuroDrugs(lepso, atchashda)
  lesso <- genDictListFromRawFreq(tesso)
  neuroesso <- filterNeuroDrugs(lesso, atchashda)
  lepi <- genDictListFromRawFreq(tepi)
  neuroepi <- filterNeuroDrugs(lepi, atchashda)
  dneuro <-
    data.frame(EpSO = neuroepso[1:210],
               ESSO = neuroesso[1:210],
               EPILONT = neuroepi[1:210])
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
  neurospace <- as.character(dneuromaxk$topkspace)
  neurotable <-
    createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk)
  expect_that(length(neurotable), equals(12))
})