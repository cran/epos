library(epos)
context("test_createNeuroTable")

test_that("Test function createNeuroTable()", {
  utils::data(rawDrugNamesCoOcEpSO, package="epos")
  utils::data(rawDrugNamesCoOcESSO, package="epos")
  utils::data(rawDrugNamesCoOcEPILONT, package="epos")
  utils::data(rawDrugNamesCoOcEPISEM, package="epos")
  utils::data(rawDrugNamesCoOcFENICS, package="epos")
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  atchashaa <-
    readAtcMapIntoHashMapAtcCodesAtcNames(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  atchashsec <-
    readSecondLevelATC(
      system.file("extdata", "atc-secondlevel.map", package = "epos"), "\t")
  tepso <- rawDrugNamesCoOcEpSO
  tesso <- rawDrugNamesCoOcESSO
  tepi <- rawDrugNamesCoOcEPILONT
  tepisem <- rawDrugNamesCoOcEPISEM
  tfenics <- rawDrugNamesCoOcFENICS

  neuroepso <- filterNeuroDrugs(tepso, atchashda)
  neuroesso <- filterNeuroDrugs(tesso, atchashda)
  neuroepi <- filterNeuroDrugs(tepi, atchashda)
  neuroepisem <- filterNeuroDrugs(tepisem, atchashda)
  neurofenics <- filterNeuroDrugs(tfenics, atchashda)
  
  dneuro <-
    data.frame(EpSO = neuroepso[1:210],
               ESSO = neuroesso[1:210],
               EPILONT = neuroepi[1:210],
               EPISEM = neuroepisem[1:210],
               FENICS = neurofenics[1:210])
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 5, 5, 5)
  neurospace <- as.character(dneuromaxk$topkspace)
  neurotable <- createNeuroTable(atchashda, atchashsec, dneuromaxk)
  expect_that(length(neurotable), equals(16))
})