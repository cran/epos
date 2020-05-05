#' Main function to call everything and produce the results
#'
#' @param coocepso list of drug names sorted by frequency co-occuring with EpSO
#' @param coocesso list of drug names sorted by frequency co-occuring with ESSO
#' @param coocepi list of drug names sorted by frequency co-occuring with EPILONT
#'
#' @return result table containin the aggregated list of drug terms and their associations
#'
#' @importFrom TopKLists calculate.maxK
#' @importFrom xtable xtable
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' utils::data(rawDrugBankCoOcESSO, package="epos")
#' utils::data(rawDrugBankCoOcEPILONT, package="epos")
#' createNeuroTable(coocepso = rawDrugBankCoOcEpSO[1:150], 
#'   coocesso=rawDrugBankCoOcESSO[1:150],
#'   coocepi=rawDrugBankCoOcEPILONT[1:150])
createNeuroTable <- function (coocepso, coocesso, coocepi) {
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  
  atchashaa <-
    readAtcMapIntoHashMapAtcCodesAtcNames(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  
  atchashsec <-
    readSecondLevelATC(
      system.file("extdata", "drugbankatc-secondlevel.map", package = "epos"), "\t")
  

  tepso <- coocepso
  tesso <- coocesso
  tepi <- coocepi
  
  lepso <- genDictListFromRawFreq(tepso)
  neuroepso <- filterNeuroDrugs(lepso, atchashda)
  
  lesso <- genDictListFromRawFreq(tesso)
  neuroesso <- filterNeuroDrugs(lesso, atchashda)
  
  lepi <- genDictListFromRawFreq(tepi)
  neuroepi <- filterNeuroDrugs(lepi, atchashda)
  
  ml <- min (c(length(neuroepso), length(neuroesso), length(neuroepi)))
  
  dneuro <-
    data.frame(EpSO = neuroepso[1:ml],
               ESSO = neuroesso[1:ml],
               EPILONT = neuroepi[1:ml])
  
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
  
  neurospace <- as.character(dneuromaxk$topkspace)
  
  neurotable <-
    createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk)
  
  return (neurotable)
}