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
#' utils::data(rawDrugNamesCoOcEpSO, package="epos")
#' utils::data(rawDrugNamesCoOcESSO, package="epos")
#' utils::data(rawDrugNamesCoOcEPILONT, package="epos")
#' createBaseTable(coocepso = rawDrugNamesCoOcEpSO[1:150],
#'   coocesso=rawDrugNamesCoOcESSO[1:150],
#'   coocepi=rawDrugNamesCoOcEPILONT[1:150])
createBaseTable <- function (coocepso, coocesso, coocepi) {
  atchashda <-
    readAtcMapIntoHashMapDrugNamesAtcCodes(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  
  atchashaa <-
    readAtcMapIntoHashMapAtcCodesAtcNames(
      system.file("extdata", "db-atc.map", package = "epos"), "\t")
  
  atchashsec <-
    readSecondLevelATC(
      system.file("extdata", "atc-secondlevel.map", package = "epos"), "\t")
  

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
    createNeuroTable(atchashda, atchashsec, dneuromaxk)
  
  return (neurotable)
}