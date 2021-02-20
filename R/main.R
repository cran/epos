#' Main function to call everything and produce the results
#'
#' @param coocepso list of drug names sorted by frequency co-occuring with EpSO
#' @param coocesso list of drug names sorted by frequency co-occuring with ESSO
#' @param coocepi list of drug names sorted by frequency co-occuring with EPILONT
#' @param coocepisem list of drug names sorted by frequency co-occuring with EPISEM
#' @param coocfenics list of drug names sorted by frequency co-occuring with FENICS
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
#' utils::data(rawDrugNamesCoOcEPISEM, package="epos")
#' utils::data(rawDrugNamesCoOcFENICS, package="epos")
#' createBaseTable(coocepso = rawDrugNamesCoOcEpSO[1:150],
#'   coocesso=rawDrugNamesCoOcESSO[1:150],
#'   coocepi=rawDrugNamesCoOcEPILONT[1:150],
#'   coocepisem=rawDrugNamesCoOcEPISEM[1:150],
#'   coocfenics=rawDrugNamesCoOcFENICS[1:150])
createBaseTable <- function (coocepso, coocesso, coocepi, coocepisem, 
                             coocfenics) {
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
  tepisem <- coocepisem
  tfenics <- coocfenics
  
  neuroepso <- filterNeuroDrugs(tepso, atchashda)
  
  neuroesso <- filterNeuroDrugs(tesso, atchashda)
  
  neuroepi <- filterNeuroDrugs(tepi, atchashda)
  
  neuroepisem <- filterNeuroDrugs(tepisem, atchashda)
  
  neurofenics <- filterNeuroDrugs(tfenics, atchashda)
  
  ml <- min (c(length(neuroepso), 
               length(neuroesso), 
               length(neuroepi),
               length(neuroepisem),
               length(neurofenics)))
  
  dneuro <-
    data.frame(EpSO = neuroepso[1:ml],
               ESSO = neuroesso[1:ml],
               EPILONT = neuroepi[1:ml],
               EPISEM = neuroepisem[1:ml],
               FENICS = neurofenics[1:ml])
  
  dneuromaxk <- TopKLists::calculate.maxK(dneuro, 5, 5, 5)
  
  neurospace <- as.character(dneuromaxk$topkspace)
  
  neurotable <-
    createNeuroTable(atchashda, atchashsec, dneuromaxk)
  
  return (neurotable)
}