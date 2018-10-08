#' Processes the input file db-atc.map to form a HashMap containing the drug names with ATC codes
#'
#' @param filename character vector with the file name of the file db-atc.map
#' @param seperator character vector with the seperator used within the map-file
#'
#' @return atchashda hashmap with drug names as keys and atc codes as values 
#' 
#' @importFrom utils read.csv
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' atchashda <- readAtcMapIntoHashMapDrugNamesAtcCodes(
#'   system.file("extdata", "db-atc.map", package = "epos"), "\t")
readAtcMapIntoHashMapDrugNamesAtcCodes <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = filename, sep = seperator)
  drugnames <- atcmap[,4]
  atccodes <- atcmap[,2]
  catccodes <- as.character(atccodes)
  cdrugnames <- as.character(drugnames)
  atchashda <- hashmap::hashmap(cdrugnames, catccodes)
  return (atchashda)
}

#' Processes the input file db-atc.map to form a HashMap containing the drug names with ATC codes
#'
#' @param filename character vector with the file name of the file db-atc.map
#' @param seperator character vector with the seperator used within the map-file
#' @importFrom hashmap hashmap
#' @return atchashaa hashmap with atc codes as keys and atc names as values 
#' @export
#'
#' @examples
#' atchashaa <-
#'   readAtcMapIntoHashMapAtcCodesAtcNames(
#'     system.file("extdata", "db-atc.map", package = "epos"), "\t")
readAtcMapIntoHashMapAtcCodesAtcNames <-  function (filename, seperator) {
  atcmap <- utils::read.csv(file = filename, sep = seperator)
  atcnames <- atcmap[,3]
  atccodes <- atcmap[,2]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashaa <- hashmap::hashmap(catccodes, catcnames)
  return (atchashaa)
}

#' Filter a given list of drug names for having an ATC code, if not they are dropped
#'
#' @param druglist a list of drug names
#' @param atchashda a hashmap containing the drug names as keys
#' @importFrom hashmap hashmap
#' @return approveddrugs a hashmap filtered for having an ATC code
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' atchashda <-
#'   readAtcMapIntoHashMapDrugNamesAtcCodes(
#'   system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' tepso <- genDictListFromRawFreq(rawDrugBankCoOcEpSO)
#' filterApprovedDrugs(tepso, atchashda)
filterApprovedDrugs <- function (druglist, atchashda) {
  counter = 0
  for (drug in druglist) {
    if (drug %in% atchashda$keys()) {
      if (counter == 0) {
        approveddrugs <- drug
      } else {
        approveddrugs <- c(approveddrugs, drug)
      }
    }
    counter = counter + 1
  }
  
  return(approveddrugs)
}

#' Filter a given list of drug names for having an ATC code starting with N indicating to be a 
#' drug for the Nervous System
#'
#' @param druglist a list of drug names
#' @param atchashda a hashmap containing the drug names as keys
#'
#' @return neurodrugs a hashmap filtered for having an ATC code starting with N
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' atchashda <-
#'   readAtcMapIntoHashMapDrugNamesAtcCodes(
#'   system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' tepso <- genDictListFromRawFreq(rawDrugBankCoOcEpSO)
#' nepso <- filterNeuroDrugs(tepso, atchashda)
filterNeuroDrugs <- function (druglist, atchashda) {
  counter = 0
  for (drug in druglist) {
    if (drug %in% atchashda$keys()) {
      atccode <- atchashda$find(drug)
      if (startsWith(atccode, "N")) {
        if (counter == 0) {
          neurodrugs <- drug
        } else {
          neurodrugs <- c(neurodrugs, drug)
        }
        counter = counter + 1
      }
    }
  }
  if (counter != 0) {
    return(neurodrugs)
  }
}

#' Read the second level ATC classes from the file drugbankatc-secondlevel.map
#'
#' @param filename the file name that is supposed to be drugbankatc-secondlevel.map
#' @param seperator the csv file delimiter
#'
#' @return atchashsec a hashmap with second level ATC classes as keys and their names as values
#' 
#' @importFrom utils read.csv
#' @importFrom hashmap hashmap
#' 
#' @export
#'
#' @examples
#' atchashsec <-
#'   readSecondLevelATC(
#'   system.file("extdata", "drugbankatc-secondlevel.map", package = "epos"), "\t")
readSecondLevelATC <- function (filename, seperator) {
  secondatc <- utils::read.csv(file = filename, sep = seperator)
  atcnames <- secondatc[,2]
  atccodes <- secondatc[,1]
  catccodes <- as.character(atccodes)
  catcnames <- as.character(atcnames)
  atchashsec <- hashmap::hashmap(catccodes, catcnames)
  return (atchashsec)
}

#' Clears object that was loaded from harddrive into a list of terms sorted by frequency
#'
#' @param topfreqdictraw list with terms from a dictionary sorted by frequency
#'
#' @return a sorted list of terms
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' genDictListFromRawFreq(rawDrugBankCoOcEpSO)
genDictListFromRawFreq <- function (topfreqdictraw) {
  # remove last element from all lists because it is falsely a 1 from previous processing
  la = length(topfreqdictraw)
  topfreqdictraw <- topfreqdictraw[-la]
  a <- attributes(topfreqdictraw)$names
  a
}