#' Create the final resulting data frame
#'
#' @param neurospace list of drug names that were aggregated using TopKLists::calculate.max topkspace
#' @param atchashda hashmap retrieved from readAtcMapIntoHashMapDrugNamesAtcCodes
#' @param atchashsec hashmap retrieved from readSecondLevelATC
#' @param dneuromaxk data frame containing columns for each intersection, ATC class, and reference list
#'
#' @return data frame containing drug names with additional columns listing association to ATC classes
#' 
#' @export
#'
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' utils::data(rawDrugBankCoOcESSO, package="epos")
#' utils::data(rawDrugBankCoOcEPILONT, package="epos")
#' atchashda <-
#' readAtcMapIntoHashMapDrugNamesAtcCodes(
#'   system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' atchashaa <-
#'   readAtcMapIntoHashMapAtcCodesAtcNames(
#'     system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' atchashsec <-
#'   readSecondLevelATC(
#'     system.file("extdata", "drugbankatc-secondlevel.map", package = "epos"), "\t")
#' tepso <- rawDrugBankCoOcEpSO[1:150]
#' tesso <- rawDrugBankCoOcESSO[1:150]
#' tepi <- rawDrugBankCoOcEPILONT[1:150]
#' lepso <- genDictListFromRawFreq(tepso)
#' neuroepso <- filterNeuroDrugs(lepso, atchashda)
#' lesso <- genDictListFromRawFreq(tesso)
#' neuroesso <- filterNeuroDrugs(lesso, atchashda)
#' lepi <- genDictListFromRawFreq(tepi)
#' neuroepi <- filterNeuroDrugs(lepi, atchashda)
#' dneuro <-
#'   data.frame(EpSO = neuroepso[1:15],
#'              ESSO = neuroesso[1:15],
#'              EPILONT = neuroepi[1:15])
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
#' neurospace <- as.character(dneuromaxk$topkspace)
#' neurotable <-
#'   createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk)
createBaseTable <- function (neurospace, atchashda, atchashsec, dneuromaxk) {
  i_epso_esso_epi <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EpSO_ESSO), "\\*", ""), ", "))
  i_epso_epi      <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EpSO), "\\*", ""), ", "))
  i_esso_epi      <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_ESSO), "\\*", ""), ", "))
  i_epso_esso     <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EpSO_ESSO), "\\*", ""), ", "))
  i_epi           <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT), "\\*", ""), ", "))
  i_epso          <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EpSO), "\\*", ""), ", "))
  i_esso          <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$ESSO), "\\*", ""), ", "))
  
  neurotopk <- c(intersect(neurospace, i_epso_esso_epi), setdiff(i_epso_esso_epi, neurospace), # 29
                 intersect(neurospace, i_epso_esso), setdiff(i_epso_esso, neurospace),     # 7
                 intersect(neurospace, i_epso), setdiff(i_epso, neurospace),          # 2
                 intersect(neurospace, i_epso_epi), setdiff(i_epso_epi, neurospace),      # 1
                 intersect(neurospace, i_epi), setdiff(i_epi, neurospace),           # 7
                 intersect(neurospace, i_esso_epi), setdiff(i_esso_epi, neurospace),      # 2
                 intersect(neurospace, i_esso), setdiff(i_esso, neurospace))
  
  
  lancet <- c(
    "Carbamazepine",
    "Gabapentin",
    "Lamotrigine",
    "Levetiracetam",
    "Oxcarbazepine",
    "Phenobarbital",
    "Phenytoin",
    "Topiramate",
    "Valproic Acid"
  )
  
  drugse <- c(
    "Lorazepam",
    "Diazepam",
    "Clonazepam",
    "Midazolam",
    "Phenytoin",
    "Valproic Acid",
    "Levetiracetam",
    "Phenobarbital",
    "Propofol",
    "Thiopental",
    "Pentobarbital",
    "Isoflurane",
    "Etomidate"
  )
  # neurotopk <- neurospace[1:39]
  rnames <- ""
  for (drug in neurotopk) {
    if (drug %in% i_epso_esso_epi) {
      rnames <- c(rnames, "EpSO_ESSO_EPILONT")
    } else if (drug %in% i_epso_esso) {
      rnames <- c(rnames, "EpSO_ESSO")
    } else if (drug %in% i_epso_epi) {
      rnames <- c(rnames, "EpSO_EPILONT")
    } else if (drug %in% i_esso_epi) {
      rnames <- c(rnames, "ESSO_EPILONT")
    } else if (drug %in% i_epso) {
      rnames <- c(rnames, "EpSO")
    } else if (drug %in% i_esso) {
      rnames <- c(rnames, "ESSO")
    } else if (drug %in% i_epi) {
      rnames <- c(rnames, "EPILONT")
    }
  }
  rnames <- rnames [2:length(rnames)]
  
  #rnames <- c(
   # rep("EpSO_ESSO_EPILONT", length(i_epso_esso_epi)),
  #  rep("EpSO_ESSO", length(i_epso_esso)),
  #  rep("EpSO", length(i_epso)),
  #  rep("EpSO_EPILONT", length(i_epso_epi)),
  #  rep("EPILONT", length(i_epi)),
  #  rep("ESSO_EPILONT", length(i_esso_epi)),
  #  rep("ESSO", length(i_esso))
  #)
  ranking <- rep("", length(neurotopk))
  lanc <- rep ("", length(neurotopk))
  dse <- rep ("", length(neurotopk))
  counter <- 1
  counter <- 1
  for (d in neurotopk) {
    # ranking position
    if (length(which(neurospace == d)) > 0) {
      ranking[counter] <- which(neurospace == d)
    }
    if (length(which(lancet == d)) > 0) {
      lanc[counter] <- "X"
    }
    if (length(which(drugse == d)) > 0) {
      dse[counter] <- "X"
    }
    counter <- counter + 1
  }
  
  
  #ranking <- ranking[2:length(ranking)]
  dntk <- data.frame(
    Rank=ranking,
    Intersection=rnames,
    DrugName=neurotopk,
    N03=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N03"),
    N05=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N05"),
    N06=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N06"),
    N01=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N01"),
    N02=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N02"),
    N04=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N04"),
    N07=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N07"),
    Lancet=lanc,
    DSE=dse
  )
  return (dntk)
}

#' Creates a vector with an X at each position where a drug from the druglist matches the ATC class list slatc
#'
#' @param druglist list of drug names
#' @param atchashda hash retrieved from readAtcMapIntoHashMapDrugNamesAtcCodes
#' @param atchashsec hash retrieved from readSecondLevelATC
#' @param slatc list of ATC classes
#'
#' @return list with crosses if the drug in druglist matches at the position of the ATC class in slatc
#' @export
#'
#' @examples
#' \dontrun{
#' createDashVectorForATC(druglist, atchashda, atchashsec, slatc)
#' }
createDashVectorForATC <- function (druglist, atchashda, atchashsec, slatc) {
  counter <- 0
  for (n in druglist) {
    atcc <- substr(atchashda[[n]], 1, 3)
    atcn <- atchashsec[[substr(atchashda[[n]], 1, 3)]]
    
    if (counter == 0) {
      if (atcc == slatc) {
        al <- "x"
      } else {
        al <- ""
      }
      counter <- counter + 1
    } else {
      if (atcc == slatc) {
        al <- c(al, "X")
      } else {
        al <- c(al, "")
      }
    }
   
  }
  return (al)
}
