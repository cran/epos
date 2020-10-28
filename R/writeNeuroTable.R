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
#' utils::data(rawDrugNamesCoOcEpSO, package="epos")
#' utils::data(rawDrugNamesCoOcESSO, package="epos")
#' utils::data(rawDrugNamesCoOcEPILONT, package="epos")
#' utils::data(rawDrugNamesCoOcEPISEM, package="epos")
#' utils::data(rawDrugNamesCoOcFENICS, package="epos")
#' atchashda <-
#' readAtcMapIntoHashMapDrugNamesAtcCodes(
#'   system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' atchashaa <-
#'   readAtcMapIntoHashMapAtcCodesAtcNames(
#'     system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' atchashsec <-
#'   readSecondLevelATC(
#'     system.file("extdata", "atc-secondlevel.map", package = "epos"), "\t")
#' epso <- genDictListFromRawFreq(rawDrugNamesCoOcEpSO)
#' neuroepso <- filterNeuroDrugs(epso, atchashda)
#' esso <- genDictListFromRawFreq(rawDrugNamesCoOcESSO)
#' neuroesso   <- filterNeuroDrugs(esso, atchashda)
#' epi <- genDictListFromRawFreq(rawDrugNamesCoOcEPILONT)
#' neuroepi    <- filterNeuroDrugs(epi, atchashda)
#' episem <- genDictListFromRawFreq(rawDrugNamesCoOcEPISEM)
#' neuroepisem <- filterNeuroDrugs(episem, atchashda)
#' fenics <- genDictListFromRawFreq(rawDrugNamesCoOcFENICS)
#' neurofenics <- filterNeuroDrugs(fenics, atchashda)
#' mx <- max(
#'     c(length(neuroepso), length(neuroesso), length(neuroepi),
#'       length(neuroepisem), length(neurofenics)))
#' dneuro <-
#'   data.frame(EpSO = c(neuroepso, rep(0, (mx-length(neuroepso)))),
#'              ESSO = c(neuroesso, rep(0, (mx-length(neuroesso)))),
#'              EPILONT = c(neuroepi, rep(0, (mx-length(neuroepi)))),
#'              EPISEM = c(neuroepisem, rep(0, (mx-length(neuroepisem)))),
#'              FENICS = c(neurofenics, rep(0, (mx-length(neurofenics)))))
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, 5, 5, 10)
#' neurospace <- as.character(dneuromaxk$topkspace)
#' neurotable <-
#'   createBaseTable(neurospace, atchashda, atchashsec, dneuromaxk)
createBaseTable <- function (neurospace, atchashda, atchashsec, dneuromaxk) {
  i_EPILONT_EPISEM_EpSO_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EPISEM_EpSO_ESSO), "\\*", ""), ", "))
  i_EPILONT_EPISEM_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EPISEM_ESSO), "\\*", ""), ", "))
  i_EPILONT_EpSO_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EpSO_ESSO), "\\*", ""), ", "))
  i_EPISEM_EpSO_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPISEM_EpSO_ESSO), "\\*", ""), ", "))
  i_EPILONT_EPISEM <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EPISEM), "\\*", ""), ", "))
  i_EPILONT_EpSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EpSO), "\\*", ""), ", "))
  i_EPILONT_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_ESSO), "\\*", ""), ", "))
  i_EpSO_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EpSO_ESSO), "\\*", ""), ", "))
  i_EPILONT <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT), "\\*", ""), ", "))
  i_EPISEM <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPISEM), "\\*", ""), ", "))
  i_EpSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EpSO), "\\*", ""), ", "))
  i_ESSO <- unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$ESSO), "\\*", ""), ", "))
  
  
  neurotopk <- c(intersect(neurospace, i_EPILONT_EPISEM_EpSO_ESSO), setdiff(i_EPILONT_EPISEM_EpSO_ESSO, neurospace), # 29
                 intersect(neurospace, i_EPILONT_EPISEM_ESSO), setdiff(i_EPILONT_EPISEM_ESSO, neurospace),
                 intersect(neurospace, i_EPILONT_EpSO_ESSO), setdiff(i_EPILONT_EpSO_ESSO, neurospace),
                 intersect(neurospace, i_EPISEM_EpSO_ESSO), setdiff(i_EPISEM_EpSO_ESSO, neurospace),
                 intersect(neurospace, i_EPILONT_EPISEM), setdiff(i_EPILONT_EPISEM, neurospace),
                 intersect(neurospace, i_EPILONT_EpSO), setdiff(i_EPILONT_EpSO, neurospace),
                 intersect(neurospace, i_EPILONT_ESSO), setdiff(i_EPILONT_ESSO, neurospace),
                 intersect(neurospace, i_EpSO_ESSO), setdiff(i_EpSO_ESSO, neurospace),
                 intersect(neurospace, i_EPILONT), setdiff(i_EPILONT, neurospace),
                 intersect(neurospace, i_EPISEM), setdiff(i_EPISEM, neurospace),
                 intersect(neurospace, i_EpSO), setdiff(i_EpSO, neurospace),
                 intersect(neurospace, i_ESSO), setdiff(i_ESSO, neurospace))
     
  broadspectrum <- c(             
    "Brivaracetam",
    "Clobazam",
    "Felbamate",
    "Lamotrigine",
    "Levetiracetam",
    "Perampanel",
    "Rufinamide",
    "Topiramate",
    "Valproate",
    "Zonisamide"
  )

  focal <- c(
    "Carbamazepine",
    "Cenobamate",
    "Eslicarbazepine",
    "Gabapentin",
    "Lacosamid",
    "Oxcarbazepine",
    "Phenobarbital",
    "Phenytoin",
    "Pregabalin",
    "Primidone",
    "Stiripentol",
    "Tiagabine",
    "Vigabatrin"
  )  
  abscence <- "Ethosuximide"

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
    if (drug %in% i_EPILONT_EPISEM_EpSO_ESSO) {
      rnames <- c(rnames, "$EPILONT_EPISEM_EpSO_ESSO$")
    } else if (drug %in% i_EPILONT_EPISEM_ESSO) {
      rnames <- c(rnames, "EPILONT_EPISEM_ESSO")
    } else if (drug %in% i_EPILONT_EpSO_ESSO) {
      rnames <- c(rnames, "EPILONT_EpSO_ESSO")
    } else if (drug %in% i_EPISEM_EpSO_ESSO) {
      rnames <- c(rnames, "EPISEM_EpSO_ESSO")
    } else if (drug %in% i_EPILONT_EPISEM) {
      rnames <- c(rnames, "EPILONT_EPISEM")
    } else if (drug %in% i_EPILONT_EpSO) {
      rnames <- c(rnames, "EPILONT_EpSO")
    } else if (drug %in% i_EPILONT_ESSO) {
      rnames <- c(rnames, "EPILONT_ESSO")
    } else if (drug %in% i_EpSO_ESSO) {
      rnames <- c(rnames, "EpSO_ESSO")
    } else if (drug %in% i_EPILONT) {
      rnames <- c(rnames, "EPILONT")
    } else if (drug %in% i_EPISEM) {
      rnames <- c(rnames, "EPISEM")
    } else if (drug %in% i_EpSO) {
      rnames <- c(rnames, "EpSO")
    } else if (drug %in% i_ESSO) {
      rnames <- c(rnames, "ESSO")
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
        al <- "X"
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
