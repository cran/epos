#' Create the final resulting data frame
#'
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
#'   data.frame(EpSO = c(neuroepso, rep("", (mx-length(neuroepso)))),
#'              ESSO = c(neuroesso, rep("", (mx-length(neuroesso)))),
#'              EPILONT = c(neuroepi, rep("", (mx-length(neuroepi)))),
#'              EPISEM = c(neuroepisem, rep("", (mx-length(neuroepisem)))),
#'              FENICS = c(neurofenics, rep("", (mx-length(neurofenics)))))
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, L=5, d=5, v=10)
#' neurotable <- createNeuroTable(atchashda, atchashsec, dneuromaxk)
createNeuroTable <- function (atchashda, atchashsec, dneuromaxk) {
  neurospace <- dneuromaxk$topkspace
  neurotopk <- c()
  counter <- 0
  for (o in dneuromaxk$venntable$objects) {
    counter <- counter + 1
      d <- unlist(strsplit(stringr::str_replace_all((o), "\\*", ""), ", "))
      d <- d[nchar(d) > 0]
      i <- intersect(neurospace, d)
      s <-  setdiff(d, neurospace)
      neurotopk <- c(neurotopk, i, s)
  }

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
  
  up2date <- union (abscence, union (broadspectrum, focal))

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
    "Clonazepam",
    "Diazepam",
    "Etomidate",
    "Isoflurane",
    "Levetiracetam",
    "Lorazepam",
    "Midazolam",
    "Pentobarbital",
    "Phenobarbital",
    "Phenytoin",
    "Propofol",
    "Thiopental",
    "Valproic Acid"
  )
  
  seizuremed <- c(
    "Brivaracetam",
    "Cannabidiol",
    "Carbamazepine",
    "Cenobamate",
    "Clobazam",
    "Clonazepam",
    "Diazepam",
    "Eslicarbazepine acetate",
    "Ethosuximide",
    "Ezogabine",
    "Felbamate",
    "Fenfluramine",
    "Gabapentin",
    "Lacosamide",
    "Lamotrigine",
    "Levetiracetam",
    "Lorazepam",
    "Midazolam",
    "Oxcarbazepine",
    "Perampanel",
    "Phenobarbital",
    "Phenytoin",
    "Pregabalin",
    "Primidone",
    "Rufinamide",
    "Stiripentol",
    "Tiagabine",
    "Topiramate",
    "Valproic acid",
    "Vigabatrin",
    "Zonisamide"
  )
  # neurotopk <- neurospace[1:39]
  rnames <- ""
  for (drug in neurotopk) {
    counter <- 0
    for (n in dneuromaxk$venntable$objects) {
      counter <- counter + 1
      curn <- unlist(strsplit(stringr::str_replace_all((n), "\\*", ""), ", "))
      cur <- names(dneuromaxk$venntable$objects)[[counter]]
      #print(c(counter, cur, curn))
      if (drug %in% curn) {
        rnames <- c(rnames, cur)
      }
    }
  }
  rnames <- rnames [2:length(rnames)]
  
  ranking <- rep("", length(neurotopk))
  lanc <- rep ("", length(neurotopk))
  dse <- rep ("", length(neurotopk))
  u2d <- rep ("", length(neurotopk))
  efo <- rep ("", length(neurotopk))
  
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
    if (length(which(up2date == d)) > 0) {
      u2d[counter] <- "X"
    }
    if (length(which(seizuremed == d)) > 0) {
      efo[counter] <- "X"
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
    DSE=dse,
    U2D=u2d,
    EFO=efo
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
      if (atchashda[[n]] != "") {
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
  }
  return (al)
}
