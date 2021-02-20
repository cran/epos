#' Print Top 10 Drugs
#'
#' @param neuroepso Ranked list of drug names co-occurring with EpSO
#' @param neuroesso Ranked list of drug names co-occurring with ESSO
#' @param neuroepi Ranked list of drug names co-occurring with EPILONT
#' @param neuroepisem Ranked list of drug names co-occurring with EPISEM
#' @param neurofenics Ranked list of drug names co-occurring with FENICS
#' 
#' @return data frame with top 10 drugs for each ontology
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
#' epso <- rawDrugNamesCoOcEpSO
#' neuroepso <- filterNeuroDrugs(epso, atchashda)
#' esso <- rawDrugNamesCoOcESSO
#' neuroesso   <- filterNeuroDrugs(esso, atchashda)
#' epi <- rawDrugNamesCoOcEPILONT
#' neuroepi    <- filterNeuroDrugs(epi, atchashda)
#' episem <- rawDrugNamesCoOcEPISEM
#' neuroepisem <- filterNeuroDrugs(episem, atchashda)
#' fenics <- rawDrugNamesCoOcFENICS
#' neurofenics <- filterNeuroDrugs(fenics, atchashda)
#' top10table <- printTop10Drugs(neuroepso, neuroesso, neuroepi, neuroepisem, neurofenics)
#' \dontrun{
#'   print(xtable::xtable(top10table, type = "latex"), 
#'     file = "top10table.tex")
#' }
printTop10Drugs <- function (neuroepso, neuroesso, neuroepi, neuroepisem, neurofenics) {
  dneuro <-
     data.frame(EpSO = neuroepso[1:10],
                ESSO = neuroesso[1:10],
                EPILONT = neuroepi[1:10],
                EPISEM = neuroepisem[1:10],
                FENICS = neurofenics[1:10])
}

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
#' epso <- rawDrugNamesCoOcEpSO
#' neuroepso <- filterNeuroDrugs(epso, atchashda)
#' esso <- rawDrugNamesCoOcESSO
#' neuroesso   <- filterNeuroDrugs(esso, atchashda)
#' epi <- rawDrugNamesCoOcEPILONT
#' neuroepi    <- filterNeuroDrugs(epi, atchashda)
#' episem <- rawDrugNamesCoOcEPISEM
#' neuroepisem <- filterNeuroDrugs(episem, atchashda)
#' fenics <- rawDrugNamesCoOcFENICS
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
 
  #neurotopk <- neurospace
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
  
  aeds <- getAllAEDs(atchashda)
  
  typetreating <- union (up2date, union(lancet, union(drugse, union(seizuremed, aeds))))
  
  typepotential <- c(
    "Ketamine",
    "Tryptophan"
  )
  
  typesomehow <- c(
    "Dextroamphetamine",
    "Levodopa",
    "Naloxone"
  )
  
  typesideeffect <- c(
    "Lidocaine",
    "Haloperidol",
    "Morphine"
  )
  
  rnames <- ""
  for (drug in neurotopk) {
    counter <- 0
    for (n in dneuromaxk$venntable$objects) {
      counter <- counter + 1
      curn <- unlist(strsplit(stringr::str_replace_all((n), "\\*", ""), ", "))
      cur <- names(dneuromaxk$venntable$objects)[[counter]]
      if (drug %in% curn) {
        rnames <- c(rnames, cur)
      }
    }
  }
  rnames <- rnames [2:length(rnames)]
  
  type <- rep("", length(neurotopk))
  ranking <- rep("", length(neurotopk))
  lanc <- rep ("", length(neurotopk))
  dse <- rep ("", length(neurotopk))
  u2d <- rep ("", length(neurotopk))
  efo <- rep ("", length(neurotopk))
  
  counter <- 1
  for (d in neurotopk) {
    # ranking position
    if (length(which(typetreating == d)) > 0) {
      type[counter] <- "T"
    } else if (length(which(typepotential == d)) > 0) {
      type[counter] <- "P"
    } else if (length(which(typesomehow == d)) > 0) {
      type[counter] <- "R"
    } else if (length(which(typesideeffect == d)) > 0) {
      type[counter] <- "S"
    }
    if (length(which(neurospace == d)) > 0) {
      ranking[counter] <- which(neurospace == d)
    }
    if (length(which(lancet == d)) > 0) {
      lanc[counter] <- "x"
    }
    if (length(which(drugse == d)) > 0) {
      dse[counter] <- "x"
    }
    if (length(which(up2date == d)) > 0) {
      u2d[counter] <- "x"
    }
    if (length(which(seizuremed == d)) > 0) {
      efo[counter] <- "x"
    }
    counter <- counter + 1
  }
  
  scoresum <- c()
  for (r in rnames) {
    scoresum <- c(scoresum, length(stringr::str_split(r,"\\_", simplify = TRUE)))
  }
  
  #ranking <- ranking[2:length(ranking)]
  dntk <- data.frame(
    Score=scoresum[ranking != ""],
    Rank=ranking[ranking != ""],
    Intersection=rnames[ranking != ""],
    DrugName=neurotopk[ranking != ""],
    Type=type[ranking != ""],
    Lancet=lanc[ranking != ""],
    DSE=dse[ranking != ""],
    U2D=u2d[ranking != ""],
    EFO=efo[ranking != ""],
    N03=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N03")[ranking != ""],
    N05=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N05")[ranking != ""],
    N06=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N06")[ranking != ""],
    N01=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N01")[ranking != ""],
    N02=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N02")[ranking != ""],
    N04=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N04")[ranking != ""],
    N07=createDashVectorForATC(neurotopk, atchashda, atchashsec, "N07")[ranking != ""]
  )
  return (dntk)
}

#' Sort table by scoring for each row
#'
#' @param dntk the table returned from writeNeuroTable
#'
#' @return the sorted table
#' 
#' @export
#' 
#' @importFrom stats setNames
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
#' epso <- rawDrugNamesCoOcEpSO
#' neuroepso <- filterNeuroDrugs(epso, atchashda)
#' esso <- rawDrugNamesCoOcESSO
#' neuroesso   <- filterNeuroDrugs(esso, atchashda)
#' epi <- rawDrugNamesCoOcEPILONT
#' neuroepi    <- filterNeuroDrugs(epi, atchashda)
#' episem <- rawDrugNamesCoOcEPISEM
#' neuroepisem <- filterNeuroDrugs(episem, atchashda)
#' fenics <- rawDrugNamesCoOcFENICS
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
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, L=5, d=5, v=5)
# neurotable <- createNeuroTable(atchashda, atchashsec, dneuromaxk)
# sortedNeuroTable <- sortTableByRefMatches(neurotable)
# print(xtable::xtable(sortedNeuroTable, type = "latex"),
#   file = "sortedNeuroTable.tex",
#   include.rownames=FALSE)
sortTableByRefMatches <- function (dntk) {
  l <- length(dntk[,1])
  
  c0 <- 0
  c1 <- 0
  c2 <- 0
  c3 <- 0
  c4 <- 0
  c5 <- 0
  
  score0 <- 0
  score1 <- 0
  score2 <- 0
  score3 <- 0
  score4 <- 0
  score5 <- 0
  
  
  for (i in 1:l) {
    curscore <- 0
    if (!is.na(match("Lancet", colnames(dntk))) & !is.na(dntk["Lancet"][i,])) {
      if (dntk["Lancet"][i,] == "x") {
        curscore <- curscore + 1
      }
    }
    if (!is.na(match("DSE", colnames(dntk))) & !is.na(dntk["DSE"][i,])) {
      if (dntk["DSE"][i,] == "x") {
        curscore <- curscore + 1
      }
    }
    if (!is.na(match("U2D", colnames(dntk))) & !is.na(dntk["U2D"][i,])) {
      if (dntk["U2D"][i,] == "x") {
        curscore <- curscore + 1
      }
    }
    if (!is.na(match("EFO", colnames(dntk))) & !is.na(dntk["EFO"][i,])) {
      if (dntk["EFO"][i,] == "x") {
        curscore <- curscore + 1
      }
    }
    if (!is.na(match("N03", colnames(dntk))) & !is.na(dntk["N03"][i,])) {
      if (dntk["N03"][i,] == "x") {
        curscore <- curscore + 1
      }
    }
    
    currow <- dntk[i,]
    currow$Score <- curscore

    if(curscore == 0) {
      c0 <- c0 + 1
      if (c0 == 1) {
        score0 <- currow
      } else {
        score0 <- rbind(score0, currow)
      }
    }
    if(curscore == 1) {
      c1 <- c1 + 1
      if (c1 == 1) {
        score1 <- currow
      } else {
        score1 <- rbind(score1, currow)
      }
    }
    if(curscore == 2) {
      c2 <- c2 + 1
      if (c2 == 1) {
        score2 <- currow
      } else {
        score2 <- rbind(score2, currow)
      }
    }
    if(curscore == 3) {
      c3 <- c3 + 1
      if (c3 == 1) {
        score3 <- currow
      } else {
        score3 <- rbind(score3, currow)
      }
    }
    if(curscore == 4) {
      c4 <- c4 + 1
      if (c4 == 1) {
        score4 <- currow
      } else {
        score4 <- rbind(score4, currow)
      }
    }
    if(curscore == 5) {
      c5 <- c5 + 1
      if (c5 == 1) {
        score5 <- currow
      } else {
        score5 <- rbind(score5, currow)
      }
    }
  }
  if (length(score5)>1) {
    score5 <- score5[order(as.numeric(unlist(score5["Rank"]))),]
  }
  if (length(score4)>1) {
    score4 <- score4[order(as.numeric(unlist(score4["Rank"]))),]
  }
  if (length(score3)>1) {
    score3 <- score3[order(as.numeric(unlist(score3["Rank"]))),]
  }
  if (length(score2)>1) {
    score2 <- score2[order(as.numeric(unlist(score2["Rank"]))),]
  }
  if (length(score1)>1) {
    score1 <- score1[order(as.numeric(unlist(score1["Rank"]))),]
  }
  if (length(score0)>1) {
    score0 <- score0[order(as.numeric(unlist(score0["Rank"]))),]
  }
  finalframe <- stats::setNames(data.frame(matrix(ncol = 15, nrow = 0)),
                         c("Score", "Rank", "Intersection", "DrugName", "Lancet", "DSE",
                           "U2D", "EFO", "N03", "N05", "N06", "N01", "N02", "N04", "N07"))
  finalframe <- rbind(finalframe, score5)
  finalframe <- rbind(finalframe, score4)
  finalframe <- rbind(finalframe, score3)
  finalframe <- rbind(finalframe, score2)
  finalframe <- rbind(finalframe, score1)
  
  score0["Score"] <- 1
  
  finalframe <- rbind(finalframe, score0)
  return (finalframe)
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
            al <- "x"
          } else {
            al <- ""
          }
          counter <- counter + 1
        } else {
          if (atcc == slatc) {
            al <- c(al, "x")
          } else {
            al <- c(al, "")
          }
        }
       
      }
  }
  return (al)
}

getAllAEDs <- function (atchashda) {
  r <- c()
  for (i in ls(atchashda)) {
    if (substr(atchashda[[i]], 1, 3) == "N03") {
      r <- c(r, i)
    }
  }
  return (r)
}
