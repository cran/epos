#' Plotting functions for DSEA lists
#'
#' @param dsepso list with enrichment for EpSO
#' @param dsesso list with enrichment for ESSO
#' @param dsepi list with enrichment for EPILONT
#' @param dsepisem list with enrichment for EPISEM
#' @param dsfenics list with enrichment for FENICS
#' @param dsspace list with enrichment for the combined ranked list
#' @param k numeric value for the length to be plotted
#' @return the plot object
#' @export
#'
#' @examples
#' utils::data(rawDrugNamesCoOcEpSO, package="epos")
#' utils::data(rawDrugNamesCoOcESSO, package="epos")
#' utils::data(rawDrugNamesCoOcEPILONT, package="epos")
#' utils::data(rawDrugNamesCoOcEPISEM, package="epos")
#' utils::data(rawDrugNamesCoOcFENICS, package="epos")
#' atchashda <-
#'     readAtcMapIntoHashMapDrugNamesAtcCodes(
#'         system.file("extdata", "db-atc.map", package = "epos"), "\t")
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
#' neurospace <- as.character(dneuromaxk$topkspace)
#' dsepso <- calcDSEA(neuroepso, mx)
#' dsesso <- calcDSEA(neuroesso, mx)
#' dsepi <- calcDSEA(neuroepi, mx)
#' dsepisem <- calcDSEA(neuroepisem, mx)
#' dsfenics <- calcDSEA(neurofenics, mx)
#' dsspace  <-  calcDSEA (neurospace, mx)
#' p <- plotDSEA(dsepso, dsesso, dsepi, dsepisem, dsfenics, dsspace, dneuromaxk$maxK)
plotDSEA <- function (dsepso, dsesso, dsepi, dsepisem, dsfenics, dsspace, k) {
  topk <- max(
    c(length(dsepso), length(dsesso), length(dsepi),
      length(dsepisem), length(dsfenics), length(dsspace)))
  
  ymax = max(
    c(max(dsepso), max(dsesso), max(dsepi),
      max(dsepisem), max(dsfenics), max(dsspace)))
  
  djbase <-
    data.frame(Elements = 1:topk,
               EpSO = c(dsepso, rep(max(dsepso), (topk-length(dsepso)))),
               ESSO = c(dsesso, rep(max(dsesso), (topk-length(dsesso)))),
               EPILONT = c(dsepi, rep(max(dsepi), (topk-length(dsepi)))),
               EPISEM = c(dsepisem, rep(max(dsepisem), (topk-length(dsepisem)))),
               FENICS = c(dsfenics, rep(max(dsfenics), (topk-length(dsfenics)))),
               Final = c(dsspace, rep(max(dsspace), (topk-length(dsspace))))
    )
  
  cols <-
    c("EpSO" = "#FF0000",#red
      "ESSO" = "#00FFFF",#cyan
      "EPILONT" = "#9ACD32",#yellowgreen
      "EPISEM" = "#0000FF",#blue
      "FENICS" = "#008000",#green
      "Final" = "#FF00FF")#magenta
  
  
  enrichmentplot <- ggplot2::ggplot(data = djbase,
                                    ggplot2::aes_string(
                                      x = "Elements",
                                      y = "EpSO",
                                      colour = shQuote("EpSO")
                                    )) +
    ggplot2::theme_minimal () +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(colour = "grey",linetype = "F1"),
      panel.grid.major = ggplot2::element_line(colour = "grey",linetype = "F1"),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey",linetype = "F1"),
      legend.text = ggplot2::element_text(family="Times New Roman", size = 48, face = "italic"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.45,0.82),
      legend.justification = c(0, 0),
      legend.key.size = ggplot2::unit(12, "mm"),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.title.x = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.title.y = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.text.x = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.text.y = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain")
    ) +
    ggplot2::labs (
      y = "Drug Set Enrichment Score",
      x = "k",
      #title = "Enrichment of Top-k Drug Names Occurring in the Reference Set",
      subtitle = ""
    ) +
    ggplot2::geom_step(
      size = 4,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "ESSO",
        colour = shQuote("ESSO")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPILONT",
        colour = shQuote("EPILONT")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPISEM",
        colour = shQuote("EPISEM")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "FENICS",
        colour = shQuote("FENICS")
      ),
      size = 2,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "Final",
        colour = shQuote("Final")
      ),
      size = 2,
      linetype = 1
    ) +
    ggplot2::coord_trans(xlim = c(1, 300), ylim = c(-5, 18)) +
    ggplot2::scale_x_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300)) +
    ggplot2::scale_y_continuous(breaks = c(-5, 0, 5, 10, 15)) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    ggplot2::scale_size_manual()
  return (enrichmentplot)
}

#' Plotting functions for enrichment lists
#'
#' @param enepso list with enrichment for EpSO
#' @param enesso list with enrichment for ESSO
#' @param enepi list with enrichment for EPILONT
#' @param enepisem list with enrichment for EPISEM
#' @param enfenics list with enrichment for FENICS
#' @param enspace list with enrichment for the combined ranked list
#' @param k numeric value for the length to be plotted
#' @return the plot object
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
#' neurospace <- as.character(dneuromaxk$topkspace)
#' enepso <- calcEnrichment(neuroepso)
#' enesso <- calcEnrichment(neuroesso)
#' enepi <- calcEnrichment(neuroepi)
#' enepisem <- calcEnrichment(neuroepisem)
#' enfenics <- calcEnrichment(neurofenics)
#' enspace <- calcEnrichment (neurospace)
#' p <- plotEnrichment(enepso, enesso, enepi, enepisem, enfenics, enspace, dneuromaxk$maxK)
plotEnrichment <- function (enepso, enesso, enepi, enepisem, enfenics, enspace, k) {
  topk <- max(
      c(length(enepso), length(enesso), length(enepi),
        length(enepisem), length(enfenics), length(enspace)))

  ymax = max(
    c(max(enepso), max(enesso), max(enepi),
      max(enepisem), max(enfenics), max(enspace)))

  djbase <-
    data.frame(Elements = 1:topk,
               EpSO = c(enepso, rep(max(enepso), (topk-length(enepso)))),
               ESSO = c(enesso, rep(max(enesso), (topk-length(enesso)))),
               EPILONT = c(enepi, rep(max(enepi), (topk-length(enepi)))),
               EPISEM = c(enepisem, rep(max(enepisem), (topk-length(enepisem)))),
               FENICS = c(enfenics, rep(max(enfenics), (topk-length(enfenics)))),
               Final = c(enspace, rep(max(enspace), (topk-length(enspace))))
               )
  
  cols <-
    c("EpSO" = "#FF0000",#red
      "ESSO" = "#00FFFF",#cyan
      "EPILONT" = "#9ACD32",#yellowgreen
      "EPISEM" = "#0000FF",#blue
      "FENICS" = "#008000",#green
      "Final" = "#FF00FF")#magenta
  
  
  enrichmentplot <- ggplot2::ggplot(data = djbase,
                                  ggplot2::aes_string(
                                    x = "Elements",
                                    y = "EpSO",
                                    colour = shQuote("EpSO")
                                  )) +
    ggplot2::theme_minimal () +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(colour = "grey",linetype = "F1"),
      panel.grid.major = ggplot2::element_line(colour = "grey",linetype = "F1"),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey",linetype = "F1"),
      legend.text = ggplot2::element_text(family="Times New Roman", size = 48, face = "italic"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.06,0.76),
      legend.justification = c(0, 0),
      legend.key.size = ggplot2::unit(12, "mm"),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.title.x = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.title.y = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.text.x = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain"),
      axis.text.y = ggplot2::element_text(family="Times New Roman", size = 48, face = "plain")
    ) +
    ggplot2::labs (
      y = "# of Reference Drugs",
      x = "k",
      #title = "Enrichment of Top-k Drug Names Occurring in the Reference Set",
      subtitle = ""
    ) +
    ggplot2::geom_step(
      size = 4,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "ESSO",
        colour = shQuote("ESSO")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPILONT",
        colour = shQuote("EPILONT")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPISEM",
        colour = shQuote("EPISEM")
      ),
      size = 3,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "FENICS",
        colour = shQuote("FENICS")
      ),
      size = 2,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "Final",
        colour = shQuote("Final")
      ),
      size = 2,
      linetype = 1
    ) +
    ggplot2::coord_trans(xlim = c(1, 250), ylim = c(1, ymax)) +
    ggplot2::scale_x_continuous(trans = 'log10', breaks = c(1, 10, 100, 250)) +
    ggplot2::scale_y_continuous(breaks = c(0, 10, 20, 30)) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    ggplot2::scale_size_manual()
  return (enrichmentplot)
}

#' Retrieve the list of drugs from the union of all reference lists
#'
#' @return list of drugs from all reference lists
#' @export
#'
#' @examples
#' d <- getRefAll()
getRefAll <- function () {
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
  
  refall <- union (seizuremed, union (drugse, union (seizuremed, up2date)))
  
  return (refall)
}



#' Calculate enrichment of one list in comparison to reference list
#'
#' @param alist the list to compare
#'
#' @return list with calculated enrichment used for plotting
#' @export
#'
#' @examples
#' a <- calcEnrichment(c("Clobazam","Oxcarbazepine"))
calcEnrichment <- function (alist) {
 
  enrichment <- c()
  curscore <- 0
  refall <- getRefAll()
  
  for (drug in alist) {
    if (drug %in% refall) {
      curscore <- curscore +1
    }
    enrichment <- c(enrichment, curscore)
  }
  
  return (enrichment)
}

#' Calculate dsea scores of one list in comparison to reference list
#'
#' @param alist list of drug names to be used for calculating dsea
#' @param N numeric value with maximum length of lists for dsea calculation
#'
#' @return list with dsea scores
#' @export
#'
#' @examples
#' calcDSEA(c("Valproic acid", "Lamotrigine", "Ketamin"), 3)
calcDSEA <- function (alist, N) {
  enrichment <- c()
  curscore <- 0
  ea <- 0
  refall <- getRefAll()
  #N <- length(alist)
  S <- length(refall)
  counter <- 0
  for (drug in alist) {
    enrichment <- c(enrichment, curscore)
    # print(cat("enrichment: ", enrichment))
    counter <- (counter + 1)
    # print(cat("counter: ", counter))
    divisor_hit  <- (S - (max (enrichment)-1))
    # print(cat("divisor_hit: ", divisor_hit))
    divisor_miss <- (N - (max (enrichment)-1))
    # print(cat("divisor_miss: ", divisor_miss))
    p_hit <-  log((curscore+S)/divisor_hit)
    # print(cat("p_hit: ", p_hit))
    p_miss <- -log((curscore+N)/divisor_miss)
    # print(cat("p_miss: ", p_miss))
    if (drug %in% refall) {
      curscore <- curscore +1
      ea <- c(ea, (utils::tail(ea, n=1) + p_hit))
    } else {
      ea <- c(ea, (utils::tail(ea,n=1) + p_miss))
    }
  }
  return (ea)
}