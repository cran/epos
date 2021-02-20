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
#' neurospace <- as.character(dneuromaxk$topkspace)
#' dsepso <- calcDSEA(neuroepso, mx)
#' dsesso <- calcDSEA(neuroesso, mx)
#' dsepi <- calcDSEA(neuroepi, mx)
#' dsepisem <- calcDSEA(neuroepisem, mx)
#' dsfenics <- calcDSEA(neurofenics, mx)
#' dsspace  <-  calcDSEA (neurospace, mx)
#' p <- plotDSEA(dsepso, dsesso, dsepi, dsepisem, dsfenics, dsspace, dneuromaxk$maxK)
#' \dontrun{
#' ggplot2::ggsave("dsea.png", 
#'    p <- plotDSEA(dsepso, dsesso, dsepi, dsepisem, dsfenics, dsspace, 
#'    dneuromaxk$maxK), width=480, height=320, units = "mm", dpi = 300)
#' }
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
    c("EpSO" = "#7800FE",#red
      "ESSO" = "#00FFD6",#cyan
      "EPILONT" = "#FFA501",#gold
      "EPISEM" = "#E80908",#blue
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
      legend.text = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "italic"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.654,0.85),
      legend.justification = c(0, 0),
      legend.key.size = ggplot2::unit(12, "mm"),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "plain"),
      axis.title.x = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "plain"),
      axis.title.y = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "plain"),
      axis.text.x = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "plain"),
      axis.text.y = ggplot2::element_text(family="Arial Nova Light", size = 24, face = "plain")
    ) +
    ggplot2::labs (
      y = "Drug Set Enrichment Score",
      x = "k",
      #title = "Enrichment of Top-k Drug Names Occurring in the Reference Set",
      subtitle = ""
    ) +
    ggplot2::geom_step(
      size = 2.7,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "ESSO",
        colour = shQuote("ESSO")
      ),
      size = 2.5,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPILONT",
        colour = shQuote("EPILONT")
      ),
      size = 2.5,
      linetype = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPISEM",
        colour = shQuote("EPISEM")
      ),
      size = 2,
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
    ggplot2::coord_trans(xlim = c(1, 225), ylim = c(-5, 10)) +
    ggplot2::scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
    ggplot2::scale_y_continuous(breaks = c(-5, 0, 5, 10)) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    ggplot2::scale_size_manual() +
    ggplot2::annotate("segment", x = 35, 
                      xend = which.max(dsepso), 
                      y = 5.5, 
                      yend = max(dsepso)+0.2, 
                      colour = "#7800FE", 
                      size=2, alpha=1, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=(data.frame(x=35,y=6,label=
      paste("max=(", 
            which.max(dsepso), 
            ",", 
            round(max(dsepso),2),")",sep=""))), 
      family="Arial Nova Light",
      ggplot2::aes_string( x="x", y="y", label="label"), 
      color="#000000", size=7 , angle=13, fontface="bold" ) +
    ggplot2::annotate("segment", x = 160, 
                      xend = (which.max(dsesso)+1), 
                      y = 6, 
                      yend = (max(dsesso)+0.1), 
                      colour = "#00FFD6", 
                      size=2, alpha=0.90, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=
                         (data.frame(
                           x=160,
                           y=6.5,label=paste("max=(", 
                                                which.max(dsesso), 
                                                ",", 
                                                round(max(dsesso),2),")",sep=""))), 
                       family="Arial Nova Light",
                       ggplot2::aes_string( x="x", y="y", label="label"), 
                       color="#000000", size=7 , angle=-10, fontface="bold" ) +
    ggplot2::annotate("segment", 
                      x = 125, 
                      xend = which.max(dsepi)+1, 
                      y = 8.5, 
                      yend = max(dsepi)+0.1, 
                      colour = "#FFA501", 
                      size=2, alpha=0.75, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=
                         (data.frame(
                           x=125,
                           y=9,label=
                                          paste("max=(", 
                                                which.max(dsepi), 
                                                ",", 
                                                round(max(dsepi),2),")",sep=""))), 
                       family="Arial Nova Light",
                       ggplot2::aes_string( x="x", y="y", label="label"), 
                       color="#000000", size=7 , angle=-5, fontface="bold" ) +
    ggplot2::annotate("segment", x = 15, 
                      xend = (which.max(dsfenics)-2), 
                      y = 8, 
                      yend = (max(dsfenics)+0.2), 
                      colour = "#008000", 
                      size=2, alpha=0.75, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=(data.frame(x=15,y=8.5,label=
                                          paste("max=(", 
                                                which.max(dsfenics), 
                                                ",", 
                                                round(max(dsfenics),2),")",sep=""))),
                       family="Arial Nova Light",
                       ggplot2::aes_string( x="x", y="y", label="label"), 
                       color="#000000", size=7 , angle=0, fontface="bold" )  +
    ggplot2::annotate("segment", x = 90, 
                      xend = (which.max(dsspace)), 
                      y = 9, 
                      yend = (max(dsspace)+0.1), 
                      colour = "#FF00FF", 
                      size=2, alpha=0.75, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=(data.frame(x=90,
                                        y=9.5,
                                        label=
                                          paste("max=(", 
                                                which.max(dsspace), 
                                                ",", 
                                                round(max(dsspace),2),")",sep=""))),
                       family="Arial Nova Light",
                       ggplot2::aes_string( x="x", y="y", label="label"), 
                       color="#000000", size=7 , angle=-3, fontface="bold" )  +
    ggplot2::annotate("segment", x = 60, 
                      xend = (which.max(dsepisem)+1), 
                      y = 8, 
                      yend = (max(dsepisem)+0.1), 
                      colour = "#E80908", 
                      size=2, alpha=0.75, arrow=ggplot2::arrow()) + 
    ggplot2::geom_text(data=(data.frame(x=60,y=8.5,label=
                                          paste("max=(", 
                                                which.max(dsepisem), 
                                                ",", 
                                                round(max(dsepisem),2),")",sep=""))), 
                       family="Arial Nova Light",
                       ggplot2::aes_string( x="x", y="y", label="label"), 
                       color="#000000", size=7 , angle=6, fontface="bold" )  
  
  
  
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
#'     readAtcMapIntoHashMapDrugNamesAtcCodes(
#'         system.file("extdata", "db-atc.map", package = "epos"), "\t")
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
    c("EpSO" = "#7800FE",#red
      "ESSO" = "#00FFD6",#cyan
      "EPILONT" = "#FFA501",#yellowgreen
      "EPISEM" = "#E80908",#blue
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
      legend.text = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "italic"),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.position = c(0.06,0.76),
      legend.justification = c(0, 0),
      legend.key.size = ggplot2::unit(12, "mm"),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "plain"),
      axis.title.x = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "plain"),
      axis.title.y = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "plain"),
      axis.text.x = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "plain"),
      axis.text.y = ggplot2::element_text(family="Arial Nova Light", size = 48, face = "plain")
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
  n03 <- c(
    "Barbexaclone",
    "Beclamide",
    "Brivaracetam",
    "Cannabidiol",
    "Carbamazepine",
    "Carisbamate",
    "Clonazepam",
    "Eslicarbazepine",
    "Ethadione",
    "Ethosuximide",
    "Ethotoin",
    "Ezogabine",
    "Felbamate",
    "Fosphenytoin",
    "Gabapentin",
    "Lacosamide",
    "Lamotrigine",
    "Levetiracetam",
    "Mephenytoin",
    "Metharbital",
    "Methsuximide",
    "Neocitrullamon",
    "Oxcarbazepine",
    "Paramethadione",
    "Perampanel",
    "Phenacemide",
    "Pheneturide",
    "Phenobarbital",
    "Phensuximide",
    "Phenytoin",
    "Pregabalin",
    "Primidone",
    "Progabide",
    "Rufinamide",
    "Stiripentol",
    "Sulthiame",
    "Tiagabine",
    "Topiramate",
    "Trimethadione",
    "Valproic acid",
    "Valpromide",
    "Vigabatrin"
  )
  
  
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
  
  refall <- union (
    seizuremed, 
    union (drugse, 
           union (
             seizuremed, 
             union (up2date, 
                    n03
        )
      )
    )
  )
  
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
    counter <- (counter + 1)
    divisor_hit  <- (S - curscore)
    divisor_miss <- (N - curscore)
    p_hit <-  log((curscore+S)/divisor_hit)
    p_miss <- -log((curscore+N)/divisor_miss)
    if (drug %in% refall) {
      curscore <- curscore +1
      ea <- c(ea, (utils::tail(ea, n=1) + p_hit))
    } else {
      ea <- c(ea, (utils::tail(ea,n=1) + p_miss))
    }
  }
  return (ea)
}
