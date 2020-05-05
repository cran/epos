#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param neuroepso list of neuro drug names co-occurring with epso
#' @param neuroesso list of neuro drug names co-occurring with esso
#' @param neuroepi list of neuro drug names co-occurring with epi
#' @param dneuromaxk object returned from TopKLists::calculate.maxkS
#'
#' @return jaccardepilepsyplot the ggplot object
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @export
#' @examples
#' utils::data(rawDrugBankCoOcEpSO, package="epos")
#' atchashda <-
#'   readAtcMapIntoHashMapDrugNamesAtcCodes(
#'     system.file("extdata", "db-atc.map", package = "epos"), "\t")
#' tepso <- genDictListFromRawFreq(rawDrugBankCoOcEpSO[1:150])
#' neuroepso <- filterNeuroDrugs(tepso, atchashda)
#' utils::data(rawDrugBankCoOcESSO, package="epos")
#' tesso <- genDictListFromRawFreq(rawDrugBankCoOcESSO[1:150])
#' neuroesso <- filterNeuroDrugs(tesso, atchashda)
#' utils::data(rawDrugBankCoOcEPILONT, package="epos")
#' tepi <- genDictListFromRawFreq(rawDrugBankCoOcEPILONT[1:150])
#' neuroepi <- filterNeuroDrugs(tepi, atchashda)
#' dneuro <-
#'   data.frame(EpSO = neuroepso[1:15],
#'              ESSO = neuroesso[1:15],
#'              EPILONT = neuroepi[1:15])
#' dneuromaxk <- TopKLists::calculate.maxK(dneuro, 3, 5, 10)
#' tanimotobaseline <- createTanimotoBaseline(neuroepso, neuroesso, neuroepi, dneuromaxk)
createTanimotoBaseline <-
  function (neuroepso, neuroesso, neuroepi, dneuromaxk) {
    
    neurobase <-  unlist(strsplit(stringr::str_replace_all((dneuromaxk$venntable$objects$EPILONT_EpSO_ESSO), "\\*", ""), ", "))
    
    topk <- length(neurobase)
    
    jbasenepso <- calcJaccard(neurobase, neuroepso[1:topk])
    jbasenesso <- calcJaccard(neurobase, neuroesso[1:topk])
    jbasenepi <- calcJaccard(neurobase, neuroepi[1:topk])
    
    
    djbase <- data.frame (
      Elements = 1:topk,
      EpSO = jbasenepso[1:topk],
      ESSO = jbasenesso[1:topk],
      EPILONT = jbasenepi[1:topk]
    )
    cols <-
      c("EpSO" = "#8A2BE2",
        "ESSO" = "#7CFC00",
        "EPILONT" = "#C71585")
    
    tanimotobase <- ggplot2::ggplot(data = djbase,
                                     ggplot2::aes_string(
                                       x = "Elements",
                                       y = "EpSO",
                                       colour = shQuote("EpSO")
                                     )) +
      ggplot2::theme_minimal () +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray"),
        panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
        legend.text = ggplot2::element_text(size =
                                              11),
        legend.position = c(0, 1),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      ) +
      ggplot2::labs (
        y = "Tanimoto",
        x = "K",
        title = "Tanimoto Similarity between DrugBank vectors of Epilepsy ontologies vs. Aggregated Baseline",
        subtitle = ""
      ) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_step(
        data = djbase,
        ggplot2::aes_string(
          x = "Elements",
          y = "ESSO",
          colour = shQuote("ESSO")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djbase,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPILONT",
          colour = shQuote("EPILONT")
        ),
        size = 1
      ) +
      ggplot2::coord_trans(limx = c(0, topk), limy = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, topk)) +
      ggplot2::scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
      ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
      ggplot2::scale_size_manual()
    return (tanimotobase)
  }

#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param jmeshepso list containing jaccard coefficients between mesh and epso for increasing k
#' @param jmeshesso list containing jaccard coefficients between mesh and esso for increasing k
#' @param jmeshepi list containing jaccard coefficients between mesh and epi for increasing k
#'
#' @return jaccardepilepsyplot the ggplot object
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' jaccardepilepsyplot <- createJaccardPlotAll(jaccardepso, jaccardesso)
#' }
createJaccardPlotDBMeSH <-
  function (jmeshepso, jmeshesso, jmeshepi) {
    djmesh <- data.frame (
      Elements = 1:250,
      EpSO = jmeshepso[1:250],
      ESSO = jmeshesso[1:250],
      EPILONT = jmeshepi[1:250]
    )
    cols <-
      c("EpSO" = "#8A2BE2",
        "ESSO" = "#7CFC00",
        "EPILONT" = "#C71585")
    
    jaccarddbmesh <- ggplot2::ggplot(data = djmesh,
                                     ggplot2::aes_string(
                                       x = "Elements",
                                       y = "EpSO",
                                       colour = shQuote("EpSO")
                                     )) +
      ggplot2::theme_minimal () +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray"),
        panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
        legend.text = ggplot2::element_text(size =
                                              11),
        legend.position = c(0, 1),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      ) +
      ggplot2::labs (
        y = "Jaccard",
        x = "Length",
        title = "Jaccard Similarity between DrugBank vectors of Epilepsy ontologies versus MeSH derived DrugBank vector",
        subtitle = ""
      ) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "ESSO",
          colour = shQuote("ESSO")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPILONT",
          colour = shQuote("EPILONT")
        ),
        size = 1
      ) +
      ggplot2::coord_trans(limx = c(0, 250), limy = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250)) +
      ggplot2::scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
      ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
      ggplot2::scale_size_manual()
    return (jaccarddbmesh)
  }

#' Creates the plot for all jaccard coefficients amongst the three epilepsy ontologies
#'
#' @param jmeshepso list of jaccard coefficients between mesh and epso for increasing k
#' @param jmeshesso list of jaccard coefficients between mesh and esso for increasing k
#' @param jmeshepi list of jaccard coefficients between mesh and epi for increasing k
#' @param jmeshepilepsyand list of jaccard coefficients between mesh and the intersection of epso, esso, and epi for increasing k
#' @param jmeshepilepsyor list of jaccard coefficients between mesh and the union of epso, esso, and epi for increasing k
#'
#' @return jaccardepilepsyplot the ggplot object
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' jaccardepilepsyplot <- createJaccardPlotAll(jaccardepso, jaccardesso)
#' }
createJaccardPlotMeSHFive <-
  function (jmeshepso, jmeshesso, jmeshepi, jmeshepilepsyand, jmeshepilepsyor) {
    djmesh <- data.frame (
      Elements = 1:962,
      EpSO = jmeshepso[1:962],
      ESSO = jmeshesso[1:962],
      EPILONT = jmeshepi[1:962],
      EPAND = jmeshepilepsyand[1:962],
      EPOR = jmeshepilepsyor[1:962]
    )
    cols <-
      c("EpSO" = "#800080", #purple
        "ESSO" = "#FFFF00", #yellow
        "EPILONT" = "#0000FF", #blue
        "EPAND" = "#008000", #green
        "EPOR" = "#FF0000" #red
        )
    
    jaccarddbmesh <- ggplot2::ggplot(data = djmesh,
                                     ggplot2::aes_string(
                                       x = "Elements",
                                       y = "EpSO",
                                       colour = shQuote("EpSO")
                                     )) +
      ggplot2::theme_minimal () +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray"),
        panel.grid.minor.y = ggplot2::element_line(colour = "gray"),
        legend.text = ggplot2::element_text(size =
                                              11),
        legend.position = c(0, 1),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      ) +
      ggplot2::labs (
        y = "Jaccard",
        x = "Length",
        title = "Jaccard Similarity between DrugBank vectors of Epilepsy ontologies versus MeSH derived DrugBank vector",
        subtitle = ""
      ) +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "ESSO",
          colour = shQuote("ESSO")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPILONT",
          colour = shQuote("EPILONT")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPAND",
          colour = shQuote("EPAND")
        ),
        size = 1
      ) +
      ggplot2::geom_step(
        data = djmesh,
        ggplot2::aes_string(
          x = "Elements",
          y = "EPOR",
          colour = shQuote("EPOR")
        ),
        size = 1
      ) +
      ggplot2::coord_trans(limx = c(0, 100), limy = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
      ggplot2::scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1)) +
      ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
      ggplot2::scale_size_manual()
    return (jaccarddbmesh)
  }

tanimotoPlot <- function(neurospace, neuromesh, k) {
  neurospace <- as.character(neurospace)
  jneuromeshneurospace <- calcJaccard(neurospace, neuromesh[1:k])
  
  dj <- data.frame(Elements = c(1:k), TopKSpace=1-jneuromeshneurospace)
  
  cols <-
    c("TopKSpace" = "#000000" #black
    )
  
  tanimoto <- ggplot2::ggplot(data = dj,
                                   ggplot2::aes_string(
                                     x = "Elements",
                                     y = "TopKSpace",
                                     colour = shQuote("TopKSpace")
                                   )) +
    ggplot2::theme_minimal () +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(colour = "black"),
      panel.grid.major.x = ggplot2::element_line(colour = "grey"),
      panel.grid.minor.x = ggplot2::element_line(colour = "black"),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey"),
      legend.text = ggplot2::element_text(size = 11),
      legend.position = c(0, 1),
      legend.justification = c(0, 0),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 11, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 11)
    ) +
    ggplot2::labs (
      y = "Tanimoto",
      x = "K",
      title = "Tanimoto Similarity between the TopKSpace of Epilepsy Ontologies and the MeSH-derived Vector",
      subtitle = ""
    ) +
    ggplot2::geom_step(size = 2) +
    ggplot2::coord_trans(limx = c(1, k), limy = c(0, 0.5)) +
    ggplot2::scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, k)) +
    ggplot2::scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    ggplot2::scale_size_manual()
}