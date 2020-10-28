#' Plotting functions for enrichment lists
#'
#' @param enepso list with enrichment for EpSO
#' @param enesso list with enrichment for ESSO
#' @param enepi list with enrichment for EPILONT
#' @param enepisem list with enrichment for EPISEM
#' @param enfenics list with enrichment for FENICS
#'
#' @return the plot object
#' @export
#'
#' @examples
#' plotEnrichment (c(0,1,2,2,3),
#'                 c(0,0,1,1,2),
#'                 c(0,1,1,2,2),
#'                 c(0,1,2,3,3),
#'                 c(1,2,3,3,3))
plotEnrichment <- function (enepso, enesso, enepi, enepisem, enfenics) {
  topk <- max(
      c(length(enepso), length(enesso), length(enepi),
        length(enepisem), length(enfenics)))
  
  ymax = max(
    c(max(enepso), max(enesso), max(enepi),
      max(enepisem), max(enfenics)))

  djbase <-
    data.frame(Elements = 1:topk,
               EpSO = c(enepso, rep(sum(enepso), (topk-length(enepso)))),
               ESSO = c(enesso, rep(sum(enesso), (topk-length(enesso)))),
               EPILONT = c(enepi, rep(sum(enepi), (topk-length(enepi)))),
               EPISEM = c(enepisem, rep(sum(enepisem), (topk-length(enepisem)))),
               FENICS = c(enfenics, rep(sum(enfenics), (topk-length(enfenics)))))
  
  cols <-
    c("EpSO" = "#800080",
      "ESSO" = "#FF0000",
      "EPILONT" = "#0000FF",
      "EPISEM" = "#008000",
      "FENICS" = "#FFA500")
  
  
  enrichmentplot <- ggplot2::ggplot(data = djbase,
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
      y = "Reference Drugs",
      x = "Top-K",
      title = "Enrichment Plot of Drug Names Occurring in the Reference Set",
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
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "EPISEM",
        colour = shQuote("EPISEM")
      ),
      size = 1
    ) +
    ggplot2::geom_step(
      data = djbase,
      ggplot2::aes_string(
        x = "Elements",
        y = "FENICS",
        colour = shQuote("FENICS")
      ),
      size = 1
    ) +
    ggplot2::coord_trans(xlim = c(0, topk), ylim = c(0, ymax)) +
    ggplot2::scale_x_continuous(breaks = c(0, (topk/5*2), (topk/5*3), (topk/5*4), topk)) +
    ggplot2::scale_y_continuous(breaks = c((ymax/5*1), (ymax/5*2), (ymax/5*3), (ymax/5*4), ymax)) +
    ggplot2::scale_colour_manual(name = "Dictionary", values = cols) +
    ggplot2::scale_size_manual()
  return (enrichmentplot)
}





#' Calculate enrichment of one list in comparison to reference list
#'
#' @param alist the list to compare
#' @param reflist the reference list
#'
#' @return list with calculated enrichment used for plotting
#' @export
#'
#' @examples
#' a <- calcEnrichment(c("a","b","c", "d", "e"), c("b", "d"))
calcEnrichment <- function (alist, reflist) {
  enrichment <- c()
  curscore <- 0
  for (i in 1:length(alist)) {
    found <- FALSE
    for (j in 1:length(reflist)) {
      if (alist[i] == reflist[j]) {
        found <- TRUE
      }
    }
    if (found) {
      curscore <- curscore +1
    }
    enrichment <- c(enrichment, curscore)
  }
  return (enrichment)
}