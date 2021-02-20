#' Create quintuple Venn Diagramm for shared synonyms between EpSO, ESSO, 
#' EPILONT, EPISEM and FENICS
#' 
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn5syn.png", plot = drawVenn5Syn(), width=240, 
#'   height=160, units = "mm", dpi = 300)
#' }
drawVenn5Syn <- function () {
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908",
      "FENICS" = "#008000")
  
  dv <- VennDiagram::draw.quintuple.venn(
    area1 = 3059,
    area2 = 7284,
    area3 = 530,
    area4 = 4847,
    area5 = 708,
    n12 = 232,
    n13 = 38,
    n14 = 62,
    n15 = 0,
    n23 = 74,
    n24 = 852,
    n25 = 0,
    n34 = 32,
    n35 = 0,
    n45 = 0,
    n123 = 18,
    n124 = 39,
    n125 = 0,
    n134 = 5,
    n135 = 0,
    n145 = 0,
    n234 = 29,
    n235 = 0,
    n245 = 0,
    n345 = 0,
    n1234 = 5,
    n1235 = 0,
    n1245 = 0,
    n1345 = 0,
    n2345 = 0,
    n12345 = 0,
    lwd = c(1,1,1,1,1),
    lty = c(2,2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM", "FENICS"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    rotation.degree = 37,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    cat.pos = c (-110,-180,110,160,70),
    margin = 0.05
  )
  return (dv)
}


#' Create quad Venn Diagramm for shared synonyms between EpSO, ESSO, EPILONT
#' and EPISEM 
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn4syn.png", plot = drawVenn4Syn(), width=240, 
#'   height=160, units = "mm", dpi = 300)
#' }
drawVenn4Syn <- function () {
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908")
  
  dv <- VennDiagram::draw.quad.venn(
    area1 = 3059,
    area2 = 7284,
    area3 = 530,
    area4 = 4847,
    #area5 = 708,
    n12 = 232,
    n13 = 38,
    n14 = 62,
    #n15 = 0,
    n23 = 74,
    n24 = 852,
    #n25 = 0,
    n34 = 32,
    #n35 = 0,
    #n45 = 0,
    n123 = 18,
    n124 = 39,
    #n125 = 0,
    n134 = 5,
    #n135 = 0,
    #n145 = 0,
    n234 = 29,
    #n235 = 0,
    #n245 = 0,
    #n345 = 0,
    n1234 = 5,
    #n1235 = 18,
    #n1245 = 39,
    #n1345 = 5,
    #n2345 = 29,
    #n12345 = 5,
    
    lwd = c(1,1,1,1),
    lty = c(2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    rotation.degree = 0,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    margin = 0.05
  )
  return (dv)
}

#' Create quintuple Venn Diagramm for shared documents with co-occurrences of
#' drug names between EpSO, ESSO, EPILONT, EPISEM and FENICS
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quintuple.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn5drugdoc.png", plot = drawVenn5DrugDoc(), width=240, 
#'   height=160, units = "mm", dpi = 300)
#' }
drawVenn5DrugDoc <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908",
      "FENICS" = "#008000")
  
  dv <- VennDiagram::draw.quintuple.venn(
    area1 = 4484726,
    area2 = 7586298,
    area3 = 819922,
    area4 = 2578483,
    area5 = 43,
    n12 = 4331595,
    n13 = 616790,
    n14 = 1974940,
    n15 = 30,
    n23 = 799696,
    n24 = 2549142,
    n25 = 40,
    n34 = 418084,
    n35 = 26,
    n45 = 8,
    n123 = 605767,
    n124 = 1964321,
    n125 = 29,
    n134 = 364724,
    n135 = 22,
    n145 = 8,
    n234 = 416047,
    n235 = 25,
    n245 = 8,
    n345 = 6,
    n1234 = 363602,
    n1235 = 21,
    n1245 = 8,
    n1345 = 6,
    n2345 = 6,
    n12345 = 6,
    lwd = c(1,1,1,1,1),
    lty = c(2,2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM", "FENICS"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    rotation.degree = 37,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    cat.pos = c (-110,-180,110,160,70),
    margin = 0.05
    
  )
  return (dv)
}

#' Create quad Venn Diagramm for shared documents with co-occurrences of
#' drug names between EpSO, ESSO, EPILONT and EPISEM
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn4drugdoc.png", plot = drawVenn4DrugDoc(), width=240, 
#'   height=160, units = "mm", dpi = 300)
#' }
drawVenn4DrugDoc <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908")
  
  dv <- VennDiagram::draw.quad.venn(
    area1 = 4484726,
    area2 = 7586298,
    area3 = 819922,
    area4 = 2578483,
    n12 = 4331595,
    n13 = 616790,
    n14 = 1974940,
    n23 = 799696,
    n24 = 2549142,
    n34 = 418084,
    n123 = 605767,
    n124 = 1964321,
    n134 = 364724,
    n234 = 416047,
    n1234 = 363602,
    lwd = c(1,1,1,1),
    lty = c(2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    margin = 0.05
  )
  return (dv)
}

#' Create quintuple Venn Diagramm for shared documents between EpSO, ESSO, 
#' EPILONT, EPISEM and FENICS
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quintuple.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn5doc.png", plot = drawVenn5Doc(), width=240, height=160, 
#'   units = "mm", dpi = 300)
#' }
drawVenn5Doc <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908",
      "FENICS" = "#008000")
  
  dv <- VennDiagram::draw.quintuple.venn(
    area1 = 9202628,
    area2 = 14329391,
    area3 = 1842409,
    area4 = 5293385,
    area5 = 62,
    n12 = 8829177,
    n13 = 1415908,
    n14 = 4220852,
    n15 = 39,
    n23 = 1798705,
    n24 = 5227880,
    n25 = 58,
    n34 = 962481,
    n35 = 35,
    n45 = 15,
    n123 = 1392279,
    n124 = 4193740,
    n125 = 37,
    n134 = 846145,
    n135 = 28,
    n145 = 11,
    n234 = 957044,
    n235 = 33,
    n245 = 15,
    n345 = 9,
    n1234 = 843026,
    n1235 = 26,
    n1245 = 11,
    n1345 = 9,
    n2345 = 9,
    n12345 = 9,
    lwd = c(1,1,1,1,1),
    lty = c(2,2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM", "FENICS"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    rotation.degree = 37,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    cat.pos = c (-110,-180,110,160,70),
    margin = 0.05
  )
  return (dv)
}

#' Create quintuple Venn Diagramm for shared documents with co-occurrences of
#' drug names between EpSO, ESSO, EPILONT and EPISEM
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn4doc.png", plot = drawVenn4Doc(), width=240, height=160, 
#'   units = "mm", dpi = 300)
#' }
drawVenn4Doc <- function () {
  
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908")
  
  dv <- VennDiagram::draw.quad.venn(
    area1 = 9202628,
    area2 = 14329391,
    area3 = 1842409,
    area4 = 5293385,
    n12 = 8829177,
    n13 = 1415908,
    n14 = 4220852,
    n23 = 1798705,
    n24 = 5227880,
    n34 = 962481,
    n123 = 1392279,
    n124 = 4193740,
    n134 = 846145,
    n234 = 957044,
    n1234 = 843026,
    lwd = c(1,1,1,1),
    lty = c(2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    margin = 0.05
  )
  return (dv)
}

#' Create quintuple Venn Diagramm for overlapping concepts 
#' between EpSO, ESSO, EPILONT, EPISEM and FENICS
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quintuple.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn5.png", plot = drawVenn5(), width=240, height=160, 
#'   units = "mm", dpi = 300)
#' }
drawVenn5 <- function () {
  cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908",
      "FENICS" = "#008000")
  
  dv <- VennDiagram::draw.quintuple.venn(
    area1 = 1357,
    area2 = 2694,
    area3 = 137,
    area4 = 1591,
    area5 = 141,
    n12 = 142,
    n13 = 21,
    n14 = 47,
    n15 = 0,
    n23 = 48,
    n24 = 471,
    n25 = 0,
    n34 = 25,
    n35 = 0,
    n45 = 0,
    n123 = 0,
    n124 = 0,
    n125 = 0,
    n134 = 0,
    n135 = 0,
    n145 = 0,
    n234 = 0,
    n235 = 0,
    n245 = 0,
    n345 = 0,
    n1234 = 0,
    n1235 = 0,
    n1245 = 0,
    n1345 = 0,
    n2345 = 0,
    n12345 = 0, 
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM", "FENICS"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.75,
    rotation.degree = 0,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1,
    cat.pos = c (-45,-30,-120,90,0),
    margin = -0,1
  )
  return (dv)
}

#' Create quad Venn Diagramm for overlapping concepts 
#' between EpSO, ESSO, EPILONT and EPISEM
#'
#' @return plot object
#' @export
#' @importFrom VennDiagram draw.quad.venn
#' @examples
#' \dontrun{
#' ggplot2::ggsave("venn4.png", plot = drawVenn4(), width=240, height=160, 
#'   units = "mm", dpi = 300)
#' }
drawVenn4 <- function () {
  cols <-
    cols <-
    c("EpSO" = "#7800FE",
      "ESSO" = "#00FFD6",
      "EPILONT" = "#FFA501",
      "EPISEM" = "#E80908")
  
  dv <- VennDiagram::draw.quad.venn(
    area1 = 1357,
    area2 = 2694,
    area3 = 137,
    area4 = 1591,
    n12 = 142,
    n13 = 21,
    n14 = 47,
    n23 = 48,
    n24 = 471,
    n34 = 25,
    n123 = 0,
    n124 = 0,
    n134 = 0,
    n234 = 0,
    n1234 = 0,
    lwd = c(1,1,1,1),
    lty = c(2,2,2,2),
    category = c("EpSO", "ESSO", "EPILONT", "EPISEM"),
    col = cols,
    fill = cols,
    alpha = 0.3,
    fontfamily = "Arial Nova Light",
    fontface = "bold",
    cex = 0.95,
    rotation.degree = 0,
    cat.fontfamily = "Arial Nova Light",
    cat.fontface = "bold",
    cat.cex = 1.3,
    margin = 0.05
  )
  return (dv)
}

#' Create plot_grid from multiple plots
#'
#' @return plot object
#' @export
#' @importFrom cowplot plot_grid
#' @examples
#' \dontrun{
#'   cowplot::plot_grid(drawVenn4 (), drawVenn4Syn(), drawVenn5Doc (), 
#'     drawVenn5DrugDoc ())
#'   ggplot2::ggsave("vennAB.png", plot = cowplot::plot_grid(drawVenn4 (), 
#'     drawVenn4Syn(), labels = c('A', 'B'), ncol = 1), width=240, height=320,  
#'     units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennAB.png", plot = cowplot::plot_grid(drawVenn4 (), 
#'     drawVenn4Syn(), labels = c('Concepts:', 'Synonyms:'), ncol = 1), width=240, 
#'     height=320,  units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennCD.png", plot = cowplot::plot_grid(drawVenn5Doc (), 
#'     drawVenn5DrugDoc(), labels = c('Documents with B-Terms:', 
#'     'Documents with B- and C-Terms:'), ncol = 1), width=240, height=320,  
#'     units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennCD.png", plot = cowplot::plot_grid(drawVenn5Doc (), 
#'     drawVenn5DrugDoc(), labels = c('Documents with B-Terms:', 
#'    'Documents with B- and C-Terms:'), ncol = 1), width=240, height=320,  units = "mm", 
#'    dpi = 300)
#'   ggplot2::ggsave("vennCD.png", plot = cowplot::plot_grid(drawVenn4Doc (), 
#'     drawVenn4DrugDoc(), labels = c('Documents with B-Terms:', 
#'     'Documents with B- and C-Terms:'), ncol = 1), width=240, height=320,  
#'     units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennCD.png", plot = cowplot::plot_grid(drawVenn4Doc (), 
#'     drawVenn4DrugDoc(), labels = c('Documents\nwith B-Terms:             ', 
#'     'Documents\nwith B- and C-Terms:'), ncol = 1), width=240, height=320,  
#'     units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennAB.png", plot = cowplot::plot_grid(drawVenn4 (), 
#'     drawVenn4Syn(), labels = c('i) Concepts:', 'ii) Synonyms:'), ncol = 1), 
#'     width=240, height=320,  units = "mm", dpi = 300)
#'   ggplot2::ggsave("vennCD.png", plot = cowplot::plot_grid(NULL, 
#'     drawVenn4Doc (), drawVenn4DrugDoc(), 
#'     labels = c('iii) Documents with B-Terms:', 
#'     'iv) Documents with B- and C-Terms:'), ncol = 1, 
#'     label_x = c(-0.105, -0.14), label_fontfamily = "Arial Nova Light", 
#'     label_fontface = "bold"), width=240, height=320,  units = "mm", dpi = 300)
#' }
drawVennGrid <- function () {
  plot = cowplot::plot_grid(NULL, drawVenn4Doc (), drawVenn4DrugDoc(), labels = c('iii) Documents with B-Terms:', 'iv) Documents with B- and C-Terms:'), ncol = 1, label_x = c(-0.105, -0.14), label_fontfamily = "Arial Nova Light", label_fontface = "bold")
  return (plot)
}