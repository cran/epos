% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawVennDiagramms.R
\name{drawVenn4DrugDoc}
\alias{drawVenn4DrugDoc}
\title{Create quad Venn Diagramm for shared documents with co-occurrences of
drug names between EpSO, ESSO, EPILONT and EPISEM}
\usage{
drawVenn4DrugDoc()
}
\value{
plot object
}
\description{
Create quad Venn Diagramm for shared documents with co-occurrences of
drug names between EpSO, ESSO, EPILONT and EPISEM
}
\examples{
\dontrun{
ggplot2::ggsave("venn4drugdoc.png", plot = drawVenn4DrugDoc(), width=240, 
  height=160, units = "mm", dpi = 300)
}
}
