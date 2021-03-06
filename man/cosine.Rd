% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcSimilarities.R
\name{cosine}
\alias{cosine}
\title{Calculate cosine similarity metric}
\usage{
cosine(ainterb, lengtha, lengthb)
}
\arguments{
\item{ainterb}{integer value with number of intersecting elements between set a and b}

\item{lengtha}{integer value with the number of items in set a}

\item{lengthb}{integer value with the number of items in set b}
}
\value{
cosine double vlaue with the cosine similarity coefficient
}
\description{
Calculate cosine similarity metric
}
\examples{
cosine(1,3,4)
}
