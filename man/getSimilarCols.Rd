% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processingDistances.R
\name{getSimilarCols}
\alias{getSimilarCols}
\title{Get similar studies}
\usage{
getSimilarCols(data, col)
}
\arguments{
\item{data}{Matrix that has numerical values of True/False ratio}

\item{col}{An array with TRUE/FALSE which will be for base row
Must have name for reach value.}
}
\value{
Returns an array of studies which are ordered based on similarity
}
\description{
Get similar studies
}
\examples{
odds_and_pvals <- getSimilarCols(data, row)
}
