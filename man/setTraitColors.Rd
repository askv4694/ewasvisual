% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processingDistances.R
\name{setTraitColors}
\alias{setTraitColors}
\title{Set trait color}
\usage{
setTraitColors(df, colors, traits)
}
\arguments{
\item{df}{A dataframe that shows connections for visNetwork}

\item{colors}{An array of colors that defines trait color in graph}

\item{traits}{An array of traits that needs to be colored}
}
\value{
A dataframe with changed colors
}
\description{
Set trait color
}
\examples{
df <- setTraitColors(df, c("red", "green"), c("smoking", "aging"))
}
