% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hs.R
\name{hs}
\alias{hs}
\title{Homogeneous segmentation function with continuous variables.}
\usage{
hs(start = "SLK.start", end = "SLK.end", var = "deflection",
          data, method = "shs", range = NULL)
}
\arguments{
\item{start}{A character of start location name of a spatial line.}

\item{end}{A character of end location name of a spatial line.}

\item{var}{A character or a character vector of variable names,
such as a road pavement performance indicator.}

\item{data}{A data frame of a dataset.}

\item{method}{A character of homogeneous segmentation method.
Available methods include "shs", "cda" and "mcv".}

\item{range}{A vector of segment length threshold.}
}
\description{
Homogeneous segmentation function with continuous variables.
}
\examples{
testdata <- tsdwa[1:100,]
hs1 <- hs(start = "SLK.start", end = "SLK.end", var = c("Curvature", "Deflection", "BLI"),
          testdata, method = "shs", range = c(0.1, 0.5))

}
