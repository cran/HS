% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segplot.R
\name{segplot}
\alias{segplot}
\title{Visualization of homogeneous segments.}
\usage{
segplot(start = "SLK.start", var = "deflection",
               seg.id = "seg.id", data, plot.range = NULL)
}
\arguments{
\item{start}{A character of start location name of a spatial line.}

\item{var}{A character or a character vector of variable names,
such as a road pavement performance indicator.}

\item{seg.id}{A character of the name of new segment number.}

\item{data}{A data frame of a dataset.}

\item{plot.range}{A vector of plot range.}
}
\description{
Visualization of homogeneous segments.
}
\examples{
testdata <- tsdwa[1:300,]
testdata$length <- testdata$SLK.end - testdata$SLK.start
testdata <- shs(var = c("Curvature", "Deflection"), length = "length",
                testdata, range = c(0.1, 0.5))
segplot(start = "SLK.start", var = c("Curvature", "Deflection"),
              seg.id = "seg.id", testdata, plot.range = 1:300)

}
