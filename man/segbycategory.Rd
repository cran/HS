% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segbycategory.R
\name{segbycategory}
\alias{segbycategory}
\title{Segmentation with categorical variables.}
\usage{
segbycategory(data, by = NULL)
}
\arguments{
\item{data}{A data frame of a dataset.}

\item{by}{A character or a vector of categorical variable names.}
}
\description{
Segmentation with categorical variables.
}
\examples{
testdata <- tsdwa[1:100,]
testdata <- segbycategory(testdata, by = c("SurfType", "PvtType"))

}
