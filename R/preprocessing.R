#' Preprocessing for field monitoring data.
#'
#' @description The preprocessing includes two steps:
#'              removing missing data and ordering data by spatial locations.
#'
#' @usage preprocessing(var = "deflection", location = "SLK", data = data)
#'
#' @param var A character of the name of a variable in a dataset,
#'            such as a road pavement performance indicator.
#' @param location A character of the name of spatial locations in a dataset.
#' @param data A data frame of monitoring data.
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata <- preprocessing(var = "Deflection", location = "SLK.start", data = testdata)
#'
#' @export

preprocessing <- function(var = "deflection", location = "SLK", data = data){
  data <- as.data.table(data)
  # remove NA
  data2 <- data[complete.cases(data[, ..var]), ]
  rownames(data2) <- c()
  k <- nrow(data) - nrow(data2)
  if (k > 0){
    message(k, " rows of missing data are removed. \n")
  }
  # sort location
  data2 <- data2[order(data2[, ..location]),]

  return(data2)
}

