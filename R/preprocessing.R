#' Preprocessing for field monitoring data.
#'
#' @description The preprocessing includes two steps:
#' removing missing data and ordering data by spatial locations.
#'
#' @usage preprocessing(var = "Deflection", location = "SLK", data = data)
#'
#' @param var A character of the name of a variable in a dataset,
#'            such as a road pavement performance indicator.
#' @param location A character of the name of spatial locations in a dataset.
#' @param data A data frame of monitoring data.
#'
#' @examples
#' def <- preprocessing(var = "Deflection", location = "SLK", data = deflection)
#'
#' @export

preprocessing <- function(var = "Deflection", location = "SLK", data = data){
  # remove NA
  k <- length(which(is.na(data[, which(colnames(data) == var)])))
  if (k > 0)
    message(k, " rows of missing data are removed. \n")

  data <- data[which(!is.na(data[, which(colnames(data) == var)])),]
  # sort location
  data <- data[order(data[, which(colnames(data) == location)]),]

  return(data)
}

