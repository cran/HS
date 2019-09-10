#' Homogeneous segmentation function with continuous variables.
#'
#' @usage hs(start = "SLK.start", end = "SLK.end", var = "deflection",
#'           data, method = "shs", range = NULL)
#'
#' @param start A character of start location name of a spatial line.
#' @param end A character of end location name of a spatial line.
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param data A data frame of a dataset.
#' @param method A character of homogeneous segmentation method.
#'               Available methods include "shs", "cda" and "mcv".
#' @param range A vector of segment length threshold.
#'
#' @importFrom utils capture.output
#' @importFrom stats aggregate complete.cases median quantile sd
#' @importFrom grDevices hcl
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' hs1 <- hs(start = "SLK.start", end = "SLK.end", var = c("Curvature", "Deflection", "BLI"),
#'           testdata, method = "shs", range = c(0.1, 0.5))
#'
#' @export

hs <- function(start = "SLK.start", end = "SLK.end", var = "deflection",
               data, method = "shs", range = NULL){
  data <- as.data.table(data)

  # preprocessing
  data <- preprocessing(var = var, location = start, data = data)

  # add length # remove system errors of small data
  data[, length := round(get(end) - get(start), digits = 10)]
  if (is.null(range)){
    range <- c(min(data$length), sum(data$length))
  }

  if (sum(data$length) <= range[2]){
    data[, "seg.id" := 1]
  } else {
    # multiple observation variables
    if (method == "shs"){
      data <- shs(var = var, length = "length", data, range = range)
    } else if (method == "mcv"){
      data <- mcv(var = var, length = "length", data, range = range)
    } else if (method == "cda"){
      data <- cda(var = var, length = "length", data, range = range)
      data <- splitlong(var = var, length = "length",
                        seg.id = "seg.id", data, range = range)
    } else {
      stop("Available methods include 'shs', 'mcv' and 'cda'.")
    }
  }

  return(data)
}


