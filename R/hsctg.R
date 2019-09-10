#' Homogeneous segmentation function with both categorical and continous variables.
#'
#' @usage hsctg(start = "SLK.start", end = "SLK.end", var = "deflection",
#'              data, method = "shs", range = NULL, by.ctg = NULL)
#'
#' @param start A character of start location name of a spatial line.
#' @param end A character of end location name of a spatial line.
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param data A data frame of a dataset.
#' @param method A character of homogeneous segmentation method.
#'               Available methods include "shs", "cda" and "mcv".
#' @param range A vector of segment length threshold.
#' @param by.ctg A vector of categorical variable names.
#'
#' @examples
#' testdata <- tsdwa[1:300,]
#' hc1 <- hsctg(start = "SLK.start", end = "SLK.end", var = c("Curvature", "Deflection", "BLI"),
#'           testdata, method = "shs", range = c(0.1, 0.5), by.ctg = c("SurfType", "PvtType"))
#'
#' @export

hsctg <- function(start = "SLK.start", end = "SLK.end", var = "deflection",
                  data, method = "shs", range = NULL, by.ctg = NULL){
  data <- as.data.table(data)
  data[, length := round(get(end) - get(start), digits = 10)]
  # segmentation by categorical variables
  data <- segbycategory(data, by = by.ctg)
  # select long segments for segmentation
  segctg.length <- data[, .(length = sum(length)), by = seg.ctg]
  k <- which(segctg.length$length > range[2])
  # loop segmentation within each category
  data[, "seg.id" := 0]
  for (i in k){
    datai <- data[which(data$seg.ctg == i), ]
    datai <- hs(start = start, end = end, var = var,
                datai, method = method, range = range)
    data$seg.id[which(data$seg.ctg == i)] <- datai$seg.id
  }
  digitk <- 1/(max(segctg.length$length)/min(data$length)*20)
  segid <- data$seg.ctg + data$seg.id * digitk
  segid <- table(segid)
  segid <- rep(1:length(segid), segid)
  data$seg.id <- segid
  # result
  return(data)
}
