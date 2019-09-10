#' Find spaital breaking locations and add a column of breaks.
#'
#' @usage findbreak(start = "SLK.start", end = "SLK.end", data,
#'                  dist.allow = 0.05, line.no = NULL)
#'
#' @param start A character of start location name of a spatial line.
#' @param end A character of end location name of a spatial line.
#' @param data A data frame of a dataset.
#' @param dist.allow A number of the maximum allowed breaks within a line segment.
#' @param line.no A character of spatial line name.
#'
#' @importFrom data.table as.data.table
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata <- findbreak(start = "SLK.start", end = "SLK.end",
#'                       data = testdata, dist.allow = 0.05)
#'
#' @export

findbreak <- function(start = "SLK.start", end = "SLK.end", data,
                      dist.allow = 0.05, line.no = NULL){
  data <- as.data.table(data)
  n <- nrow(data)
  # location breaks
  slkdif <- round(data[1:(n - 1), get(end)] - data[2:n, get(start)], digits = 10)
  k.location <- which(abs(slkdif) > dist.allow)
  if (is.null(line.no)){
    k.line <- c()
  } else {
    k.line <- which(data[2:n, get(line.no)] != data[1:(n - 1), get(line.no)])
  }
  message(length(k.location) + 1, " breaks are found on ", length(k.line) + 1, " spatial lines.")
  kk <- sort(unique(c(k.location, k.line))) + 1
  # add a column of breaks
  k1 <- c(1, kk)
  k2 <- c(kk - 1, n)
  ll <- k2 - k1 + 1
  data$breaks <- rep(1:length(ll), ll)
  return(data)
}
