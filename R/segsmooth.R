#' Smoothing data using the moving average method for the homogenous segmentation.
#'
#' @description A center aligned moving window is used for the moving average method.
#'
#' @usage segsmooth(var = "Deflection", range = 11, data = data)
#'
#' @param var A character of the name of a variable in a dataset,
#'            such as a road pavement performance indicator.
#' @param range A number of the size of moving window. An odd number is required.
#' @param data A data frame of monitoring data.
#'
#' @importFrom zoo rollapply
#'
#' @examples
#' # preprocessing
#' def <- preprocessing(var = "Deflection", location = "SLK", data = deflection)
#' # smoothing
#' def$smooth_def <- segsmooth(var = "Deflection", range = 11, data = def)
#' # plot
#' n <- 1:500
#' plot(def$SLK[n], def$Deflection[n], type = "l",
#'      col = "lightblue", xlab = "SLK", ylab = "Deflection")
#' lines(def$SLK[n], def$smooth_def[n])
#'
#' @export

segsmooth <- function(var = "Deflection", range = 11, data = data){
  # smooth
  x <- rollapply(data[, which(colnames(data) == var)], width = range, FUN = function(x) mean(x, na.rm=TRUE),
                 by = 1, by.column = TRUE, partial = TRUE, fill = NA, align = "center")
  return(x)
}
