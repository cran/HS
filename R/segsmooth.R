#' Smoothing data using the moving average method for the homogeneous segmentation.
#'
#' @description A center aligned moving window is used for the moving average method.
#'
#' @usage segsmooth(var = "deflection", range = 11, data)
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
#' testdata <- tsdwa[1:500,]
#' testdata <- preprocessing(var = "Deflection", location = "SLK.start", data = testdata)
#' # smoothing
#' testdata <- segsmooth(var = "Deflection", range = 11, data = testdata)
#' # plot
#' plot(testdata$SLK.start, testdata$Deflection, type = "l",
#'      col = "lightblue", xlab = "location", ylab = "deflection")
#' lines(testdata$SLK.start, testdata$smooth.Deflection)
#'
#' @export

segsmooth <- function(var = "deflection", range = 11, data){
  data <- as.data.table(data)
  # smooth
  x <- rollapply(data[, ..var], width = range, FUN = function(x) mean(x, na.rm=TRUE),
                 by = 1, by.column = TRUE, partial = TRUE, fill = NA, align = "center")
  x <- data.frame(x)
  x.names <- paste("smooth", var, sep = ".")
  names(x) <- x.names
  data <- cbind(data, x)
  return(data)
}

