#' Segmentation with categorical variables.
#'
#' @usage segbycategory(data, by = NULL)
#'
#' @param data A data frame of a dataset.
#' @param by A character or a vector of categorical variable names.
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata <- segbycategory(testdata, by = c("SurfType", "PvtType"))
#'
#' @importFrom tidyr unite
#'
#' @export

segbycategory <- function(data, by = NULL){
  if (is.null(by)){
    data$seg.ctg <- 1
  } else {
    data <- as.data.table(data)
    # combine multiple columns of variables to a column
    initialseg <- unite(data[, ..by], by)
    # segment id
    k <- which(initialseg$by[1:(length(initialseg$by) - 1)] !=
                 initialseg$by[2:length(initialseg$by)])
    k1 <- c(1, k + 1)
    k2 <- c(k, length(initialseg$by))
    seg.ctg <- 1:length(k1)
    data$seg.ctg <- 0
    for (i in seg.ctg){
      data$seg.ctg[k1[i]:k2[i]] <- seg.ctg[i]
    }
  }
  return(data)
}


