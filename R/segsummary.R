#' Statistical summary of homogeneous segments.
#'
#' @usage segsummary(start = "SLK.start", end = "SLK.end", var = "deflection",
#'                   seg.id = "seg.id", data, by.ctg = NULL)
#' \method{print}{segsummary}(x, ...)
#'
#' @aliases segsummary print.segsummary
#'
#' @param start A character of start location name of a spatial line.
#' @param end A character of end location name of a spatial line.
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param seg.id A character of the name of new segment number.
#' @param data A data frame of a dataset.
#' @param by.ctg A character of categorical variable names.
#' @param x A list of segmentation result.
#' @param ... Ignore
#'
#' @importFrom data.table setkey
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata$length <- testdata$SLK.end - testdata$SLK.start
#' testdata <- shs(var = c("Curvature", "Deflection"), length = "length",
#'                 testdata, range = c(0.1, 0.5))
#' s1 <- segsummary(start = "SLK.start", end = "SLK.end", var = c("Curvature", "Deflection"),
#'                  seg.id = "seg.id", testdata)
#' s1
#'
#' @export

segsummary <- function(start = "SLK.start", end = "SLK.end", var = "deflection",
                       seg.id = "seg.id", data, by.ctg = NULL){
  data <- as.data.table(data)

  cv <- function(x) sd(x)/mean(x)
  q75 <- function(x) quantile(x, 0.75)

  # segments
  summary.seg <- data[, .(start = get(start)[1], end = get(end)[.N],
                          length = sum(length), nobs = .N),
                           by = .(seg.id)] # debug: use get
  names(summary.seg)[2:3] <- c(start, end)

  # categorical variables
  if (!is.null(by.ctg)){
    vi <- matrix(NA, nrow(summary.seg), length(by.ctg))
    for (i in 1:length(by.ctg)){
      vari <- by.ctg[i]
      vari.ctg <- data[, .(vari = get(vari)[1]), by = .(seg.id)][[2]]
      vi[, i] <- as.character(vari.ctg)
    }
    summary.ctg <- as.data.table(vi)
    colnames(summary.ctg) <- by.ctg
  } else {
    summary.ctg <- NA
  }

  # cv summary
  summary.data <- list()
  cv.list <- list()
  for (i in 1:length(var)){
    setkey(data, seg.id)
    summary.data[[i]] <- data[, .(start = get(start)[1], end = get(end)[.N],
                                  mean = mean(get(var[i])), median = median(get(var[i])),
                                  q75 = q75(get(var[i])), max = max(get(var[i])),
                                  sd = sd(get(var[i])), cv = cv(get(var[i])),
                                  nobs = .N,
                                  variable = factor(var[i], levels = var),
                                  length = sum(length)),
                              by = .(seg.id)] # debug: use get
    names(summary.data[[i]])[2:3] <- c(start, end)
    cv.list[[i]] <- summary.data[[i]]$cv
  }
  names(summary.data) <- var

  cvdata <- as.data.table(do.call(cbind, cv.list))
  names(cvdata) <- var
  cvdata[, All := rowMeans(.SD)]
  summary.cv <- as.data.table(cbind(summary.seg, cvdata))

  # cv statistics
  cv1 <- summary.cv[, -c(1:5)]
  cv1 <- cv1[, lapply(.SD, mean, na.rm = TRUE)]

  names.cv1 <- names(cv1)
  cv1 <- as.numeric(cv1)
  names(cv1) <- names.cv1

  seg.summary <- list("seg.cv" = summary.cv, "summary" = summary.data,
                      "seg.ctg" = summary.ctg, "mean.cv" = cv1)

  class(seg.summary) <- "segsummary"
  return(seg.summary)
}

print.segsummary <- function(x, ...){
  seg.cv <- x$seg.cv
  # print: total number of segments
  n.seg <- nrow(seg.cv)
  message(n.seg, " homogenous segments are generated.")
  # print: cv statistics
  message("Mean CV of homogenous segments:")
  message(paste0(capture.output(round(x$mean.cv, digits = 4)), collapse = "\n"))

  # print
  # percentage
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }

  k1 <- which(seg.cv[, All] <= 0.25)
  k2 <- which(seg.cv[, All] <= 0.10)
  # print percentages of count and length
  if (length(k1) > 0){
    message("CV values of ", percent(length(k1)/n.seg), " segments are lower than 0.25.")
    message("CV values of ", percent(sum(seg.cv[k1, length])/sum(seg.cv[, length])),
            " length of segments are lower than 0.25.")
  }
  if (length(k2) > 0){
    message("CV values of ", percent(length(k2)/n.seg), " segments are lower than 0.10.")
    message("CV values of ", percent(sum(seg.cv[k2, length])/sum(seg.cv[, length])),
            " length of segments are lower than 0.10.")
  }
  invisible(x)
}

