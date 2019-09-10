#' Minimization coefficient of variation (MCV) for homogeneous segmentation of spatial lines data.
#'
#' @usage mcv(var = "deflection", length = "length", data, range = NULL)
#'
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param length A character of road length name in data.
#' @param data A data frame of a dataset.
#' @param range A vector of segment length threshold.
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata$length <- testdata$SLK.end - testdata$SLK.start
#' testdata <- mcv(var = "Deflection", length = "length", testdata, range = c(0.1, 0.5))
#'
#' @export

mcv <- function(var = "deflection", length = "length", data, range = NULL){
  data <- as.data.table(data)
  n.var <- length(var)

  cump <- function(data){ # debug: use cump to calculate p values along all segment points
    n <- length(data)
    cum.n <- 1:(n - 1)
    n1 <- cum.n; n2 <- rev(cum.n)
    data.left <- data[cum.n]
    data.right <- rev(data)[cum.n]
    cum.data.left <- cumsum(data.left)
    cum.data.right <- rev(cumsum(data.right))
    cum.datasquare.left <- cumsum(data.left * data.left)
    cum.datasquare.right <- rev(cumsum(data.right * data.right))

    cpvalue <- (sqrt((n1 * cum.datasquare.left /(cum.data.left * cum.data.left) - 1) * n1/(n1 - 1)) +
                  sqrt((n2 * cum.datasquare.right /(cum.data.right * cum.data.right) - 1) * n2/(n2 - 1)) )/2
    return(cpvalue)
  }

  min.length <- range[1]
  max.length <- range[2]

  seg2 <- function(data, min.length){
    data.var <- data[, ..var]
    data.length <- data[[length]]
    cumlength.left <- cumsum(data.length); cumlength.right <- cumsum(rev(data.length))
    k.left <- c(1, which(cumlength.left <= min.length) + 1)
    k.right <- nrow(data.var) - c(0, which(cumlength.right <= min.length))
    k <- c(1:nrow(data.var))[-c(k.left, k.right)]

    pvalue <- matrix(NA, length(k), n.var)
    for (i in 1:n.var){
      pvalue[, i] <- cump(data.var[[i]])[k - 1] # debug: use cump function instead of p function
    }
    pvalue <- rowMeans(pvalue)
    maxk <- which(pvalue == min(pvalue)) + max(k.left)
    return(maxk)
  }
  # first segmentation
  ss <- seg2(data, min.length)
  # following segmentation
  k1 <- c(1, ss)
  k2 <- c(ss - 1, nrow(data))
  ll <- k2 - k1 + 1
  cum.ll <- c(0, cumsum(ll))
  segid <- c(rep(1:length(ll), ll))
  segdatalist <- split(data, segid)
  lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = segid))
  seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]
  seglength <- round(seglength.summary[[2]], digits = 10)
  k <- which(seglength > max.length)

  while(length(k) > 0){
    sa <- sapply(k, function(x){
      seg2(segdatalist[[x]], min.length) + cum.ll[x]
    })
    ss <- sort(c(ss, sa))

    k1 <- c(1, ss)
    k2 <- c(ss - 1, nrow(data))
    ll <- k2 - k1 + 1
    cum.ll <- c(0, cumsum(ll))
    segid <- c(rep(1:length(ll), ll))
    segdatalist <- split(data, segid)
    lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = segid))
    seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]
    seglength <- round(seglength.summary[[2]], digits = 10)
    k <- which(seglength > max.length)
  }
  # add seg.id
  k1 <- c(1, ss)
  k2 <- c(ss - 1, nrow(data))
  ll <- k2 - k1 + 1
  data$seg.id <- rep(1:length(ll), ll)
  data$seg.point <- 0
  data$seg.point[c(1, ss)] <- 1
  return(data)
}



