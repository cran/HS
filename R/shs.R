#' Spatial heterogeneity-based segmentation (SHS) for homogeneous segmentation of spatial lines data.
#'
#' @usage shs(var = "deflection", length = "length", data, range = NULL)
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
#' testdata <- shs(var = "Deflection", length = "length", testdata, range = c(0.1, 0.5))
#'
#' @export

shs <- function(var = "deflection", length = "length", data, range = NULL){
  data <- as.data.table(data)
  n.var <- length(var)

  cumq <- function(data){ # debug: use cumq to calculate q values along all segment points
    n <- length(data)
    cum.n <- 1:(n - 1)
    data.left <- data[cum.n]
    data.right <- rev(data)[cum.n]
    cum.data.left <- cumsum(data.left)
    cum.data.right <- rev(cumsum(data.right))
    sumd <- sum(data)
    cum.datasquare.left <- cumsum(data.left * data.left)
    cum.datasquare.right <- rev(cumsum(data.right * data.right))

    cqvalue <- 1 - ((cum.datasquare.left - cum.data.left * cum.data.left/cum.n) +
      (cum.datasquare.right - cum.data.right * cum.data.right/rev(cum.n)))/
      (sum(data * data) - sumd * sumd/n)
    return(cqvalue)
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

    qvalue <- matrix(NA, length(k), n.var)
    for (i in 1:n.var){
      qvalue[, i] <- cumq(data.var[[i]])[k - 1] # debug: use cumq function instead of q function
    }
    qvalue <- rowMeans(qvalue)
    maxk <- which(qvalue == max(qvalue)) + max(k.left)
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



