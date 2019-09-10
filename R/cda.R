#' Cummulative difference approach (CDA) for homogeneous segmentation of spatial lines data.
#'
#' @description Function for homogeneous segmentation of spatial lines data using
#'              a cummulative difference approach (CDA).
#'
#' @usage cda(var = "deflection", length = "length", data, range = NULL)
#'
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param length A character of road length name in data.
#' @param data A data frame of a dataset.
#' @param range A vector of length threshold.
#'
#' @importFrom data.table as.data.table
#'
#' @examples
#' testdata <- tsdwa[1:100,]
#' testdata$Length <- testdata$SLK.end - testdata$SLK.start
#' testdata <- cda(var = "Deflection", length = "Length", testdata)
#'
#' @export

cda <- function(var = "deflection", length = "length", data, range = NULL){
  data <- as.data.table(data)

  nrow1 <- nrow(data)
  min.length <- range[1]
  max.length <- range[2]

  data.length <- data[[length]]

  ai <- data[, ..var] * data.length
  ai[2:nrow1, ] <- (data[1:(nrow1 - 1), ..var] + data[2:nrow1, ..var])/2 * data[[length]][2:nrow1]

  zx <- ai
  sum.length <- sum(data.length)
  cumsum.length <- cumsum(data.length)
  for (i in 1:length(var)){
    sum.ai <- sum(ai[[i]])
    cumsum.ai <- cumsum(ai[[i]]) ## debug: replace loop by cumsum
    pi <- cumsum.length/sum.length*sum.ai
    zi <- cumsum.ai - pi
    zx[[i]] <- zi
  }

  meanzx <- zx[, rowMeans(.SD)] # mean zx

  data$slopezx <- meanzx[2] - meanzx[1]
  data$slopezx[2:nrow1] <- meanzx[2:nrow1] - meanzx[1:(nrow1 - 1)]
  # segment id and segment point
  k <- which(sign(data$slopezx[1:(nrow1 - 1)]) != sign(data$slopezx[2:nrow1]))
  # select k: length > min(range) to remove short roads
  if (!is.null(range)){
    # solve edge
    cumlength.left <- cumsum(data.length); cumlength.right <- cumsum(rev(data.length))
    k.left <- c(1, which(cumlength.left <= min.length) + 1)
    k.right <- nrow1 - c(0, which(cumlength.right <= min.length))
    k.in.edge <- which(k %in% c(k.left, k.right))
    if (length(k.in.edge) != 0){
      k <- k[-which(k %in% c(k.left, k.right))]
    }
    # remove short
    cumlength.k <- c(0, cumsum.length[c(k, nrow1)])
    cumlength.kdif <- cumlength.k[2:length(cumlength.k)] - cumlength.k[1:(length(cumlength.k) - 1)]
    k.short <- which(cumlength.kdif < range[1])
    k <- k[-k.short]
  }
  # segment id
  k1 <- c(1, k + 1)
  k2 <- c(k, nrow1)
  seg.id <- 1:length(k1)
  data$seg.id <- 0
  for (i in seg.id){
    data$seg.id[k1[i]:k2[i]] <- seg.id[i]
  }
  # segment point
  data$seg.point <- 0
  data$seg.point[c(1, k + 1)] <- 1

  return(data)
}
