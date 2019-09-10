#' Split long segments to segments within length threshold.
#'
#' @usage splitlong(var = "deflection", length = "length",
#'                  seg.id = "seg.id", data, range = NULL)
#'
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param length A character of length name.
#' @param seg.id A character of the name of new segment number.
#' @param data A data frame of a dataset.
#' @param range A vector of segment length threshold.
#'
#' @importFrom partitions compositions
#'
#' @examples
#' testdata <- tsdwa[1:1000,]
#' testdata$length <- round(testdata$SLK.end - testdata$SLK.start, digits = 10)
#' testdata <- cda(var = "Deflection", length = "length", testdata, range = c(0.1, 0.5))
#' testdata <- splitlong(var = "Deflection", length = "length",
#'                       seg.id = "seg.id", testdata, range = c(0.1, 0.5))
#' seglength.summary <- testdata[, .(sum(length)), by = .(seg.id)]
#'
#' @export
#'

splitlong <- function(var = "deflection", length = "length",
                      seg.id = "seg.id", data, range = NULL){
  # debug: use two lines to deal with same or different names of length
  lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = data[[seg.id]]))
  seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]

  segid <- seglength.summary[[1]]
  seglength <- round(seglength.summary[[2]], digits = 10)
  # find long segments
  k.longseg <- segid[which(seglength > range[2])]
  if (length(k.longseg) >= 1){
    k.longseg.group <- split(k.longseg, cumsum(c(1, diff(k.longseg) != 1)))

    # split long
    SplitToMultiple <- function(x){
      klgi <- k.longseg.group[[x]]
      data.split <- data[(data[, seg.id] %in% klgi),]
    # segmentation by CDA
      split.result <- cda(var = var, length = length, data.split, range = range)
      data$seg.id[(data[, seg.id] %in% klgi)] <-
        klgi[1] + split.result$seg.id / (max(split.result$seg.id) + 10)
      return(data)
    }

    for (i in 1:length(k.longseg.group)){
      data <- SplitToMultiple(i)
    }
    # replace seg.id by 1:number of new segments
    n.id <- data.frame(table(data$seg.id))
    data$seg.id <- rep(1:nrow(n.id), n.id$Freq)

    # repeat to split long segments
    lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = data[[seg.id]]))
    seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]
    segid <- seglength.summary[[1]]
    seglength <- round(seglength.summary[[2]], digits = 10)

    maxlength <- max(seglength)
    ml <- 0
    if (maxlength > range[2]){
      while(maxlength > range[2]){
        k.longseg <- which(seglength > range[2])
        k.longseg.group <- split(k.longseg, cumsum(c(1, diff(k.longseg) != 1)))
        for (i in 1:length(k.longseg.group)){
          data <- SplitToMultiple(i)
        }
        # replace seg.id by 1:number of new segments
        n.id <- data.frame(table(data$seg.id))
        data$seg.id <- rep(1:nrow(n.id), n.id$Freq)
        # summary
        lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = data[[seg.id]]))
        seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]
        seglength <- round(seglength.summary[[2]], digits = 10)
        maxlength <- max(seglength)
        # stop when maxlength won't change
        ml <- c(maxlength, ml)
        if (ml[1] == ml[2]){
          k.longseg <- which(seglength > range[2])
          k.longseg.group <- split(k.longseg, cumsum(c(1, diff(k.longseg) != 1)))
          for (i in 1:length(k.longseg.group)){
            klgi <- k.longseg.group[[i]]
            data.split.k <- which(data[, seg.id] %in% klgi)
            part.no <- ceiling(sum(data[[length]][data.split.k])/range[2])
            part.k <- rep(1:part.no, each = ceiling(length(data.split.k)/part.no))
            part.k <- part.k[1:length(data.split.k)]
            id.k <- split(data.split.k, part.k)
            for (j in 1:length(id.k)){
              data$seg.id[id.k[[j]]] <- klgi[1] + j / (length(id.k) + 10)
            }
          }
          # replace seg.id by 1:number of new segments
          n.id <- data.frame(table(data$seg.id))
          data$seg.id <- rep(1:nrow(n.id), n.id$Freq)
          # summary
          lengthdata <- as.data.table(cbind(length = data[[length]], seg.id = data[[seg.id]]))
          seglength.summary <- lengthdata[, .(sum(length)), by = .(seg.id)]
          seglength <- round(seglength.summary[[2]], digits = 10)
          maxlength <- max(seglength)
        }
      }
    }

    # segment point
    data$seg.point <- 0
    k <- which(data$seg.id[1:(nrow(data) - 1)] != data$seg.id[2:nrow(data)])
    data$seg.point[c(1, k + 1)] <- 1
  }

  return(data)
}

