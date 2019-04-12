#' Homogenous segmentation for spatial lines data.
#'
#' @description Function for homogenous segmentation for spatial lines data using
#'              a moving coefficient of variation (CV) method.
#'
#' @usage seg(var = "Deflection", location = "SLK", interval = interval, unit = 10, data = data)
#' \method{print}{seg}(x, ...)
#' \method{plot}{seg}(x, range = 1:300, legend_location = "topright", ...)
#'
#' @aliases seg print.seg plot.seg
#'
#' @param var A character of the name of a variable in a dataset,
#'            such as a road pavement performance indicator.
#' @param location A character of the name of spatial locations in a dataset.
#' @param interval A vector of available segmentation intervals. The length of the vector is longer than 1.
#' @param unit A number of the unit of the interval data.
#' @param data A data frame of monitoring data.
#' @param x A list of homogenous segmentation result.
#' @param range A vector of a range of plot data.
#' @param legend_location A character of legend location.
#' @param ... Ignore.
#'
#' @importFrom graphics abline legend lines par plot
#' @importFrom stats aggregate sd median quantile
#'
#' @examples
#' # preprocessing
#' def <- preprocessing(var = "Deflection", location = "SLK", data = deflection)
#' # smoothing
#' def$smooth_def <- segsmooth(var = "Deflection", range = 11, data = def)
#' # segmentation
#' interval <- seq(100, 500, 20)
#' seg1 <- seg(var = "smooth_def", location = "SLK", interval = interval, unit = 10, data = def)
#' seg1
#' plot(seg1, range = 1:994, legend_location = "topleft")
#'
#' @export

seg <- function(var = "Deflection", location = "SLK", interval = interval, unit = 10, data = data){

  # add id
  data$id <- 1:nrow(data)
  data <- data[, match(c("id", location, var), colnames(data))]

  # function cv
  cv <- function(x) sd(x)/mean(x)

  # cv matrix
  int_unit <- interval/unit
  cvdata <- matrix(NA, nrow(data), length(int_unit))
  for (i in 1:length(int_unit)){
    width <- int_unit[i] + 1
    cvi <- rollapply(data[, which(colnames(data) == var)], width = width,
                    FUN = function(x) sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE),
                    by = 1, by.column = TRUE, partial = TRUE, fill = NA, align = "center")
    cvdata[, i] <- cvi
  }
  # min cv and corresponding segment length
  data$mincv <- apply(cvdata, 1, min, na.rm = TRUE)
  mincv_location <- apply(cvdata, 1, function(x) {if (all(is.na(x))) {NA}  else {which.min(x)} })
  int_by <- (interval[2] - interval[1])
  data$mincv_seg <- mincv_location * int_by + (2 * interval[1] - interval[2])

  # segmentation
  data$seg0 <- NA

  # initial segmentation
  for (j in 1:21){
    hj <- interval[j]

    if (length(which(data$mincv_seg == hj)) == 0)
      next
    else
      h1 <- which(data$mincv_seg == hj)

    h1 <- c(0, h1, (nrow(data) + 1))
    if (length(h1) < 3)
      next
    else
    h2 <- c()
    h3 <- c()
    for (i in 2:(length(h1) - 1)){
      if (h1[i] != h1[i - 1] + 1){
        h2 <- c(h2, h1[i])
      }
      if (h1[i] != h1[i + 1] - 1){
        h3 <- c(h3, h1[i])
      }
    }
    h2 <- h2[1:(min(length(h2), length(h3)))]
    h3 <- h3[1:(min(length(h2), length(h3)))]

    if (is.null(h2) | is.null(h3))
      next
    else

    seglength <- hj
    seginterval <- hj/int_by

    for (i in 1:length(h2)){
      a <- h2[i]:h3[i]
      # step 1: min cv within an interval
      m <- a[which(data$mincv[a] == min(data$mincv[a]))[1]]

      if (m - seginterval <= 0){
        t1 <- 1
        t2 <- m + seginterval
      } else if (m + seginterval >= nrow(data)) {
        t1 <- m - seginterval
        t2 <- nrow(data)
      } else {
        t1 <- m - seginterval
        t2 <- m + seginterval
      }
      # step 2: min cv within the segment (no replacement, so no use)
      if (data$mincv[m] == min(data$mincv[t1:t2])){
        data$seg0[t1:t2] <- m ## add id to new segment
      }
    }
  }


  # segmentation for seg0 == NA & length > min(initial)/unit; repeat 5 times
  # previous roads won't be replaced
  for (k in 1:5){

    for (j in 1:21){
      hj <- interval[j]

      if (length(which(is.na(data$seg0) & data$mincv_seg == hj)) == 0)
        next
      else
        h1 <- which(is.na(data$seg0) & data$mincv_seg == hj)

      h1 <- c(0, h1, (nrow(data) + 1))
      if (length(h1) < 3)
        next
      else
      h2 <- c()
      h3 <- c()
      for (i in 2:(length(h1) - 1)){
        if (h1[i] != h1[i - 1] + 1){
          h2 <- c(h2, h1[i])
        }
        if (h1[i] != h1[i + 1] - 1){
          h3 <- c(h3, h1[i])
        }
      }
      h2 <- h2[1:(min(length(h2), length(h3)))]
      h3 <- h3[1:(min(length(h2), length(h3)))]

      if (is.null(h2) | is.null(h3))
        next
      else

      seglength <- hj
      seginterval <- hj/int_by

      for (i in 1:length(h2)){
        a <- h2[i]:h3[i]
        if (length(a) < min(seginterval * 2 + 1)) # this step is for relatively long road
          next
        else
        # step 1: min cv within an interval
        a2 <- a[-c(1:seginterval, (length(a) - seginterval + 1):length(a))]
        m <- a2[which(data$mincv[a2] == min(data$mincv[a2]))[1]]

        if (m - seginterval <= 0){
          t1 <- 1
          t2 <- m + seginterval
        } else if (m + seginterval >= nrow(data)) {
          t1 <- m - seginterval
          t2 <- nrow(data)
        } else {
          t1 <- m - seginterval
          t2 <- m + seginterval
        }
        # step 2: min cv within the segment (no replacement, so no use)
        data$seg0[t1:t2] <- m ## add id to new segment
      }
    }
    # end repeat
  }

  # add seg0 to remained NA
  k <- which(is.na(data$seg0))
  data$seg0[k[1]] <- data$id[k[1]]
  for (i in 2:length(k)){
    if (data$mincv_seg[k[i]] == data$mincv_seg[k[i-1]] & data$id[k[i]] == (data$id[k[i-1]]+1))
      data$seg0[k[i]] <- data$seg0[k[i-1]]
    else
      data$seg0[k[i]] <- data$id[k[i]]
  }

  ### remove shorter than min(interval)
  kk0 <- data$seg0
  for (u in 1:10){
    kk1 <- kk0

    n1 <- as.data.frame(table(kk1))
    n1$Accu1 <- 0
    n1$Accu2 <- 0
    n1$Accu1[1] <- 1
    n1$Accu2[1] <- n1$Freq[1]
    for (i in 2:(nrow(n1) - 1)){
      n1$Accu2[i] <- sum(n1$Freq[1:i])
    }
    n1$Accu1[2:(nrow(n1))] <- n1$Accu2[1:(nrow(n1)-1)] + 1
    n1$Accu2[nrow(n1)] <- nrow(data)

    # test first and last segment
    if (n1$Freq[1] < 10){
      kk1[n1$Accu1[1] : n1$Accu2[1]] <- kk1[n1$Accu1[2]]
    }
    if (n1$Freq[nrow(n1)] < 10){
      kk1[n1$Accu1[nrow(n1)] : n1$Accu2[nrow(n1)]] <- kk1[n1$Accu1[nrow(n1)-1]]
    }
    # remove short segments
    for (i in 2:(nrow(n1) - 1)){
      if (n1$Freq[i] < 10) {
        a1 <- c(n1$Accu1[i - 1] : n1$Accu2[i])
        a2 <- c(n1$Accu1[i] : n1$Accu2[i + 1])
        cv1 <- cv(data[a1, which(colnames(data) == var)])
        cv2 <- cv(data[a2, which(colnames(data) == var)])

        if (cv1 <= cv2){
          kk1[n1$Accu1[i] : n1$Accu2[i]] <- kk1[n1$Accu1[i - 1]]
        } else {
          kk1[n1$Accu1[i] : n1$Accu2[i]] <- kk1[n1$Accu1[i + 1]]
        }
      }
    }
    kk0 <- kk1
  }
  data$seg1 <- kk0

  # summary
  n2 <- as.data.frame(table(data$seg1))
  segcv <- aggregate(data[, which(colnames(data) == var)], list(data$seg1), FUN = cv)
  n2$segcv <- segcv$x

  segid <- 1:nrow(n2)
  segcv <- cbind(segid, n2[, 2:3])

  # add new segid
  k <- match(data$seg1, n2$Var1)
  data$segid <- segid[k]

  # return results
  results <- list("segdata" = data, "segcv" = segcv, "cvmatrix" = cvdata)

  # define class
  class(results) <- "seg"
  results
}


print.seg <- function(x, ...){
  # percentage
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }

  segcv <- x$segcv

  message(nrow(segcv), " homogenous segments are generated.\n")
  message("Mean CV of homogenous segments is ", format(round(mean(segcv$segcv), 2), nsmall = 3), ".\n")

  k1 <- which(segcv$segcv <= 0.25)
  # print percentages of count and length
  if (length(k1) > 0){
    message("The CV values of ", percent(length(k1)/nrow(segcv)), " segments are lower than 0.25.\n")
    message("The CV values of ", percent(sum(segcv$Freq[k1])/sum(segcv$Freq)), " length of segments are lower than 0.25.\n")
  }

  invisible(x)
}


plot.seg <- function(x, range = 1:300, legend_location = "topright", ...){
  segdata <- x$segdata
  segcv <- x$segcv
  cvmatrix <- x$cvmatrix

  segdata$vline <- 0
  segdata$segid2 <- segdata$segid
  segdata$segid2[1:(nrow(segdata) - 1)] <- segdata$segid[2:nrow(segdata)]
  k <- which(segdata$segid != segdata$segid2)
  segdata$vline[k] <- 1

  mean_def <- aggregate(segdata$smooth_def, list(segdata$segid), mean)
  median_def <- aggregate(segdata$smooth_def, list(segdata$segid), median)
  q75_def <- aggregate(segdata$smooth_def, list(segdata$segid), FUN = function(x) quantile(x, 0.75))
  k <- match(segdata$segid, mean_def$Group.1)
  segdata$mean_def <- mean_def$x[k]
  segdata$median_def <- median_def$x[k]
  segdata$q75_def <- q75_def$x[k]

  k <- match(segdata$segid, segcv$segid)
  segdata$segcv <- segcv$segcv[k]

  # add vline
  segdata$vline[which(segdata$vline == 1)] <- segdata[which(segdata$vline == 1), 2]

  par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
  # plot raw data
  plot(segdata[range, 2], segdata[range, 3], type = "l", xlab = "Location", ylab = "var")
  lines(segdata[range, 2], segdata$mean_def[range], type = "l", col = "blue")
  lines(segdata[range, 2], segdata$median_def[range], type = "l", col = "blueviolet")
  lines(segdata[range, 2], segdata$q75_def[range], type = "l", col = "darkorange")
  abline(v = segdata$vline[range], col = "red")
  legend(legend_location,
         legend = c("Mean", "Median", "75% quantile", "Segment"),
         col = c("blue", "blueviolet", "darkorange", "red"),
         text.width = 1, x.intersp = 0.5, y.intersp = 0.5,
         lty = 1, cex = 0.8, inset = 0.02, xpd = TRUE)

  # plot cv
  y1 <- c(min(cvmatrix[range, ]), max(cvmatrix[range, ]))
  x1 <- c(min(segdata[range, 2]), max(segdata[range, 2]))
  plot(NA, xlim = x1, ylim = y1, xlab = "Location", ylab = "Coefficient of variance (CV)")
  for (i in 2:(ncol(cvmatrix) - 1)){
    lines(segdata[range, 2], cvmatrix[range, i], type = "l", col = "gray")
  }
  lines(segdata[range, 2], cvmatrix[range, 1], type = "l", col = "green")
  lines(segdata[range, 2], cvmatrix[range, 21], type = "l", col = "orange")
  lines(segdata[range, 2], segdata$mincv[range], type = "l", col = "black")
  lines(segdata[range, 2], segdata$segcv[range], type = "l", col = "blue", lwd = 2)
  abline(v = segdata$vline[range], col = "red")
  legend(legend_location,
         legend = c("First interval", "Last interval", "Middle intervals",
                    "Best interval", "CV of segment", "Segment"),
         col = c("green", "orange", "gray", "black", "blue", "red"),
         text.width = 1, x.intersp = 0.5, y.intersp = 0.5,
         lty = 1, lwd = c(1,1,1,1,2,1), cex = 0.8, inset = 0.02, xpd = TRUE)

  par(mfrow = c(1, 1))
}



















