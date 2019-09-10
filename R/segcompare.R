#' Segments comparison of different homogeneous segmentations methods.
#'
#' @usage segcompare(start = "SLK.start", end = "SLK.end", var = "deflection",
#'                   data, segid.matrix, methods = NULL)
#' \method{print}{segcompare}(x, ...)
#' \method{plot}{segcompare}(x, ...)
#'
#' @aliases segcompare print.segcompare plot.segcompare
#'
#' @param start A character of start location name of a spatial line.
#' @param end A character of end location name of a spatial line.
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param data A list of segmentation result.
#' @param segid.matrix A matrix of segmentations.
#' @param methods A vector of segmentation method names, default NULL.
#' @param x A list of segments comparison result.
#' @param ... Ignore
#'
#' @importFrom data.table .SD ":=" .N
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_step geom_vline geom_hline
#'             scale_x_continuous scale_y_continuous theme theme_bw element_blank
#' @importFrom stats dist
#'
#' @examples
#' testdata <- tsdwa[1:300, ]
#' testdata$length <- testdata$SLK.end - testdata$SLK.start
#' variable <- c("Curvature", "Deflection", "BLI")
#' \donttest{
#' seg1 <- hs(start = "SLK.start", end = "SLK.end", var = variable,
#'            testdata, method = "shs", range = c(0.1, 0.5)) # 0.3 s
#' seg2 <- hs(start = "SLK.start", end = "SLK.end", var = variable,
#'            testdata, method = "cda", range = c(0.1, 0.5)) # 0.7 s
#' seg3 <- hs(start = "SLK.start", end = "SLK.end", var = variable,
#'            testdata, method = "mcv", range = c(0.1, 0.5)) # 0.6 s
#' segid.matrix <- cbind(seg1$seg.id, seg2$seg.id, seg3$seg.id)
#' }
#' data(segid.matrix)
#' cp <- segcompare(start = "SLK.start", end = "SLK.end", var = variable,
#'                  testdata, segid.matrix, methods = c("SHS", "CDA", "MCV")) # 4.8 s
#' cp
#' plot(cp)
#'
#' @export

segcompare <- function(start = "SLK.start", end = "SLK.end", var = "deflection",
                       data, segid.matrix, methods = NULL){
  # segment id of different methods
  segid.matrix <- data.frame(segid.matrix)
  if (ncol(segid.matrix) < 2){
    stop("At least two segmentation ID vectors are required for comparison.")
  }
  n.method = ncol(segid.matrix)
  if (is.null(methods)){
    methods = paste("method", 1:n.method, sep = ".")
  }

  # no of var
  n.var <- length(var)

  # add length
  data <- as.data.table(data)
  data[, length := round(get(end) - get(start), digits = 10)] # remove system errors of small data

  ##===========================
  ## 1. Statistics

  # summary of segment data
  seg.stat <- list()
  for (i in 1:n.method){
    data$seg.id = segid.matrix[, i]
    seg.stat[[i]] <- segsummary(start = start, end = end, var = var,
                     seg.id = "seg.id", data)
  }

  # numbers of segments
  n.seg <- c()
  for (i in 1:n.method){
    n.seg[i] <- nrow(seg.stat[[i]]$seg.cv)
  }
  names(n.seg) <- methods

  ##===========================
  ## 2. Homogeneoutiy within segments

  # sum cv of observations within segments
  sumcv <- matrix(NA, n.method, n.var + 1)
  for (i in 1:n.method){
    sumcv[i, ] <- round(1/(seg.stat[[i]]$mean.cv * n.seg[i]), digits = 4) * nrow(data)/100
  }
  colnames(sumcv) <- names(seg.stat[[1]]$mean.cv)
  rownames(sumcv) <- methods

  # cv and pct of length
  cv <- function(x) sd(x)/mean(x)
  q75 <- function(x) quantile(x, 0.75)

  cv.length.list <- c()
  for (i in 1:n.method){
    cv.list <- list()
    for (j in 1:n.var){
      summary.data <- data[, .(start = get(start)[1], end = get(end)[.N],
                               length = sum(length),
                                    mean = mean(get(var[j])), median = median(get(var[j])),
                                    q75 = q75(get(var[j])), max = max(get(var[j])),
                                    cv = cv(get(var[j]))),
                                by = segid.matrix[, i]] # debug: use get
      summary.data[, variable := factor(var[j], levels = var)]
      cv.list[[j]] <- summary.data$cv
    }
    cv.byvar <- do.call(cbind, cv.list)

    mean.cv.byvar <- apply(cv.byvar, 1, mean)
    cv.length.list[[i]] <- data.frame(mean.cv.byvar, summary.data$length, "method" = methods[i])
  }

  maxcv.byvar <- max(do.call(rbind, cv.length.list)$mean.cv.byvar)
  compare.cv <- seq(0, maxcv.byvar, by = maxcv.byvar/99)
  pctlength <- function(cv.length, x){
    k <- which(cv.length$mean.cv.byvar <= compare.cv[x])
    pctl <- sum(cv.length$summary.data.length[k])/sum(cv.length$summary.data.length)
    return(pctl)
  }
  compare.pctlength <- matrix(NA, 100, ncol(segid.matrix))
  for (i in 1:n.method){
    cv.length <- cv.length.list[[i]]
    compare.pctlength[, i] <- sapply(1:100, function(x) pctlength(cv.length, x))
  }
  compare.pctlength <- data.frame("cv" = compare.cv, compare.pctlength)
  names(compare.pctlength)[-1] <- methods

  # pct of length
  pct2510 <- matrix(NA, n.method, 2)
  for (i in 1:n.method){
    k1 <- which(cv.length.list[[i]]$mean.cv.byvar <= 0.25)
    k2 <- which(cv.length.list[[i]]$mean.cv.byvar <= 0.10)
    pct1 <- sum(cv.length.list[[i]]$summary.data.length[k1])/
      sum(cv.length.list[[i]]$summary.data.length)
    pct2 <- sum(cv.length.list[[i]]$summary.data.length[k2])/
      sum(cv.length.list[[i]]$summary.data.length)
    pct2510[i, ] <- c(pct1, pct2)
  }
  colnames(pct2510) <- c("cv <= 0.25", "cv <= 0.10")
  rownames(pct2510) <- methods

  ##===========================
  ## 3. Heterogeneity among segments

  # q value
  qa <- function(x) (length(x) - 1) * sd(x)^2
  qvalue.matrix <- matrix(NA, n.method, n.var)
  for (i in 1:n.method){
    for (j in 1:n.var){
      varj <- var[j]
      strata <- split(data[[varj]], segid.matrix[, i])
      qvalue <- 1 - sum(sapply(1:length(strata), function(x) qa(strata[[x]])))/qa(data[[varj]])
      qvalue.matrix[i, j] <- round(qvalue, digits = 4)
    }
  }
  qvalue.matrix <- cbind(qvalue.matrix, round(rowMeans(qvalue.matrix), digits = 4))
  colnames(qvalue.matrix) <- c(var, "All")
  rownames(qvalue.matrix) <- methods

  ##===========================
  ## 4. Morphology of segments

  # length balance
  skew <- function(x){
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    g <- sqrt(n*(n - 1))/(n - 2) * sum((x - m)^3/n)/s^3
    return(g)
  }
  length.skew <- c()
  for (i in 1:n.method){
    length.skew[i] <- round(skew(seg.stat[[i]]$seg.cv$length), digits = 4)
  }
  names(length.skew) <- methods

  # mean length location indicator
  mll <- function(x){
    m <- (mean(x) - min(x))/(max(x) - min(x)) * 2 - 1
    return(m)
  }
  theta <- c()
  for (i in 1:n.method){
    theta[i] <- round(mll(seg.stat[[i]]$seg.cv$length), digits = 4)
  }
  names(theta) <- methods

  lskew <- rbind(length.skew, theta)
  rownames(lskew) <- c("Normal distribution", "Uniform distribution")

  ##===========================
  ## return results

  result <- list("nseg" = n.seg, # statistics
                 "sumcv" = sumcv, "cv.length" = cv.length.list, # homogeneity
                 "cv.pctlength" = compare.pctlength, "cv.pct" = pct2510,
                 "qseg" = qvalue.matrix, # heterogeneity
                 "lengthskew" = lskew) # Morphology
  class(result) <- "segcompare"
  return(result)
}

print.segcompare <- function(x, ...){
  message("Segment numbers:\n", paste0(capture.output(x$nseg), collapse = "\n"))
  message("-------------------")
  message("Homogeneity of data within segments (reciprocal of the sum of segment-based CV):\n",
          paste0(capture.output(x$sumcv), collapse = "\n"))
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  cv.pct <- x$cv.pct; cv.pct2 <- cv.pct
  for (i in 1:length(cv.pct)){
    cv.pct2[i] <- percent(cv.pct[i])
  }
  message("Percentage of segment lengths with CV lower than 0.25 and 0.10:\n",
          paste0(capture.output(cv.pct2), collapse = "\n"))
  message("-------------------")
  message("Spatial heterogeneity (Q values) of segment-based data:\n",
          paste0(capture.output(x$qseg), collapse = "\n"))
  message("-------------------")
  message("Skew of segment length distribution:\n",
          paste0(capture.output(x$lengthskew), collapse = "\n"))
  invisible(x)
}

plot.segcompare <- function(x, ...){
  # compare segment-based cv
  cv.length <- x$cv.length
  for (i in 1:length(x$nseg)){
    cumlength <- cumsum(cv.length[[i]]$summary.data.length)
    cv.length[[i]]$summary.data.length <- c(0, cumlength[-length(cumlength)])
  }
  cv.length <- do.call(rbind, cv.length)
  message("CV values comparison for segment data ...\n")
  p1 <- ggplot(cv.length, aes(x = summary.data.length, y = mean.cv.byvar, color = method)) +
    geom_step(aes(linetype = method)) +
    scale_x_continuous("location") +
    scale_y_continuous("CV of segment data") +
    geom_hline(yintercept = c(0.1, 0.25), linetype = 2) +
    theme_bw() +
    theme(panel.grid = element_blank())
  print(p1)

  # mean cv and length
  compare.pctlength <- melt(x$cv.pctlength, id = "cv")
  names(compare.pctlength)[2] <- "method"
  message("Relationship between mean CV and segment length ...\n")
  p2 <- ggplot(compare.pctlength, aes(x = cv, y = value, color = method)) +
    geom_line() +
    geom_vline(xintercept = c(0.1, 0.25), linetype = 2) +
    scale_x_continuous("CV of segment data") +
    scale_y_continuous("Percentage of length") +
    theme_bw() +
    theme(panel.grid = element_blank())
  print(p2)

  # homogeneity and heterogeneity
  ho <- data.frame(x$sumcv); he <- data.frame(x$qseg)
  ho$method <- rownames(ho); he$method <- rownames(he)
  ho <- melt(ho, id = c("method")); he <- melt(he, id = c("method"))
  hh <- cbind(ho, he$value)
  hh$method <- factor(hh$method, levels = names(x$nseg))
  names(hh)[3:4] <- c("homogeneity", "heterogeneity")
  message("Homogeneity and heterogeneity comparison of segmentations ...\n")
  p3 <- ggplot(hh, aes(x = homogeneity, y = heterogeneity, color = method)) +
    geom_point(aes(shape = variable)) +
    scale_x_continuous("homogeneity within segments") +
    scale_y_continuous("heterogeneity among segments") +
    theme_bw() +
    theme(panel.grid = element_blank())
  print(p3)
}
