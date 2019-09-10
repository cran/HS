#' Visualization of homogeneous segments.
#'
#' @usage segplot(start = "SLK.start", var = "deflection",
#'                seg.id = "seg.id", data, plot.range = NULL)
#'
#' @param start A character of start location name of a spatial line.
#' @param var A character or a character vector of variable names,
#'            such as a road pavement performance indicator.
#' @param seg.id A character of the name of new segment number.
#' @param data A data frame of a dataset.
#' @param plot.range A vector of plot range.
#'
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2 scale_color_manual
#' @importFrom data.table rbindlist
#'
#' @examples
#' testdata <- tsdwa[1:300,]
#' testdata$length <- testdata$SLK.end - testdata$SLK.start
#' testdata <- shs(var = c("Curvature", "Deflection"), length = "length",
#'                 testdata, range = c(0.1, 0.5))
#' segplot(start = "SLK.start", var = c("Curvature", "Deflection"),
#'               seg.id = "seg.id", testdata, plot.range = 1:300)
#'
#' @export

segplot <- function(start = "SLK.start", var = "deflection",
                    seg.id = "seg.id", data, plot.range = NULL){
  data <- as.data.table(data)
  # segment point
  data$seg.point <- 0
  k <- which(data$seg.id[1:(nrow(data) - 1)] != data$seg.id[2:nrow(data)])
  data$seg.point[c(1, k + 1)] <- 1

  # summary

  cv <- function(x) sd(x)/mean(x)
  q75 <- function(x) quantile(x, 0.75)

  summary.data <- list()
  for (i in 1:length(var)){
  setkey(data, seg.id)
  summary.data[[i]] <- data[, .(location = get(start)[1],
                                mean = mean(get(var[i])), cv = cv(get(var[i])),
                                median = median(get(var[i])), q75 = q75(get(var[i]))),
                            by = .(seg.id)] # debug: use get
  summary.data[[i]][, var := factor(var[i], levels = var)]
  }
  summary.data <- rbindlist(summary.data)

  # plot.range
  if (is.null(plot.range)){
    plot.range <- 1:nrow(data)
  }

  # data1: data for observation plot
  subset.data <- data[plot.range, ]
  subset.col <- c(start, var)
  data1 <- subset.data[, ..subset.col]
  data1 <- melt(data1, id = start)
  names(data1)[1] <- c("location")
  # data2: data for segment statistics plot
  k <- which(summary.data$location >= min(subset.data[, get(start)]) &
               summary.data$location <= max(subset.data[, get(start)]))
  data2 <- summary.data[k, c(2,3,5:7)]
  data2 <- melt(data2, id = c("location", "var"))
  names(data2)[2:3] <- c("variable", "statistics")
  # data3: cv data plot
  data3 <- summary.data[k, c(2,4,7)]
  names(data3)[3] <- "variable"
  meancv <- data3[, .(cv = mean(cv)), by = .(location)]
  meancv <- as.data.table(cbind(unique(summary.data[k, 2]), cv = meancv[, cv], variable = "meancv"))
  data3 <- rbindlist(list(data3, meancv))

  # verticle lines of segments
  vline.values <- subset.data[which(subset.data$seg.point == 1), get(start)]
  # x limits
  limits.x <- subset.data[c(plot.range[1], plot.range[length(plot.range)]), get(start)]
  # plot
  ggplot.colors <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols <- ggplot.colors(length(var))
  p1 <- ggplot(data1, aes(x = location, y = value, group = variable, color = variable)) +
    geom_line() +
    scale_x_continuous("", limits = limits.x) +
    scale_y_continuous("observation") +
    geom_vline(xintercept = vline.values, color = "gray") +
    theme_bw() +
    theme(panel.grid = element_blank())
  p2 <- ggplot(data2, aes(x = location, y = value, color = variable, linetype = statistics)) +
    geom_step() +
    scale_x_continuous("", limits = limits.x) +
    scale_y_continuous("statistics") +
    geom_vline(xintercept = vline.values, color = "gray") +
    theme_bw() +
    theme(panel.grid = element_blank())
  p3 <- ggplot(data3, aes(x = location, y = cv, color = variable)) +
    geom_step() +
    scale_color_manual("variable", values = c(cols, "black")) +
    scale_x_continuous(limits = limits.x) +
    scale_y_continuous("CV") +
    geom_vline(xintercept = vline.values, color = "gray") +
    theme_bw() +
    theme(panel.grid = element_blank())
  ggarrange(p1, p2, p3, legend = "right",
            ncol = 1, nrow = 3, align = "v")

}

