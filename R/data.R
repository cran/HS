#' @title Road deflection dataset.
#'
#' @description The "deflection" dataset is a sample of the road deflection data
#'              monitored and collected by Main Roads Western Australia.
#'
#' @name deflection
#' @format \code{deflection}: A data frame with 1000 rows and 4 variables.
#'
#' \itemize{
#'   \item id. Number of observation.
#'   \item SLK.start. Spatial start location of data. SLK is short for the straight line kilometer.
#'   \item SLK.end. Spatial end location of data.
#'   \item Deflection. The monitored road deflection value.
#' }
#'
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset deflection
"deflection"

#' @title Traffic speed deflectometer (TSD) data of pavement deteriorations.
#'
#' @description The "tsdwa" dataset is a sample of the pavement deterioration data
#'              monitored and collected by Main Roads Western Australia.
#'
#' @name tsdwa
#' @format \code{tsdwa}: A data frame with 5000 rows and 8 variables.
#'
#' \itemize{
#'   \item id. Number of observation.
#'   \item SLK.start. Spatial start location of data. SLK is short for the straight line kilometer.
#'   \item SLK.end. Spatial end location of data.
#'   \item SurfType. Surfacing type.
#'   \item PvtType. Pavement type.
#'   \item Curvature.
#'   \item Deflection.
#'   \item BLI. Base layer index.
#' }
#'
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset TSD
"tsdwa"

#' @title A matrix of segmentations with different methods for data "tsdwa".
#'
#' @description Segmentation results of CDA, MCV and SHS methods for data "tsdwa".
#'
#' @name segid.matrix
#' @format \code{segid.matrix}: A matrix with 300 rows and 3 columes,
#'         representing segmentations of three methods, CDA, MCV and SHS.
#'
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset segmentations
"segid.matrix"

