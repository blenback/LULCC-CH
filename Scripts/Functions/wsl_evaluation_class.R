### =========================================================================
### Set Class
### =========================================================================
#' An S4 class to store evaluation data
#'
#' @slot meta A list with meta information
#' @slot thres A vector with externally supplied thresholds
#' @slot performance A list with model performance estimates
#' @author Philipp
#' @export
wsl.evaluation<-setClass("wsl.evaluation",slots=c(meta="list", # Meta information
                                                  thres="numeric", # supply external threshold
                                                  performance="list")) # conserve function call
