### =========================================================================
### define multi.input class
### =========================================================================
#' An S4 class to store evaluation data
#'
#' @slot mod Name of model algorithm to be called (character) e.g., 'glm'
#' @slot args A list of arguments to be supplied to the model fitting alrogrithm
#' @slot tag The name of the model setup (character)
#' @slot weight Should observations be weighted based on their prevalence?
#' @author Philipp Brun (main) with edits by Antoine Adde
#' @export
pipe.multi.input<-setClass("multi.input",slots=c(mod="character", # Model function
                                            args="list", # Model function arguments
                                            tag="character", # Model set-up name
                                            weight="logical")) # Should observations be weighted?

