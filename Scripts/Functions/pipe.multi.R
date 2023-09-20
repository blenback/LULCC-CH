### =========================================================================
### define multi function
### =========================================================================
#' Define settings for function that should be supplied to wsl.flex
#'
#' Create a multi.input object that efficiently stores model specifications
#'
#' @param mod a character with the name of the function to be called. E.g. "gam"
#' @param args a list with arguments to be passed to the function specified in mod
#' @param tag character with name for model set-up
#' @return Object of class 'multi'
#' @author Philipp Brun (main) with edits by Antoine Adde
#' @export

pipe.multi=function(mod,args,tag="",step=FALSE,weight=FALSE){

  out=pipe.multi.input()
  out@tag=tag
  out@args=args
  out@mod=mod
  out@weight=weight

  return(out)
}
