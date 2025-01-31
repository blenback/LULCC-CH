### =========================================================================
### Set wsl.fit Class
### =========================================================================
#' An S4 class to store fitted objects
#'
#' @slot meta A list with meta information
#' @slot tesdat A list with held out data to be used for testing
#' @slot performance fitted model objects
#' @slot call the function call
#'
wsl.fit<-setClass("wsl.fit",slots=c(meta="list", # Meta information
                                    tesdat="list", # Test data subset
                                    train ="list", #Training data subset
                                    fits="list", # Model objects
                                    call="call")) # conserve function call
