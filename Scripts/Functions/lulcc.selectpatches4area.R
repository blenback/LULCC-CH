#############################################################################
## lulcc.selectpatches4area: function to generate set of possible solutions
## of patches to approximately meet an areal target
## Date: 03-01-2024
## Author: Ben Black
#############################################################################
#'
#' @param xy dataframe of patches with area and patch_id
#' @param sfind Int, target area to be found
#' @param nmax Int, maximum number of solutions to be found
#' @param tmax Int, maximum time allowed for calculation
#'
#' @author Ben Black
#' @export
#'

lulcc.selectpatches4area <- function(xy, sfind, nmax, tmax = 100000000000000000000000000) {

  # calculate 2.5% of sfind to use as an upper and lower bound
  sfind_bound <- sfind * 0.025

  #calculate upper and lower bounds for sfind
  sfind_upper <- sfind + sfind_bound
  sfind_lower = sfind - sfind_bound

  #sort xy according to target variable
  xy <- xy[order(xy$num_cells, decreasing = TRUE),]

  #reset row.names
  row.names(xy) <- 1:nrow(xy)

  #seperate patch areas
  x = xy[, "num_cells"]


  #stop if the sum of all patches does not exceed the desired area
  if (sum(x) < sfind) stop("Impossible solution! sum(x)<sfind!")

  # #helper function to calculate difference from start time
  # fTimeSec <- function() as.numeric(Sys.time()-l$tstart, units="secs")

  #Create a vector the same length as the num of patches to start the loop,
  #first entry TRUE all the subsequent entries FALSE
  #updated iteratively in loop
  sel = c(TRUE, rep(FALSE, length(x) - 1))

  #List of intermediate states of the vector sel
  lsel = list()

  #List with a collection of parameters and results
  l = list(
    patch_ids = list(),
    x = x,
    tstart = Sys.time(),
    xfind = list(),
    stop = FALSE,
    reason = "")

  while (TRUE) {

    # #Maximum Runtime Test
    # if(fTimeSec()>tmax) {
    #   l$reason = "Calculation time is greater than tmax.\n"
    #   l$stop = TRUE
    #   break
    # }

    #Record the solution and test the number of solutions
    if (sum(l$x[sel]) >= sfind_lower & sum(l$x[sel]) <= sfind_upper) {

      #Save solution
      l$xfind[[length(l$xfind) + 1]] = l$x[sel]
      l$patch_ids[[length(l$patch_ids) + 1]] = xy[sel, "patch_id"]

      #Test the number of solutions
      if (length(l$patch_ids) == nmax) {
        l$reason = "Already found nmax solutions.\n"
        l$stop = TRUE
        break
      }
    }

    idx = which(sel)
    if (idx[length(idx)] == length(sel)) {
      if (length(lsel) == 0) break
      sel = lsel[[length(lsel)]]
      idx = which(sel)
      lsel[length(lsel)] = NULL
      sel[idx[length(idx)]] = FALSE
      sel[idx[length(idx)] + 1] = TRUE
      next
    }

    if (sum(l$x[sel]) >= sfind) {
      sel[idx[length(idx)]] = FALSE
      sel[idx[length(idx)] + 1] = TRUE
      next
    } else {
      lsel[[length(lsel) + 1]] = sel  #Save the current state of sel vector
      sel[idx[length(idx)] + 1] = TRUE
      next
    }
  }

  if (length(l$patch_ids) == 0 & !l$stop) stop("No solutions!")

  #vector summary of result
  l$reason = paste(l$reason, "Found", length(l$patch_ids), "\n")

  #print summary of combinatorial step
  cat(l$reason)

  #return results object
  return(l)
}

# ORIGINAL VERSION OF FUNCTION INCLUDING TIME-OUT

#solver function from: https://stackoverflow.com/questions/69608840/selecting-such-vector-elements-so-that-the-sum-of-elements-is-exactly-equal-to-t
#for identifying combination of patches whose sum total area
#exceeds a specifcied amount
# findSumm <- function(xy, sfind, nmax=10000, tmax=100000000000000000000000000){
#
# #sort xy according to target variable
# xy <- xy[order(xy$num_cells, decreasing = TRUE),]
#
# #seperate patch areas
# x = xy[, "num_cells"]
#
# #stop if the sum of all patches does not exceed the desired area
# if(sum(x)<sfind) stop("Impossible solution! sum(x)<sfind!")
#
# #helper function to calculate difference from start time
# fTimeSec <- function() as.numeric(Sys.time()-l$tstart, units="secs")
#
# #Create a vector the same length as the num of patches to start the loop,
# #first entry TRUE all the subsequent entries FALSE
# #updated iteratively in loop
# sel = c(TRUE, rep(FALSE, length(x)-1))
#
# #List of intermediate states of the vector sel
# lsel = list()
#
# #List with a collection of parameters and results
# l = list(
#   patch_ids = list(),
#   x = x,
#   tstart = Sys.time(),
#   chosen = list(),
#   xfind = list(),
#   time = c(),
#   stop = FALSE,
#   reason = "")
#
# while(TRUE) {
#   #Maximum Runtime Test
#   if(fTimeSec()>tmax) {
#     l$reason = "Calculation time is greater than tmax.\n"
#     l$stop = TRUE
#     break
#   }
#
#   #Record the solution and test the number of solutions
#   if(sum(l$x[sel])==sfind){
#     #Save solution
#     l$chosen[[length(l$chosen)+1]] = sel
#     l$xfind[[length(l$xfind)+1]] = l$x[sel]
#     l$patch_ids[[length(l$patch_ids)+1]] = xy[sel, "patch_id"]
#     l$time = c(l$time, fTimeSec())
#
#     #Test the number of solutions
#     if(length(l$chosen)==nmax){
#       l$reason = "Already found nmax solutions.\n"
#       l$stop = TRUE
#       break
#     }
#   }
#
#   idx = which(sel)
#   if(idx[length(idx)]==length(sel)) {
#     if(length(lsel)==0) break
#     sel=lsel[[length(lsel)]]
#     idx = which(sel)
#     lsel[length(lsel)]=NULL
#     sel[idx[length(idx)]]=FALSE
#     sel[idx[length(idx)]+1]=TRUE
#     next
#   }
#
#   if(sum(l$x[sel])>=sfind){
#     sel[idx[length(idx)]]=FALSE
#     sel[idx[length(idx)]+1]=TRUE
#     next
#   } else {
#     lsel[[length(lsel)+1]] = sel  #Save the current state of sel vector
#     sel[idx[length(idx)]+1]=TRUE
#     next
#   }
# }
#
# if(length(l$chosen)==0 & !l$stop) stop("No solutions!")
#
# #vector summary of result
# l$reason = paste(l$reason, "Found", length(l$chosen),
#                  "solutions in time", signif(fTimeSec(), 3), "seconds.\n")
#
# #print summary of combinatorial step
# cat(l$reason)
#
# #return results object
# return(l)
# }
  
  