### =========================================================================
### lulcc.neighbourhoodlayercreation:
### ===========================================================================
#' 
#'
#' @author Ben Black (lulcc function), Majid Shadman Roodposhti (matrix functions)
#' @export

### =========================================================================
### A- Instantiate small functions for neighbourhood creation
### =========================================================================

#function to set up a random Pythagorian Matrix generating the central values 
#of each matrix x0,0 and their corresponding decay rates 
#decay rates are positive, pseudorandom number from the uniform distribution 
#in the range 0>??>1. For a randomised search, ??  may be a pseudorandom number 
#from the uniform distribution, while for a grid search ??  may be any number 
#from a user-defined set
#function to plot all generated random Pythagorian matrices for checking, 
#with values of each cell labelled in the center
randomPythagorianMatrix <- function(n, x, interpolation="smooth", search="random") {
  
  choices <- c("random", "grid")
  search  <- choices[pmatch(search, choices, duplicates.ok=FALSE)]
  if(search=="random") {
    seed   <- runif(n, 5, 50)
    drops  <- mapply(runif, 1, 1, seed)
  } else {
    seed   <- rep(seq.int(5,n,5), each=5)
    drops  <- rep(c(3,6,12,24,48), times=n/5)
  }
  Map(getPythagorianMatrix, x, seed, drops, interpolation)
}

#function that shapes the actual random Pythagorian Matrix using
#smooth or linear interpolation
getPythagorianMatrix <- function(x, mid, drop, interpolation="smooth") {
  if(x %% 2 == 0 | x < 0) stop("x must be an odd positive number")
  
  choices <- c("smooth", "linear")
  interpolation <- choices[pmatch(interpolation, choices, duplicates.ok=FALSE)]
  
  dists <- outer(abs(1:x - ceiling(x/2)), abs(1:x - ceiling(x/2)), function(x,y) sqrt(x^2+y^2))
  if(interpolation=="smooth") {
    mat <- (1/drop) ^ dists * mid
  } else {
    mat <- matrix(approx(x=0:x, y=0.1^(0:x)*mid, xout=dists)$y, ncol=x, nrow=x)
  }
  return(mat)
}



plotPythagorianMatrix <- function(mat) {
  colors <- colorRampPalette(c("deepskyblue4", "deepskyblue3", "darkslateblue", "deepskyblue1", "lightblue1", "gray88"))(256)
  corrplot::corrplot(mat, is.corr=FALSE, method="shade",
                     col=colors, tl.pos="n",
                     number.cex = .7, addCoef.col = "black")
  text(row(mat), col(mat), round(mat, 2), cex=0.7)
}

#function to plot the outcomes of applying different decay rates on every generated 
#central cell

plotPythagorianMatrixDecay <- function(mat, plot=TRUE, ...) {
  mid  <- ceiling(ncol(mat)/2)
  drop <- mat[mid, mid+1] / mat[mid, mid]
  xs <- seq(0, mid, 0.1)
  if(isTRUE(all.equal(mat[mid+1,mid+1], drop^sqrt(2) * mat[mid,mid]))) {
    ys <- drop^(xs)*mat[mid,mid]
    if(plot) {
plot(xs, ys, type="l", las=1, lwd=2, xlab="distance", ylab="value",
           cex.axis=0.7, mgp=c(2,0.5,0), tck=-0.01, ...
      )
    }
  } else {
    ys <- approx(0:mid, drop^(0:mid), xout=xs)$y * mat[mid,mid]
    if(plot) {
plot(xs, ys, type="l", las=1, lwd=2, xlab="distance", ylab="value",
           cex.axis=0.7, mgp=c(2,0.5,0), tck=-0.01, ...
      )
    }

  }
  return(data.frame(x=xs, y=ys))
  
}


### =========================================================================
### B- Instantiate nested loop functions to create focal LULC rasters from matrix
### =========================================================================

lulcc.generatenhoodrasters <- function(LULC_raster, Neighbourhood_matrices, Active_LULC_class_names, Data_period, Nhood_folder_path){

  
#get pixel values of all active LULC classes
Active_class_values <- sapply(Active_LULC_class_names, function(x){
   class_value <- LULC_raster@data@attributes[[1]][LULC_raster@data@attributes[[1]]$lulc_name == x, "Pixel_value"]
   return(class_value)
  })

#subset LULC raster by all Active_class_values
Active_class_raster_subsets <- lapply(Active_class_values, function(subset_value){
  subset_raster <- LULC_raster == subset_value})

#outer loop over the active class LULC rasters
All_focal_rasters <- mapply(function(active_class_raster, active_class_raster_name){
  
 Matrices_for_lulc_class <- mapply(function(single_matrix, matrix_name){ #Inner loop: running the focal function using each matrix in the list
  Focal_layer <- focal(x=active_class_raster, w= single_matrix, na.rm=FALSE, pad=TRUE, padValue=0, NAonly=FALSE) #create focal layer using matrix
  Focal_file_name <- paste(active_class_raster_name, "nhood", matrix_name, Data_period, sep = "_") #create file path for saving this layer
  Focal_full_path <- paste0(Nhood_folder_path, "/", Focal_file_name, ".grd") #create full folder path
  writeRaster(Focal_layer, Focal_full_path ,datatype='INT2U', overwrite=TRUE) #save layer
  names(Focal_layer) <- Focal_file_name #rename focal layer
  return(Focal_layer) #return layer for inspection
  }, single_matrix = Neighbourhood_matrices, matrix_name = names(Neighbourhood_matrices), SIMPLIFY = FALSE) #close inner loop
return(Matrices_for_lulc_class) 
}, #close outer loop
active_class_raster = Active_class_raster_subsets,
active_class_raster_name = names(Active_class_raster_subsets), SIMPLIFY = FALSE)

return(All_focal_rasters)
}

### =========================================================================
### C- Instantiate function to create focal LULC rasters from a list of details
###    to be used as part of dynamic updating of focals during simulation
### =========================================================================

lulcc.producefocalsbylist <- function(Focal_specifications, List_of_matrices, LULC_raster, Simulation_time_step, simulation_ID){
      
  #create a folder path using simulation ID and time step 
  Dynamic_focal_folder_path <- paste0("Data/Preds/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)
    
  #create directory
  dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)
  
  for (i in 1:nrow(Focal_specifications)){
    
      lulcc.generatenhoodrasters(LULC_raster = LULC_raster,
                             Neighbourhood_matrices = List_of_matrices[Focal_specifications[i,]$matrix_id],
                             Active_LULC_class_names = Focal_specifications[i,]$active_lulc,
                             Data_period = "", #This is a blank as we need generic layer names for the existing fitted models
                             Nhood_folder_path = paste0(Dynamic_focal_folder_path, "/"))
  }
}
