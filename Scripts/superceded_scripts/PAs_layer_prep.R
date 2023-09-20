library(doFuture)


PA_int_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Int_data"

Patches_LSM <- rast(paste0(PA_int_dir, "/Patches_LSM.tif"))

#small function calculating stats on patches
Patch_stats <- function(Patch_raster, Val_raster){

  #calculate the area of all patches
  area_df <- as.data.frame(freq(Patch_raster))

  #subset cols and rename
  area_df <- area_df[c('value','count')]
  colnames(area_df) <- c("patch_id", "num_cells")

  # stack with the prioritization map
  r_stack <- c(Patch_raster, Val_raster)

  # Use terra's zonal function to compute the median for each unique value in Patch_rast
  zonal_median <- terra::zonal(r_stack, Patch_raster, fun = function(x) { median(x, na.rm = TRUE) })
  zonal_median <- zonal_median[c(2,3)]

  # Rename the columns of zonal_median
  colnames(zonal_median) <- c("patch_id", "median")

  #merge dfs with median of prio and area of each patch
  df_merge <- merge(area_df,zonal_median,by="patch_id")
}

#calculate stats for each patch generation approach
LSM_patch_stats <- Patch_stats(Patch_raster = Patches_LSM, Val_raster = Biodiv_prio_wo_pa)

#remove patches that are 2 cells or less
LSM_patch_stats <- LSM_patch_stats[LSM_patch_stats$num_cells > 2,]
length(unique(LSM_patch_stats$num_cells))

n_cells <- c("EI_SOC", "EI_CUL", "EI_NAT")
names(n_cells) <- c("EI_SOC", "EI_CUL", "EI_NAT")

for(i in 1:length(Scenario_names)){}
i = 1

#Load in solutions
Solutions <- readRDS(paste0(PA_int_dir, "/Solutions_10k_", names(n_cells)[i], ".rds"))

#rank solutions according greatest value of the sum of median patch priority * patch size
#plan(future.callr::callr, workers=availableCores(omit = 2))
plan(multisession)
size = 5000*1024^2 
options(future.globals.maxSize= size)

#loop over transitions (currently takes 4 mins in parallel)
Solution_scores <- rbindlist(future.apply::future_lapply(1:length(Solutions$chosen),
                                       function(x){
                                         
  #seperate vector of whether patches were chosen (by ID)  
  idx = Solutions$chosen[[x]]
  
  #calculate sum of median patch priority * patch size for solution
  return(list("Solution_num" = x,
  "Patch_prio_sum" = sum(LSM_patch_stats$median[idx] * LSM_patch_stats$num_cells[idx])))
  }))
       
plan(sequential)                                  

#sort results
Ranked_solutions <- Solution_scores[order(Solution_scores$Patch_prio_sum, decreasing = TRUE),]
Best_solution_ID <- Ranked_solutions[[1,"Solution_num"]]

# get best patches via id
Patch_index <- Solutions$chosen[[Best_solution_ID]]
Best_patch_stats <- LSM_patch_stats[Patch_index,]
Best_patch_ids <- LSM_patch_stats[Patch_index,"patch_id"]

Patch_areas <- sum(Solutions$xfind[[Best_solution_ID]])


sum(Best_patch_stats$num_cells)

new_pa <-  ifel(Patches_terra %in% result_patches, Patches_terra, NaN)
writeRaster(new_pa,file= "Data/Spat_prob_perturb_layers/Protected_areas/New_pas.tif", overwrite=TRUE) 

test <- LSM_patch_stats[Solutions[["chosen"]][[23]],]
sum(test$num_cells)

