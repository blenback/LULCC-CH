#working directory is the base folder where the R.project file is located
#open the project to set wd automatically

library(terra)
library(dplyr)
library("devtools")

# package to interface with the Swiss Geoadmin API
devtools::install_github("zumbov2/swissgd")

#-------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------

ProjCH <- "+proj=somerc +init=epsg:2056"
Ref_grid_path <- ("Data/Spat_prob_perturb_layers/Protected_areas/Ref_grid.grd")
Ref_grid <- rast(Ref_grid_path)

#load PAs shapefile
PA <- vect("Data/Spat_prob_perturb_layers/Protected_areas/SwissPA.shp")

#Non_PA_BPAs <- vect("Data/Spat_prob_perturb_layers/Protected_areas/Non_PA_BPA.shp")
LC <- vect("Data/Spat_prob_perturb_layers/Protected_areas/swissTLMRegio_LandCover.shp")
names(LC)
settlement <- subset(LC, LC$OBJVAL == "Siedl")
road <- vect("Data/Spat_prob_perturb_layers/Protected_areas/swissTLMRegio_Road.shp")
road <- subset(road, road$CONSTRUCT == 'Keine Kunstbaute')
railway <- vect("Data/Spat_prob_perturb_layers/Protected_areas/swissTLMRegio_Railway.shp")
railway <- subset(road, road$CONSTRUCT == 'Keine Kunstbaute')

#load biodiversity prioritization map
Biodiv_grid_path <- paste0("Data/Spat_prob_perturb_layers/Protected_areas/prioritization_no_PAs.tif")
Biodiv_prio <- rast(Biodiv_grid_path)
crs(Biodiv_prio) <- ProjCH

#Get the PA-categories and subset the shp file accordingly
subset_rows <- PA$Res_Type %in% c("Ramsar","Swiss National Park","Unesco_BiosphereReserve","Unesco_CulturalSites","Unesco_NaturalSites","ProNatura reserves", "Emeraude")
PA_BAFU <- PA[subset_rows, ]
PA_BAFU <- terra::project(PA_BAFU, ProjCH)
PA_BAFU_df <- as.data.frame(PA_BAFU)
writeVector(PA_BAFU, "Data/Spat_prob_perturb_layers/Protected_areas/PA_BAFU.shp", overwrite=TRUE)

#check for invalid polygons (i.e. holes)
polys_invalid <- any(is.valid(PA_BAFU, messages=FALSE, as.points=FALSE)== FALSE)

#if invalid polygons then makeValid
if(polys_invalid == TRUE){
PA_BAFU <- makeValid(PA_BAFU)
}

#old approach creates slight in that we lose cells in the raster
#both the biodiv_prio raster and the Ref_grid have 8280000 cells but if we
#rasterize in this way then the PA_BAFU_raster has 7542860 
# Ref_grid_alt <- rast("Data/Spat_prob_perturb_layers/Protected_areas/prioritization_no_PAs.tif")
# x <- crop(Ref_grid_alt, ext(PA_BAFU))
# PA_BAFU_raster <- mask(x, PA_BAFU)
# global(PA_BAFU_raster, fun="notNA")
# global(PA_BAFU_raster, fun="isNA")
# ncell(PA_BAFU_raster)
# ext(PA_BAFU_raster)

#Alt approach using terra::rasterize on the original Ref_grid
#somehow results in less PA cells
# PA_BAFU_rasterize <- rasterize(PA_BAFU, Ref_grid)
# global(PA_BAFU_rasterize, fun="notNA")
# global(PA_BAFU_rasterize, fun="isNA")
# ncell(PA_BAFU_rasterize)
# ext(PA_BAFU_rasterize)

#Preferred approach with masking which results in same num of PA cells
#as the original approach but keeps the ncells and extent consistent
PA_BAFU_raster <- mask(Biodiv_prio, PA_BAFU) 
# global(PA_BAFU_raster, fun="notNA")
# global(PA_BAFU_raster, fun="isNA")
# ncell(PA_BAFU_raster)
# plot(PA_BAFU_raster)
# ext(PA_BAFU_raster)

#change non-NA values to 1
PA_BAFU_raster <- ifel(!is.na(PA_BAFU_raster), 1, 0)
writeRaster(PA_BAFU_raster,"Data/Spat_prob_perturb_layers/Protected_areas/PA_BAFU_raster.tif", overwrite=TRUE)

#combine PA raster with raster of Biodiversity promotion areas
BPA_raster <- rast("Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPA_raster.tif")
BPA_raster <- ifel(!is.na(BPA_raster), 1, 0)
PA_total_rast <- PA_BAFU_raster+BPA_raster 

#addition results in 2's for overlap and 1 for non-overlapping BPAs
#convert all values greater than 0 to 1 and the rest back to NA
PA_total_rast <- ifel(PA_total_rast == 0, NA, 1)
#global(PA_total_rast, fun="notNA")
#global(PA_total_rast, fun="isNA")
#confirming that the correct number of PA cells have been changed to NA
#NA_confirm should match number of non NA cells in PA_total_raster
#NA_confirm = global(Biodiv_prio, fun="isNA")-global(Biodiv_prio_wo_pa, fun="isNA")
writeRaster(PA_total_rast, "Data/Spat_prob_perturb_layers/Protected_areas/PA_combined.tif", overwrite = TRUE)

#-------------------------------------------------------------------------
# Calculating PA areal coverage
#-------------------------------------------------------------------------

#if we use the rasterized layers of PAs and BPAs we will overestimate current
#coverage because many only occupy portions of 100m cells hence we should 
#calculate areas from the Spatvectors

#For the BPAs this is easy as there is an area attribute with values for all polygons
#but for the PA shapefile there is several incomplete area columns so we will
#need to estimate from the polygons however some are overlapping so first 
#we need to aggregate to non-overlapping areas only 

#1. calculate discrepancy between area of BPAs from polygons vs. BPA raster cells
#that do not overlap with other PAs
#set 0's back to NA in BPA and PA_BAFU rasters
BPA_raster <- ifel(BPA_raster == 0, NA, 1)
PA_BAFU_raster <- ifel(PA_BAFU_raster==0, NA, 1)

#Identify BPA cells not in PAs (inverse masking)
Non_PA_BPAs_rast <- mask(BPA_raster, PA_BAFU_raster, inverse = TRUE) 
writeRaster(Non_PA_BPAs_rast,"Data/Spat_prob_perturb_layers/Protected_areas/Non_PA_BPA_raster.tif", overwrite=TRUE)

#calculate area of BPA cells not in PAs
cell_area_BPA <- expanse(Non_PA_BPAs, unit="m")

#filter Spatvector of BPAs by first intersecting with the BAFU PAs then subsetting
#the Spatvector by the intersecting polygons it would be faster to use
#terra::crop but it is throwing an error: "TopologyException: Input geom 1 is invalid" 
#I have check geometry validity using terra::is.valid and apparently all are valid?
#note: intersecting splits polygons meaning that there are rows with non-unique IDs
intersecting_BPAs <- terra::intersect(BPAs, PA_BAFU)
Non_PA_BPAs <- BPAs[which(!BPAs$ID %in% unique(intersecting_BPAs$ID)), ]
writeVector(Non_PA_BPAs, "Data/Spat_prob_perturb_layers/Protected_areas/Non_PA_BPA.shp", overwrite=TRUE)

#sum up areas of remaining BPA polygons
poly_area_BPA <- sum(Non_PA_BPAs$flaeche_m2)

#cell area minus polygon area equates to the areal overestimation
BPA_area_overestimate <- cell_area_BPA-poly_area_BPA

#2. Calculate discrepancy between area of PA polygons minus the overlapping 
#areas and the raster cell total area

#combine overlapping polygons, terra::aggregate/union?
#produces an error of geom types not matching however
#geomtype(PA_BAFU) returns only one geom type?
#try st_union instead?
PA_union <- terra::union(PA_BAFU) 
PA_agg <- terra::aggregate(PA_BAFU, dissolve = TRUE)
writeVector(PA_agg, "Data/Spat_prob_perturb_layers/Protected_areas/PA_agg.shp", overwrite=TRUE)

#calc areas of remaining polygons
PA_poly_area <- expanse(PA_agg)

#calc raster cell area
PA_cell_area <- expanse(PA_BAFU_raster)

#calculate overestimation of raster
PA_area_overestimate <- PA_cell_area-PA_poly_area

#3. calculate total PA coverage

#Calculating the number of cells needed for protection goals
n_prot_cells <- unlist(global(PA_total_rast, fun="notNA")) #number of protected cells

#directly calculate area of protected cells and subtract the overestimation of BPAs
area_prot_raster <- expanse(PA_total_rast, unit="m")
area_prot_poly <- PA_poly_area+poly_area_BPA

#area_prot_adj <- (expanse(PA_total_rast, unit="m"))-BPA_area_overestimate

#rather than a figure for actual CH area perhaps we should use the approximate
#area of Switzerland covered by our Biodiv_prio raster?
area_ch_raw = 41285*1000000 
area_ch_raster <- expanse(Biodiv_prio, unit="m")

#percent of total area that should be protected (adapt to a vector for scenarios later)
perc_goal <- 0.30

#current coverage estimate from polygons vs. raster under the raster CH area
cover_raster <- area_prot_raster/area_ch_raster #26.5%
cover_poly <- area_prot_poly/area_ch_raster #16.9%
#polygon area is closer to BAFU figure of 12%

#under the raw CH area
cover_raster_raw <- area_prot_raster/area_ch_raw #25.8%
cover_poly_raw <- area_prot_poly/area_ch_raw #16.5%

#additional % area of switzerland required to meet goal
perc_todo <- perc_goal-(area_prot_poly/area_ch)
 
#number of cells required to meet goal
n_cells = ceiling(perc_todo*area_ch/prod(res(Biodiv_prio))) 

#dataframe for visualization is not needed in calculation
# t = data.frame(table(cut(values(Biodiv_prio_wo_pa), breaks=seq(0.85, 1.0, by=0.01))))
# t$area=t$Freq*10000
# t$share=t$area/area_ch
# t$current_sum <- rev(cumsum(rev(t$share)))
# t$diff <- n_todo-t$current_sum #shows in which value range the goal can be reached

#-------------------------------------------------------------------------
# Identify locations for new PAs based on bioversity prioritization map
#-------------------------------------------------------------------------

#mask Biodiv_prio map so that values inside PAs are 0
Biodiv_prio_wo_pa <- mask(Biodiv_prio, PA_total_rast, maskvalue=1)
#writeRaster(Biodiv_prio_wo_pa,file=paste0(getwd(),"/Prio_without_PA.tif"), overwrite=TRUE)

##TO DO: possibly exclude urban areas perhaps usinfg the settlement area polygons of swissTLM?
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, settlement, inverse=TRUE)
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, road, inverse=TRUE)
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, railway, inverse=TRUE)

#Equal amount of additional patches per timestep, using overall the best patches
#Get the n_cells with highest value
n_cells_values <- sort(values(Biodiv_prio_wo_pa), decreasing = TRUE)[1:n_cells]

# Set the rest of the cells to NA
Biodiv_prio_wo_pa[!(Biodiv_prio_wo_pa %in% n_cells_values)] <- NA

#two approaches to trial for patch identification
#1. terra::patches, Detect patches (clumps) i.e. groups of cells that are surrounded by cells that are NA.
#2. landscapemetrics::get_patches which forms patches based on class values 
#which could be used to better identify patches of high priority 
#however we would need to discretize the continuous cell values into bins

#Get patches
Patches_terra <- patches(Biodiv_prio_wo_pa)


writeRaster(Patches_terra,file= "Data/Spat_prob_perturb_layers/Protected_areas/Patches_terra.tif", overwrite=TRUE) 
Patches_terra <- rast("Data/Spat_prob_perturb_layers/Protected_areas/Patches_terra.tif")


#Raster with all the best cells, that would be enough to reach the desired share of protected area
writeRaster(Biodiv_prio_wo_pa,file= "Data/Spat_prob_perturb_layers/Protected_areas/Prio_bestpatches.tif", overwrite=TRUE) 
#Biodiv_prio_wo_pa <- rast("Data/Spat_prob_perturb_layers/Protected_areas/Prio_bestpatches.tif")

#https://stackoverflow.com/questions/69608840/selecting-such-vector-elements-so-that-the-sum-of-elements-is-exactly-equal-to-t

#calculate the area per unique value in Pateches_terra
value_freq <- freq(Patches_terra)
area_df <- as.data.frame(value_freq)
area_df <- area_df[c('value','count')]
colnames(area_df) <- c("patch_id", "num_cells")

# Create a raster stack
r_stack <- c(Patches_terra, Biodiv_prio_wo_pa)

# Use terra's zonal function to compute the median for each unique value in Patches_terra
zonal_median <- terra::zonal(r_stack, Patches_terra, fun = function(x) { median(x, na.rm = TRUE) })
zonal_median = zonal_median[c('patches','prioritization_no_PAs')]

# Rename the columns of zonal_median
colnames(zonal_median) <- c("patch_id", "median")

#merge dfs with median of prio and area of each patch
df_merge <- merge(area_df,zonal_median,by="patch_id")

findSumm = function(xy, sfind, nmax=10, tmax=100){
  x = xy[, "num_cells"]
  if(sum(x)<sfind) stop("Impossible solution! sum(x)<sfind!")
  
  fTimeSec = function() as.numeric(Sys.time()-l$tstart, units="secs")
  #The current selection of vector element
  sel = c(TRUE, rep(FALSE, length(x)-1))
  #List of intermediate states of the vector sel
  lsel = list()
  #List with a collection of parameters and results
  l = list(
    patch_ids = list(),
    x = sort(x, TRUE),
    tstart = Sys.time(),
    chosen = list(),
    xfind = list(),
    time = c(),
    stop = FALSE,
    reason = "")
  
  while(TRUE) {
    #Maximum Runtime Test
    if(fTimeSec()>tmax) {
      l$reason = "Calculation time is greater than tmax.\n"
      l$stop = TRUE
      break
    }
    
    #Record the solution and test the number of solutions
    if(sum(l$x[sel])==sfind){
      #Save solution
      l$chosen[[length(l$chosen)+1]] = sel
      l$xfind[[length(l$xfind)+1]] = l$x[sel]
      l$patch_ids[[length(l$patch_ids)+1]] = xy[match(l$x[sel], xy$num_cells), "patch_id"]
      l$time = c(l$time, fTimeSec())
      
      #Test the number of solutions
      if(length(l$chosen)==nmax){
        l$reason = "Already found nmax solutions.\n"
        l$stop = TRUE
        break
      }
    }
    
    idx = which(sel)
    if(idx[length(idx)]==length(sel)) {
      if(length(lsel)==0) break
      sel=lsel[[length(lsel)]]
      idx = which(sel)
      lsel[length(lsel)]=NULL
      sel[idx[length(idx)]]=FALSE
      sel[idx[length(idx)]+1]=TRUE
      next
    }
    
    if(sum(l$x[sel])>=sfind){
      sel[idx[length(idx)]]=FALSE
      sel[idx[length(idx)]+1]=TRUE
      next
    } else {
      lsel[[length(lsel)+1]] = sel  #Save the current state of sel vector
      sel[idx[length(idx)]+1]=TRUE
      next
    }
  }
  if(length(l$chosen)==0 & !l$stop) stop("No solutions!")
  
  l$reason = paste(l$reason, "Found", length(l$chosen),
                   "solutions in time", signif(fTimeSec(), 3), "seconds.\n")
  cat(l$reason)
  return(l)
}     


#hardcoded area needed in number of cells, because inputs missing for perc_todo
cell_goal_diff <- ceiling(41285*1000000 * 0.131 / 10000)

#pass the num_patch column as vector, s find is the number of patches still neede
result <- findSumm(df_merge, cell_goal_diff)


#result$chosen has boolean indicating, if an entry was chosen therefore each solution can be weighted according to the median(prio)*patchsize
best_value = 0
best_result = 0
for (i in 1:length(result$chosen)) {
  idx = which(result$chosen[[i]])
  x <- sum(df_merge$median[idx] * df_merge$num_cells[idx])
  print(x)
  if (x > best_value) {
    best_result <- i
    best_value <- x
  }
}
print(ids)
ids = result$patch_ids[[best_result]]
df_result = df_merge[df_merge$patch_id %in% ids, ]

result_patches = df_result$patch_id
new_pa <-  ifel(Patches_terra %in% result_patches, Patches_terra, NaN)
writeRaster(new_pa,file= "Data/Spat_prob_perturb_layers/Protected_areas/New_pas.tif", overwrite=TRUE) 




