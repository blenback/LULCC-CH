#Instantiate small function to calculate the mismatch between the total projected 
#area of each LULC class from the respective from/to transition area against
#the proscribed class area from the experts
lulcc.projectedclassareasmismatchstepwise <- function(Scenario_names,
                                               Scenario_trans_area_tables,
                                               Scenario_class_area_tables,
                                               Sim_years){
   
  #loop over scenario's to calculate class area mismatches
  Scenario_mismatch_info <- lapply(Scenario_names, function(scenario){
  
    #seperate projected area table for scenario
    Proj_area_table <- Scenario_trans_area_tables[[scenario]]
  
    #seperate class area_table
    area_table <- Scenario_class_area_tables[[scenario]]
  
    #vector unique lulc classes
    lulc_classes <- unique(c(Proj_area_table$Initial_class, Proj_area_table$Final_class))
  
    #upper loop over time steps
    Time_step_mistmatches <- list()
    for(n in Sim_years[2:length(Sim_years)]){
      
    current_year <- n
    previous_year <- paste(as.numeric(n)-5)
        
    #calculate mismatches in class areas between the desired and projected 
    #negative values equate to the projected area being greater than desired
    #positive values indicate that the projected value is smaller than desired
    class_mismatches <- data.frame(t(sapply(lulc_classes, function(lulc){
      
    gain_area <- sum(Proj_area_table[Proj_area_table$Final_class == lulc, current_year])
    loss_area <- sum(Proj_area_table[Proj_area_table$Initial_class == lulc, current_year])
    area_change = gain_area-loss_area
    
    Proj_area <- area_table[area_table$LULC_class == lulc, previous_year]+area_change
    desired_area <- area_table[area_table$LULC_class == lulc, current_year]
    Area_mismatch <- desired_area-Proj_area
    
    Perc_mismatch <- (Area_mismatch/desired_area)*100
    
    return(list(Area_mismatch = Area_mismatch,
           Perc_mismatch = Perc_mismatch))
    }))) # close loop over LULC classes
    
    class_area_mismatches <- unlist(class_mismatches$Area_mismatch)
    class_perc_mismatches <- unlist(class_mismatches$Perc_mismatch)                                
  
    #calculate the total area of mismatch
    Total_mismatch_area <- sum(abs(class_area_mismatches))
    Total_mismatch_perc <- sum(abs(class_perc_mismatches))
    
    #calculate total undersized and oversized classes to see which is greater
    total_undersized <- sum(class_perc_mismatches[class_perc_mismatches >0])
    total_oversized <- sum(class_perc_mismatches[class_perc_mismatches <0])
    
    #convert mismatch values to absolute and sort
    abs_class_perc_mismatches <- abs(class_perc_mismatches)
    greatest_diffs <- sort(abs_class_perc_mismatches[abs_class_perc_mismatches >3], decreasing = TRUE)
    
    #loop over greatest diffs identifying which transitions to target
    oversized_target_trans <- list()
    undersized_target_trans <- list()
    oversized_target_trans_0ed <- list()
    undersized_target_trans_0ed <- list()
    
    for(i in names(greatest_diffs)){

      #get the actual class difference value
      class_diff <- class_area_mismatches[names(class_area_mismatches)== i]

      #if class_diff is positive then it is undersized so look for transitions 
      #to the class, seperating those that already have a positive area from
      #those  that are currently 0 
      if(class_diff >0){
    
        #identify transitions
        target_trans <- Proj_area_table[Proj_area_table$Final_class == i,]
        undersized_target_trans[[i]] <- target_trans[target_trans[["2025"]]!=0, "Trans_name"]
        undersized_target_trans_0ed[[i]] <- target_trans[target_trans[["2025"]]==0, "Trans_name"]
          
        }else if(class_diff <0){
  
       #if class_diff is negative then it is oversized so look for transitions 
        #from the class that are currently 0
        target_trans <- Proj_area_table[Proj_area_table$Initial_class == i & Proj_area_table[["2025"]]==0,]
        oversized_target_trans[[i]] <- target_trans[target_trans[["2025"]]!=0, "Trans_name"]
        oversized_target_trans_0ed[[i]] <- target_trans[target_trans[["2025"]]==0, "Trans_name"]
        }
    } #close loop over greatest diffs
      
    #identify which of the oversized/undersized transitions are synergistic
    Synergistic_trans <- intersect(unlist(undersized_target_trans), unlist(oversized_target_trans))
    Synergistic_trans_0ed <- intersect(unlist(undersized_target_trans_0ed), unlist(oversized_target_trans_0ed))
    
    #Return both the mismatches by classes and the totals across scenario's
    Time_step_mistmatches[[n]] <- list(class_mismatches = data.frame(class_mismatches),
                Total_mismatch_area = Total_mismatch_area,
                Total_mismatch_perc = Total_mismatch_perc,
                Total_oversized =total_oversized,
                Total_undersized = total_undersized,
                Oversized_target_trans = oversized_target_trans,
                Undersized_target_trans = undersized_target_trans,
                Undersized_target_trans_0ed = undersized_target_trans_0ed,
                Oversized_target_trans_0ed = oversized_target_trans_0ed,
                Synergistic_trans = Synergistic_trans,
                Synergistic_trans_0ed = Synergistic_trans_0ed)
    }# close loop over time steps
    names(Time_step_mistmatches) <- paste(Sim_years[2:length(Sim_years)])
    return(Time_step_mistmatches)
    }) #close loop over scenarios
  names(Scenario_mismatch_info) <- Scenario_names
  return(Scenario_mismatch_info)
} # close function


test_stepwise <- lulcc.projectedclassareasmismatchstepwise(Scenario_names = names(Mod_trans_area_tables),
                                                         Sim_years = Sim_years,
                                                         Scenario_trans_area_tables = Mod_trans_area_tables,
                                                         Scenario_class_area_tables = Mod_area_tables)





#Instantiate function to attempt to reduce mismatch by iteratively looping
#over tables and balancing over and undersized classes by assigning area
#to corresponding transitions in order of minimal impact on other class mismatches
lulcc.improvetransareadistribstepwise <- function(Scenario_class_area_tables,
                                          old_trans_areas,
                                          Sim_years,
                                          Sim_time_steps,
                                          reps){

#lists to capture outputs during iteration
Iteration_area_tables <- list()
Iteration_mismatch_checks <- list()
Iteration_improvements <- list()

for(i in 1:reps){

#create duplicate of trans_area table for updating
if(i == 1){
old_trans_areas <- old_trans_areas  
}else{
old_trans_areas <- new_trans_areas 
}
new_trans_areas <- old_trans_areas

#Loop over scenarios attempting to reduce mismatch
for(scenario in names(old_trans_areas)){
  
  #seperate scenario table
  Scenario_table <- new_trans_areas[[scenario]]

  #loop over time steps
  for(n in Sim_years[2:length(Sim_years)]){
    
    current_year <- n
    previous_year <- paste(as.numeric(n)-5)
    step_years <- c(previous_year,current_year)
  
    #check mismatch of old_trans_areas
    old_areas_mismatch <- lulcc.projectedclassareasmismatchstepwise(Scenario_names = scenario,
                                                         Sim_years = step_years,
                                                         Scenario_trans_area_tables = old_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables)
  #convert mismatch values to absolute and sort
  abs_class_perc_mismatches <- abs(unlist(old_areas_mismatch[[scenario]][[n]][["class_mismatches"]][["Perc_mismatch"]]))
  greatest_mismatch <- sort(abs_class_perc_mismatches[abs_class_perc_mismatches >1], decreasing = TRUE)

  #seperate class area and perc mismatches
  class_area_mismatches <- unlist(old_areas_mismatch[[scenario]][[n]][["class_mismatches"]][["Area_mismatch"]])
  class_perc_mismatches <- unlist(old_areas_mismatch[[scenario]][[n]][["class_mismatches"]][["Perc_mismatch"]])

  #combine undersized trans
  undersized_keys <- unique(c(names(old_areas_mismatch[[scenario]][[n]][["Undersized_target_trans"]]),
                              names(old_areas_mismatch[[scenario]][[n]][["Undersized_target_trans_0ed"]])))
  Undersized_trans <- setNames(mapply(c, old_areas_mismatch[[scenario]][[n]][["Undersized_target_trans"]][undersized_keys],
                                      old_areas_mismatch[[scenario]][[n]][["Undersized_target_trans_0ed"]][undersized_keys]),
                               undersized_keys)
 
  #combine oversized trans
  Oversized_keys <- unique(c(names(old_areas_mismatch[[scenario]][[n]][["Oversized_target_trans"]]),
                              names(old_areas_mismatch[[scenario]][[n]][["Oversized_target_trans_0ed"]])))
  Oversized_trans <- setNames(mapply(c, old_areas_mismatch[[scenario]][[n]][["Oversized_target_trans"]][Oversized_keys],
                                      old_areas_mismatch[[scenario]][[n]][["Oversized_target_trans_0ed"]][Oversized_keys]),
                               Oversized_keys)
  
  #loop over classes in order of greatest mismatch
  for(class in names(greatest_mismatch)){
  
    #get the actual class area mismatch value
    class_mismatch <- class_area_mismatches[names(class_area_mismatches) == class]

    #if class_diff is positive then it is undersized separate the relevant
    #transitions to this class if there is more than one relevant transition 
    #then loop over them calculating the % change of the class mismatch area
    #to be assigned to them based on their degree of mismatch of the opposing class
    if(class_mismatch >0){
      
      class_trans <- na.omit(unlist(Undersized_trans[names(Undersized_trans)== class]))
      opp_col_name <- "Initial_class"
    }else if(class_mismatch <0){ #close if statement for undersized class and open for oversized class
      
      #if class_diff is negative then seperate relevant oversized transitions
      class_trans <- na.omit(unlist(Oversized_trans[names(Oversized_trans)== class]))
      opp_col_name <- "Final_class"
    } #close if statement for oversized class
    
    if(length(class_trans) > 1){
      
      #loop over transitions getting absolute values of opposing class mismatch
      Opp_class_mismatches <- abs(sapply(class_trans, function(trans){
        
      #get opposing class name
      opp_class <- Scenario_table[Scenario_table$Trans_name == trans, opp_col_name]
        
      #get mismatch of oppsing class
      opp_class_mismatch <- class_perc_mismatches[names(class_perc_mismatches) == opp_class]
      
      }))#close loop over transitions   
      
      #rename vector
      names(Opp_class_mismatches) <- class_trans
        
      #allocate portions of the class mismatch area according to the smallest 
      #relative changes in mismatch to the opposing class
        
      #total mismatch area of opposing classes
      Total_opp_mismatch <- sum(abs(Opp_class_mismatches))
        
      #calculate a mismatch factor for each trans: total mismatch/ trans mismatch
      Mismatch_factors <- Total_opp_mismatch/Opp_class_mismatches
      Mismatch_factors[!is.finite(Mismatch_factors)] <- 0
        
      #% of mismatch area to be assigned to each trans
      Trans_mismatch_perc_alloc <- Mismatch_factors/sum(Mismatch_factors) 

      #area to be assigned to each transition for each time step
      Trans_area_alloc <- class_mismatch*Trans_mismatch_perc_alloc
        
      #add values to trans_area table
      for(j in names(Trans_area_alloc)){
          
        trans_area_step <- Trans_area_alloc[names(Trans_area_alloc)==j]
          
        #replace transition area values across time steps
        current_areas <- old_trans_areas[[scenario]][old_trans_areas[[scenario]]["Trans_name"] == j, n]
        new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == j, n] <- current_areas + trans_area_step
        }#close loop replacing values in table
                        
      } #close if statement for multiple trans open statement
        # case of for single transition
      else if(length(class_trans) == 1){
        
      #step wise amount of area to be allocated to eliminate mismatch  
      Step_change <- class_mismatch
      
      #replace transition area values across time steps
      current_areas <- old_trans_areas[[scenario]][old_trans_areas[[scenario]]["Trans_name"] == names(class_trans), n]
      new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == names(class_trans), n] <- current_areas + Step_change
      }#close if statement for single trans
} #close loop over lulc classes

} #close loop over time steps

} #close loop over scenarios

#check mismatch of new transition areas
new_areas_mismatch <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = Sim_time_steps,
                                                         Scenario_trans_area_tables = new_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables)

old_areas_mismatch <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = Sim_time_steps,
                                                         Scenario_trans_area_tables = old_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables)

#compare total mismatch values across 1st and 2nd iterations
Mismatch_improvement <- sapply(names(old_areas_mismatch), function(x){
  Mismatch_diff <- old_areas_mismatch[[x]][["Total_mismatch_perc"]]-new_areas_mismatch[[x]][["Total_mismatch_perc"]]
  })

#add iteration outputs to lists
Iteration_area_tables[[i]] <- new_trans_areas
Iteration_mismatch_checks[[i]] <- new_areas_mismatch 
Iteration_improvements[[i]] <- Mismatch_improvement

} #close loop over reps

return(list(Area_tables = Iteration_area_tables,
            Mismatch_checks = Iteration_mismatch_checks,
            Improvement_results = Iteration_improvements))

} #close function

#test function to see if focusing on the synergistic transitions first helps
test_func_stepwise <- lulcc.improvetransareadistribstepwise(Scenario_class_area_tables = Mod_area_tables,
                                           old_trans_areas = Mod_trans_area_tables,
                                           Sim_years = Sim_years,
                                           Sim_time_steps = Sim_time_steps,
                                           reps = 5)

test_post_syneristic <- lulcc.improvetransareadistrib(Scenario_class_area_tables = Mod_area_tables,
                                           old_trans_areas = Mod_trans_areas_3,
                                           Sim_time_steps = Sim_time_steps,
                                           reps = 20)

#results show that it helps for EI_NAT but for EI_CUL and EI_SOC it achieves much worse results!  
print(sapply(test_func$Mismatch_checks[[20]], function(x){x[["Total_mismatch_perc"]]}))
print(sapply(test_post_syneristic$Mismatch_checks[[20]], function(x){x[["Total_mismatch_perc"]]}))


