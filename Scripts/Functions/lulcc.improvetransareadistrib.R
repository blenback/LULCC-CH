### =========================================================================
### lulcc.improvetransareadistrib: Improving distribution of transition areas
### to approximate expert values for scenarios 
### ===========================================================================
#' 
#'
#' @param transition_result vector of transitions result used as response vector for GRRF 
#' @param cov_data dataframe of predictors to be used in GRRF 
#' @param weight_vector vector of weights to be supplied to 'classwt' argument in RF
#' @param gamma numeric between 0-1) controls the weight
#'               of the normalized importance score and is called the importance coefficient. 
#'               Note that when Gamma = 0 we are essentially perform RRF (Regularized Random Forest) 
#'               i.e there is no 'Guided effect' vice versa when Gamma = 1 then we are applying
#'               the strongest guiding effect which will lead to the most penalisation of redundant
#'               features and hence the most concise feature sets
#' @returns dataframe of covariates selected by GRRF ranked according to importance (Mean decrease Gini)
#' @author Adapted from Deng (2013: https://arxiv.org/pdf/1306.0237.pdf)
#'         by Antoine Adde edited by Ben Black
#' @export

#Instantiate small function to calculate the mismatch between the total projected 
#area of each LULC class from the respective from/to transition area against
#the proscribed class area from the experts
lulcc.projectedclassareasmismatch <- function(Scenario_names,
                                               Scenario_trans_area_tables,
                                               Scenario_class_area_tables,
                                               Sim_time_steps,
                                               mismatch_thres){
   
  #loop over scenario's to calculate class area mismatches
  Scenario_mismatch_info <- lapply(Scenario_names, function(scenario){
  
    #seperate projected area table for scenario
    Proj_area_table <- Scenario_trans_area_tables[[scenario]]
  
    #seperate class area_table
    area_table <- Scenario_class_area_tables[[scenario]]
  
    #loop over unique lulc classes
    lulc_classes <- unique(c(Proj_area_table$Initial_class, Proj_area_table$Final_class))
  
    #calculate mismatches in class areas between the desired and projected 
    #negative values equate to the projected area being greater than desired
    #positive values indicate that the projected value is smaller than desired
    class_mismatches <- data.frame(t(sapply(lulc_classes, function(lulc){
      
    gain_area <- sum(Proj_area_table[Proj_area_table$Final_class == lulc, paste(Sim_time_steps)])
    loss_area <- sum(Proj_area_table[Proj_area_table$Initial_class == lulc, paste(Sim_time_steps)])
    area_change = gain_area-loss_area
    
    Proj_area_2060 <- area_table[area_table$LULC_class == lulc, "2020"]+area_change
    desired_area_2060 <- area_table[area_table$LULC_class == lulc, "2060"]
    Area_mismatch <- desired_area_2060-Proj_area_2060
    
    Perc_mismatch <- (Area_mismatch/desired_area_2060)*100
    
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
    
    #convert mismatch values to absolute 
    abs_class_perc_mismatches <- abs(class_perc_mismatches)
    
    #filter by threshold
    abs_class_perc_mismatches <- abs_class_perc_mismatches[abs_class_perc_mismatches > mismatch_thres]
    
    #sort
    greatest_diffs <- sort(abs_class_perc_mismatches, decreasing = TRUE)
    
    #loop over greatest diffs identifying which transitions to target
    oversized_target_trans <- list()
    undersized_target_trans <- list()
    oversized_target_trans_0ed <- list()
    undersized_target_trans_0ed <- list()
    
    for(H in names(greatest_diffs)){

      #get the actual class difference value
      class_diff <- class_area_mismatches[names(class_area_mismatches)== H]

      #if class_diff is positive then it is undersized so look for transitions 
      #to the class, seperating those that already have a positive area from
      #those  that are currently 0 
      if(class_diff >0){
    
        #identify transitions
        target_trans <- Proj_area_table[Proj_area_table$Final_class == H,]
        undersized_target_trans[[H]] <- target_trans[target_trans[["2025"]]!=0, "Trans_name"]
        undersized_target_trans_0ed[[H]] <- target_trans[target_trans[["2025"]]==0, "Trans_name"]
          
        }else if(class_diff <0){
  
       #if class_diff is negative then it is oversized so look for transitions 
        #from the class that are currently 0
        target_trans <- Proj_area_table[Proj_area_table$Initial_class == H & Proj_area_table[["2025"]]==0,]
        oversized_target_trans[[H]] <- target_trans[target_trans[["2025"]]!=0, "Trans_name"]
        oversized_target_trans_0ed[[H]] <- target_trans[target_trans[["2025"]]==0, "Trans_name"]
        }
    } #close loop over greatest diffs
      
    #identify which of the oversized/undersized transitions are synergistic
    Synergistic_trans <- intersect(unlist(undersized_target_trans), unlist(oversized_target_trans))
    Synergistic_trans_0ed <- intersect(unlist(undersized_target_trans_0ed), unlist(oversized_target_trans_0ed))
    
    #Return both the mismatches by classes and the totals across scenario's
    return(list(class_mismatches = data.frame(class_mismatches),
                Total_mismatch_area = Total_mismatch_area,
                Total_mismatch_perc = Total_mismatch_perc,
                Total_oversized =total_oversized,
                Total_undersized = total_undersized,
                Oversized_target_trans = oversized_target_trans,
                Undersized_target_trans = undersized_target_trans,
                Undersized_target_trans_0ed = undersized_target_trans_0ed,
                Oversized_target_trans_0ed = oversized_target_trans_0ed,
                Synergistic_trans = Synergistic_trans,
                Synergistic_trans_0ed = Synergistic_trans_0ed))
    
    }) #close loop over scenarios
  names(Scenario_mismatch_info) <- Scenario_names
  return(Scenario_mismatch_info)
} # close function

#Instantiate function to attempt to reduce mismatch by iteratively looping
#over tables and balancing over and undersized classes by assigning area
#to corresponding transitions in order of minimal impact on other class mismatches
lulcc.improvetransareadistrib <- function(Scenario_class_area_tables,
                                          old_trans_areas,
                                          Sim_time_steps,
                                          reps,
                                          mismatch_thres){

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

#check mismatch of old_trans_areas
old_areas_mismatch <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = old_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables,
                                                         mismatch_thres= mismatch_thres)

#Loop over scenarios attempting to reduce mismatch
for(scenario in names(old_trans_areas)){
  
  #seperate scenario table
  Scenario_table <- new_trans_areas[[scenario]]
  
  #seperate class area and perc mismatches
  class_area_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Area_mismatch"]])
  class_perc_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Perc_mismatch"]])
  
  #--------------------------------------------------------------------------
  #Step 1 Try adjusting synergistic transitions first and see if this
  # improves mismatch or not. 
  #--------------------------------------------------------------------------
  
  #At the moment this process works because there is never more than one 
  #synergistic transition per scenario and hence it is ok to loop over them and
  #use the old class mismatches values to determine how much area needs to be
  #allocated if there were >1 syneristic transition with overlapping 
  #initial/final classes this would not work because the amount allocated 
  #would have to be based upon new mismatch values calaculated after every step in the loop
  
  #combine details of synergistic trans
  synergistic_trans <- unique(c(old_areas_mismatch[[scenario]][["Synergistic_trans"]],
                              old_areas_mismatch[[scenario]][["Synergistic_trans_0ed"]]))
  
  if(length(synergistic_trans) != 0){
  
    #loop over synergistic transitions
    for(S_trans in synergistic_trans){
    
    #seperate info for transition
    trans_info <- old_trans_areas[[scenario]][old_trans_areas[[scenario]]$Trans_name == S_trans,]

    #mismatch area of the initial class
    initial_mismatch_area <- class_area_mismatches[names(class_area_mismatches) == trans_info$Initial_class]
    
    #mismatch area of the final class
    Final_mismatch_area <- class_area_mismatches[names(class_area_mismatches) == trans_info$Initial_class]
    
    #mismatch % of the initial class
    initial_mismatch_perc <- class_perc_mismatches[names(class_perc_mismatches) == trans_info$Initial_class]
    
    #mismatch % of the final class
    Final_mismatch_perc <- class_perc_mismatches[names(class_perc_mismatches) == trans_info$Initial_class]
    
    
    #use the area of the class with the smallest % mismatch value as the area to allocate
    if(abs(initial_mismatch_perc) >= abs(Final_mismatch_perc)){
      
      #calculate stepwise change 
      stepwise_area <- abs(Final_mismatch_area)/length(Sim_time_steps)
      
      }else if(abs(Final_mismatch_perc) > abs(initial_mismatch_perc)){
       stepwise_area <- abs(initial_mismatch_area)/length(Sim_time_steps) 
      } #close if/else statement
    
      #replace transition area values across time steps
      current_value <- new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == S_trans, paste(Sim_time_steps)]
      new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == S_trans, paste(Sim_time_steps)] <- current_value + stepwise_area 
    } #close loop over synergistic trans
      }#close if statement
  } #close loop over scenarios
 

#calculate class area mismatch after adjusting synergistic transitions 
Syn_mismatch_check <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = new_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables,
                                                         mismatch_thres = mismatch_thres)

#check to see if adjusting the synergistic transitions has reduced mismatch
Syn_mismatch_improvement <- sapply(names(old_areas_mismatch), function(x){
  Mismatch_diff <- old_areas_mismatch[[x]][["Total_mismatch_perc"]]-Syn_mismatch_check[[x]][["Total_mismatch_perc"]]
  })

#Identify which scenarios has seen a reduction in mismatch
Syn_improved_scenarios <- names(Syn_mismatch_improvement[Syn_mismatch_improvement > 0])

#for the scenarios that have been improved update old_trans_areas with their new tables
if(length(Syn_improved_scenarios) != 0){
  
  #replace old table with new
  for(scenario in Syn_improved_scenarios){
  old_trans_areas[[scenario]] <- new_trans_areas[[scenario]]
  } #close for loop
  
  #recalculate mismatch
  old_areas_mismatch <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = old_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables,
                                                         mismatch_thres= mismatch_thres)
} #close if statement

#--------------------------------------------------------------------------
#Step 2 adjust transition areas by allocating portions of the class mismatch 
# area to relevant transitions according to the smallest relative changes
# in mismatch to the opposing classes
#--------------------------------------------------------------------------

#2nd loop over scenarios  
for(scenario in names(old_trans_areas)){
  
  #seperate scenario table
  Scenario_table <- new_trans_areas[[scenario]]
  
  #seperate class area and perc mismatches
  class_area_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Area_mismatch"]])
  class_perc_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Perc_mismatch"]])

  #convert mismatch values to absolute and sort
  abs_class_perc_mismatches <- abs(unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Perc_mismatch"]]))
  
  #filter by threshold
  abs_class_perc_mismatches <- abs_class_perc_mismatches[abs_class_perc_mismatches >mismatch_thres]
  
  #sort
  greatest_mismatch <- sort(abs_class_perc_mismatches, decreasing = TRUE)

  #seperate class area and perc mismatches
  class_area_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Area_mismatch"]])
  class_perc_mismatches <- unlist(old_areas_mismatch[[scenario]][["class_mismatches"]][["Perc_mismatch"]])

  #combine undersized trans
  undersized_keys <- unique(c(names(old_areas_mismatch[[scenario]][["Undersized_target_trans"]]),
                              names(old_areas_mismatch[[scenario]][["Undersized_target_trans_0ed"]])))
  Undersized_trans <- setNames(mapply(c, old_areas_mismatch[[scenario]][["Undersized_target_trans"]][undersized_keys],
                                      old_areas_mismatch[[scenario]][["Undersized_target_trans_0ed"]][undersized_keys]),
                               undersized_keys)
 
  #combine oversized trans
  Oversized_keys <- unique(c(names(old_areas_mismatch[[scenario]][["Oversized_target_trans"]]),
                              names(old_areas_mismatch[[scenario]][["Oversized_target_trans_0ed"]])))
  Oversized_trans <- setNames(mapply(c, old_areas_mismatch[[scenario]][["Oversized_target_trans"]][Oversized_keys],
                                      old_areas_mismatch[[scenario]][["Oversized_target_trans_0ed"]][Oversized_keys]),
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
      #special case for when all mismatch factors = 0 which is when
      #opposing classes are not mismatched
      if(sum(Mismatch_factors) != 0){
        Trans_mismatch_perc_alloc <- Mismatch_factors/sum(Mismatch_factors) 
      } else{
        Trans_mismatch_perc_alloc <- rep(1/length(Mismatch_factors), length(Mismatch_factors))
      }
      
      #area to be assigned to each transition for each time step
      Trans_area_alloc <- (abs(class_mismatch)*Trans_mismatch_perc_alloc)/length(Sim_time_steps)
       
      #add values to trans_area table
      for(j in names(Trans_area_alloc)){
          
        trans_area_step <- Trans_area_alloc[names(Trans_area_alloc)==j]
          
        #replace transition area values across time steps
        current_areas <- old_trans_areas[[scenario]][old_trans_areas[[scenario]]["Trans_name"] == j, paste(Sim_time_steps)]
        new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == j, paste(Sim_time_steps)] <- current_areas + trans_area_step
        }#close loop replacing values in table
                        
      } #close if statement for multiple trans open statement
        # case of for single transition
      else if(length(class_trans) == 1){
        
      #step wise amount of area to be allocated to eliminate mismatch  
      Step_change <- abs(class_mismatch)/length(Sim_time_steps)
      
      #replace transition area values across time steps
      current_areas <- old_trans_areas[[scenario]][old_trans_areas[[scenario]]["Trans_name"] == names(class_trans), paste(Sim_time_steps)]
      new_trans_areas[[scenario]][new_trans_areas[[scenario]]["Trans_name"] == names(class_trans), paste(Sim_time_steps)] <- current_areas + Step_change
      }#close if statement for single trans
} #close loop over lulc classes
  
} #close loop over scenarios

#check mismatch of new transition areas
new_areas_mismatch <- lulcc.projectedclassareasmismatch(Scenario_names = names(old_trans_areas),
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = new_trans_areas,
                                                         Scenario_class_area_tables = Scenario_class_area_tables,
                                                         mismatch_thres = mismatch_thres)

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




