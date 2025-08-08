### =========================================================================
### lulcc.simulationtransitionratesolver: A linear programming solver for
### interpolating future rates of LULCC transitions under alternate scenarios
### and parameters for prediction 
### =========================================================================

#' @description
#' This function sets up and solves a linear programming (LP) problem to
#' interpolate future rates of land use/land cover change (LULCC) transitions
#' for multiple regions and scenarios. It uses initial and target LULC data,
#' and incorporates penalties for transition rate violations and shape deviations.
#' 
#' @param subset_lulc_exp_areas A data frame containing LULC classes, their initial
#' areas at the start of the simulation, expected areas at the end of the
#' simulation and the shapes of areal changes curves to be approximated by the solver
#' @param trans_rates A data frame containing the min/max transition rates for each Initial and Final LULC class
#' @param Num_steps An integer representing the number of time intervals for the simulation.
#' @param lambda_bounds A numeric value representing the penalty weight for
#' transition bound violations (default is 0.1).
#' @param mu_shape A numeric value representing the penalty weight for shape
#' deviations (default is 15).
#' @param margin A numeric value providing flexibility around min/max rates
#' (default is 0.01).
#' @param Step_length A numeric vector representing the lengths of each time step.
#' @param Time_steps A numeric vector representing the calendar years corresponding
#' to each time step.
#' @param use_temporal_smoothing_constraint A boolean indicating whether to
#' apply a temporal smoothing constraint to the LULC areas (default is TRUE).
#' Note the behaviour of this constraint tends to override the effect of the shape-based constraint
#' @param mu_temporal_smoothness A numeric value representing the penalty weight for
#' temporal smoothing constraints (default is 1).
#' @return A list containing the LP solution, including the optimal transition
#' rates, areas, and any deviations from bounds or shape constraints.
lulcc.simulationtransitionratesolver <- function(
  subset_lulc_exp_areas, 
  trans_rates = trans_rates, 
  Num_steps = Num_steps,  
  Step_length = Step_length, 
  Time_steps = Time_steps,
  use_temporal_smoothing_constraint = TRUE,
  mu_temporal_smoothness,
  lambda_bounds,  
  mu_shape,
  margin
  ){
  
  # Helper functions to get 1D index for LP variables (0-indexed t for intervals)
#' @param i,j,t Indices for LULC classes and time intervals
#' @return
#' Returns a 1D index for the transition variable x_{i->j,t}, lower and upper slack variables,
#' and area variable for LULC l at time t.
idx_x <- function(i,j,t) (t * L * L + (i-1)*L + j ) # Index for transition x_i,j,t
idx_devLower <- function(i,j,t) (n_x + idx_x(i,j,t) ) # Index for lower slack of x_i,j,t
idx_devUpper <- function(i,j,t) (n_x + n_lower + idx_x(i,j,t) ) # Index for upper slack of x_i,j,t

#' @param l,t Indices for LULC classes and time intervals
#' @return
#' Returns a 1D index for the area variable area_l,t.
#' This is used to track the area of LULC l at time t.
idx_area <- function(l,t) ( n_x + n_lower + n_upper + t*L + l ) # Index for area_l,t (0-indexed t for time points)


#' @description
#' # Helper function to build a row for the LP constraint matrix
#' # This function constructs a row for the LP constraint matrix that compares
#' # the change in area of a LULC class between two time steps t1 and t2,
#' # # scaled by the ratio of step lengths for those time steps.
#' #' @param l_i Index of the LULC class
#' #' @param t1 First time step index (0-indexed)
#' #' @param t2 Second time step index (0-indexed)
#' #' @param ratio Ratio of step lengths between t1 and t2
#' #' @return
#' # Returns a numeric vector representing the row for the LP constraint matrix.
build_diff_row <- function(l_i, t1, t2, ratio) {
  row_ <- numeric(n_x+n_lower+n_upper+n_area)
  row_[ idx_area(l_i,t1) ]   <- 1    # +area_l,t1
  row_[ idx_area(l_i,t1-1) ] <- -1   # -area_l,t1-1
  row_[ idx_area(l_i,t2) ]   <- -ratio # -ratio * area_l,t2
  row_[ idx_area(l_i,t2-1) ] <- ratio  # +ratio * area_l,t2-1
  row_
}

#' @description
#' # Helper functions to manage shape slack variables and constraints
#' # These functions are used to create and manage shape slack variables
#' # and to add shape-related inequalities to the LP model.
#' @param l_i Index of the LULC class for which the slack is created
#' # @return
#' # Returns a new shape slack variable ID, which is a global counter incremented
#' # each time a new slack is created. The LULC index is stored in a metadata list
#' # for later reference.
new_shape_slack <- function(l_i) {
  shapeSlack_count <<- shapeSlack_count + 1 # Increment global counter
  shapeSlack_meta[shapeSlack_count] <<- l_i # Store LULC index for this slack
  return(shapeSlack_count) # Return the ID of the new slack
}

#' @description 
#' # Function to add a shape inequality to the shape_list
#' # This function adds a shape inequality (with its slack variable index and coefficient)
#' # to the shape_list, which is used to store all shape-related constraints
#' # for the LP model.
#' @param rowVec Constraint expression for the shape inequality
#' @param sense Sense of the inequality (">=", "<=", etc.)
#' @param sIndex Index of the slack variable associated with this shape inequality
#' @param sCoeff Coefficient for the slack variable in the constraint
#' @return
#' # Adds the shape inequality to the global shape_list, which is later used
#' # to construct the final LP constraint matrix.
add_shape_ineq <- function(rowVec, sense, sIndex, sCoeff) {
  shape_list[[ length(shape_list)+1 ]] <<- list(
    row = rowVec,
    sense= sense,
    slackIndex= sIndex,
    slackCoeff= sCoeff
  )
}
  
  
  # vector list of unique LULC classes
  lulcs <- unique(subset_lulc_exp_areas$LULC) 
  
  # Number of total LULC classes
  L <- length(lulcs)
  
  # Total area of the region (should remain constant)
  region_area <- sum(subset_lulc_exp_areas$init_area)
  
  # Define counts for different types of decision variables in the LP
  # x_{i->j,t}: amount of LULC i transitioning to LULC j in interval t
  # devLower_{i->j,t}, devUpper_{i->j,t}: slack for transition rate bounds
  # area_{l,t}: area of LULC l at time step t (0 to Num_steps)
  n_x <- L * L * Num_steps       # Number of transition variables
  n_lower <- n_x         # Number of lower bound deviation slacks for transitions
  n_upper <- n_x         # Number of upper bound deviation slacks for transitions
  n_area  <- L*(Num_steps+1)     # Number of LULC area variables (for Num_steps+1 points in time)
  
  # Initialize lists to build the LP constraint matrix (A), RHS vector (b), and directions (dir)
  constr_list <- list()
  rhs_list <- c()
  dir_list <- c()
  
  # Constraint 1: Initial area definition
  # area_{l,0} = initial_amount_l
  for(l_i in seq_len(L)) {
    row_ <- numeric(n_x + n_lower + n_upper + n_area) # Initialize constraint row
    row_[idx_area(l_i, 0) ] <- 1 # Coefficient for area_l,0
    constr_list <- append(constr_list, list(row_))
    rhs_list <- c(rhs_list, subset_lulc_exp_areas$init_area[l_i]) # RHS is initial amount
    dir_list<- c(dir_list, "=") # Equality constraint
  }
  
  # Constraint: Total area conservation at each time step t
  # sum_l area_{l,t} = region_area for t = 0 to Num_steps
  for(t_i in 0:Num_steps) { # Iterate through all time points including initial and final
    row_ <- numeric(n_x + n_lower + n_upper + n_area)
    for(l_i in seq_len(L)) {
      row_[idx_area(l_i,t_i)] <- 1 # Sum of all LULC areas at time t_i
    }
    constr_list <- append(constr_list, list(row_))
    rhs_list<- c(rhs_list, region_area) # RHS is total region area
    dir_list<- c(dir_list, "=")
  }
  
  # Constraint: Flow conservation - outgoing transitions from LULC i
  # sum_j x_{i->j,t} = area_{i,t} for t = 0 to Num_steps-1
  # (Area of LULC i at time t must transition or remain as LULC i)
  for(t_i in 0:(Num_steps-1)) { # Iterate through time intervals
    for(i_i in seq_len(L)) { # For each source LULC i
      row_ <- numeric(n_x + n_lower + n_upper +n_area)
      for(j_j in seq_len(L)) { # Sum over all destination LULCs j
        row_[idx_x(i_i,j_j,t_i) ] <- 1 # Sum of x_i->j,t
      }
      row_[idx_area(i_i,t_i)] <- -1 # Must equal area_i,t
      constr_list <- append(constr_list, list(row_))
      rhs_list<- c(rhs_list, 0)
      dir_list<- c(dir_list, "=")
    }
  }
  
  # Constraint: Flow conservation - incoming transitions to LULC j
  # area_{j,t+1} = sum_i x_{i->j,t} for t = 0 to Num_steps-1
  # (Area of LULC j at t+1 is the sum of all transitions into j during interval t)
  for(t_i in 0:(Num_steps-1)) { # Iterate through time intervals
    for(j_j in seq_len(L)) { # For each destination LULC j
      row_ <- numeric(n_x + n_lower + n_upper + n_area)
      row_[idx_area(j_j, (t_i+1))] <- 1 # area_j,t+1
      for(i_i in seq_len(L)) { # Sum over all source LULCs i
        row_[idx_x(i_i,j_j,t_i)] <- -1 # Minus sum of x_i->j,t
      }
      constr_list <- append(constr_list, list(row_))
      rhs_list<- c(rhs_list, 0)
      dir_list<- c(dir_list, "=")
    }
  }
  
  # Constraint 2: Soft bounding for transition amounts (x_{i->j,t})
  # Based on min/max rates from df_trans_source, with margin and slack variables
  for(t_i in 0:(Num_steps-1)) { # Iterate through time intervals
    for(i_i in seq_len(L)) { # For each source LULC i
      lulc_i <- lulcs[i_i]
      sub_bound <- trans_rates %>% filter(iLULC == lulc_i) # Bounds for transitions from lulc_i
      min_map <- setNames(sub_bound$minRate, sub_bound$jLULC) # Map jLULC to its minRate
      max_map <- setNames(sub_bound$maxRate, sub_bound$jLULC) # Map jLULC to its maxRate
      
      for(j_j in seq_len(L)) { # For each destination LULC j
        lulc_j <- lulcs[j_j]
        r_min  <- min_map[lulc_j] # Min rate for i->j
        r_max  <- max_map[lulc_j] # Max rate for i->j
        if(is.na(r_min)) r_min <- 0 # Default min rate if NA
        if(is.na(r_max)) r_max <- 0 # Default max rate if NA (or could be 1 for i==j)
        
        r_min_clamped <- max(r_min, 0) # Ensure min rate is not negative
        
        # Upper bound: x_{i->j,t} <= (r_max + margin) * area_{i,t} + devUpper_{i->j,t}
        # Rewrite: x_{i->j,t} - (r_max + margin)*area_{i,t} - devUpper_{i->j,t} <= 0
        row_up <- numeric(n_x + n_lower + n_upper +n_area)
        row_up[idx_x(i_i,j_j,t_i)] <- 1
        
        # if r_max == 0 i.e. transition is not allowed then set the upper bound to 0
        if(r_max == 0){
          row_up[idx_area(i_i,t_i)] <- 0
        } else{
          row_up[idx_area(i_i,t_i)] <- -(r_max + margin)  
        }
        row_up[idx_devUpper(i_i,j_j,t_i)] <- -1 # Slack for exceeding upper bound
        constr_list <- append(constr_list, list(row_up))
        rhs_list<- c(rhs_list, 0)
        dir_list<- c(dir_list, "<=")
        
        # Lower bound: x_{i->j,t} >= (r_min_clamped - margin) * area_{i,t} - devLower_{i->j,t}
        # Rewrite: -x_{i->j,t} + (r_min_clamped - margin)*area_{i,t} - devLower_{i->j,t} <= 0
        row_low <- numeric(n_x + n_lower + n_upper + n_area)
        row_low[idx_x(i_i,j_j,t_i)] <- -1
        
        if(r_min_clamped == 0){
          row_low[idx_area(i_i,t_i)] <- 0
        } else{
          row_low[idx_area(i_i,t_i)] <- (r_min_clamped - margin)  
        }
        row_low[idx_devLower(i_i,j_j,t_i)] <- -1 # Slack for falling below lower bound
        constr_list <- append(constr_list, list(row_low))
        rhs_list<- c(rhs_list, 0)
        dir_list<- c(dir_list, "<=")
      }
    }
  }
  
  # Constraint 3) Final area constrained within a +/- 1% range of the target
  # For each LULC l: 0.99 * target_area[l] <= area_{l,Num_steps} <= 1.01 * target_area[l]
  for(l_i in seq_len(L)) {
    # Calculate lower and upper bounds for the final area of LULC l_i
    lowerBound <- 0.99 * subset_lulc_exp_areas$final_area[l_i]
    upperBound <- 1.01 * subset_lulc_exp_areas$final_area[l_i]
    
    # Constraint: area_{l_i,Num_steps} >= 0.99 * final_target_area[l_i]
    row_min <- numeric(n_x + n_lower + n_upper + n_area)
    row_min[idx_area(l_i, Num_steps)] <- 1 # Coefficient for area_l_i,Num_steps
    constr_list <- append(constr_list, list(row_min))
    rhs_list <- c(rhs_list, lowerBound) # RHS is the 99% bound
    dir_list <- c(dir_list, ">=")       # Direction is greater than or equal to
    
    # Constraint: area_{l_i,Num_steps} <= 1.01 * final_target_area[l_i]
    row_max <- numeric(n_x + n_lower + n_upper + n_area)
    row_max[ idx_area(l_i,Num_steps) ] <- 1 # Coefficient for area_l_i,Num_steps
    constr_list <- append(constr_list, list(row_max))
    rhs_list <- c(rhs_list, upperBound) # RHS is the 101% bound
    dir_list <- c(dir_list, "<=")       # Direction is less than or equal to
  }
  
  # Constraint 4: Hard monotonic direction for LULC area change
  # Based on whether final area is greater or less than initial area
  for(l_i in seq_len(L)) {
    initA  <- subset_lulc_exp_areas$init_area[l_i] # Initial area of LULC l_i
    finA   <- subset_lulc_exp_areas$final_area[l_i]  # Final target area of LULC l_i
    
    if(finA > initA) {
      # If LULC is expected to grow, enforce area(t) >= area(t-1) for all t
      # Rewrite: area(t) - area(t-1) >= 0
      for(t_i in 1:Num_steps) { # For t=1 to Num_steps (comparing with t-1)
        row_ <- numeric(n_x + n_lower + n_upper + n_area)
        row_[idx_area(l_i,t_i)]   <- 1  # area_l,t_i
        row_[idx_area(l_i,t_i-1)] <- -1 # - area_l,t_i-1
        constr_list <- append(constr_list, list(row_))
        rhs_list<- c(rhs_list, 0)
        dir_list<- c(dir_list, ">=") # Must be non-decreasing
      }
    } else if(finA < initA) {
      # If LULC is expected to decline, enforce area(t) <= area(t-1) for all t
      # Rewrite: area(t) - area(t-1) <= 0
      for(t_i in 1:Num_steps) {
        row_ <- numeric(n_x + n_lower + n_upper + n_area)
        row_[idx_area(l_i,t_i)]   <- 1
        row_[idx_area(l_i,t_i-1) ] <- -1
        constr_list <- append(constr_list, list(row_))
        rhs_list<- c(rhs_list, 0)
        dir_list<- c(dir_list, "<=") # Must be non-increasing
      }
    }
    # If finalA == initA, no hard monotonic constraint by default; shape handles constant.
  }
  
  # Combine all "core" constraints (related to areas and transitions)
  A_core <- do.call(rbind, constr_list) # Core constraint matrix
  ddir_core <- dir_list # Core constraint directions
  rhs_core  <- rhs_list  # Core constraint RHS values
  
  # Constraint 5: Soft shape constraints on area change patterns
  # Compares (area_l,t1 - area_l,t1-1)/len(t1) with (area_l,t2 - area_l,t2-1)/len(t2)

  shape_list <- list() # To store definitions of shape constraints before adding to main matrix
  shapeSlack_count <- 0 # Counter for shape slack variables
  
  # Store which LULC each shape slack belongs to for normalized penalty scaling
  shapeSlack_meta <- c() # Metadata: maps slack index to LULC index l_i
  
  # Add shape constraints for each LULC based on its 'chosen_shape'
  for(l_i in seq_len(L)) { # For each LULC type
    shape_ <- subset_lulc_exp_areas$chosen_shape[l_i] # The chosen shape (e.g., "Instant growth")
    initA  <- subset_lulc_exp_areas$init_area[l_i]
    finA   <- subset_lulc_exp_areas$final_area[l_i]
    if(shape_ %in% c("Instant growth","Delayed growth","Constant change",
                     "Instant decline","Delayed decline")) {
      # These shapes compare rates of change over consecutive intervals
      for(t_i in 1:(Num_steps-1)) { # Compare interval t_i with interval t_i+1
        ratio <- Step_length[t_i]/Step_length[t_i+1] # Ratio of time step lengths
        rowC <- build_diff_row(l_i, t_i, t_i+1, ratio) # Change rate comparison row
        sIndex <- new_shape_slack(l_i)  # Create a new slack variable
        
        if(shape_=="Instant growth" && finA>initA) {
          # Rate of growth decreases or stays constant: R(t_i) >= R(t_i+1) => rowC >= -slack
          add_shape_ineq(rowC, ">=", sIndex, +1)
        } else if(shape_=="Delayed growth" && finA>initA) {
          # Rate of growth increases or stays constant: R(t_i) <= R(t_i+1) => rowC <= +slack
          add_shape_ineq(rowC, "<=", sIndex, -1)
        } else if(shape_=="Instant decline" && finA<initA) {
          # Rate of decline higher at start (more negative): R(t_i) <= R(t_i+1) => rowC <= +slack
          add_shape_ineq(rowC, "<=", sIndex, -1)
        } else if(shape_=="Delayed decline" && finA<initA) {
          # Rate of decline lower at start (less negative): R(t_i) >= R(t_i+1) => rowC >= -slack
          add_shape_ineq(rowC, ">=", sIndex, +1)
        } else if(shape_=="Constant change") {
          # Rate of change is constant: R(t_i) = R(t_i+1) => Two inequalities
          add_shape_ineq(rowC, ">=", sIndex, +1) # rowC >= -slack1
          s2 <- new_shape_slack(l_i)             # New slack for the second part
          add_shape_ineq(rowC, "<=", s2, -1)    # rowC <= +slack2
        }
      }
    }
  }
  
  # Total number of variables before adding shape slacks
  total_vars_core <- n_x + n_lower + n_upper + n_area
  # Final total number of variables including shape slacks
  total_vars_final <- total_vars_core + shapeSlack_count
  
  # Expand A_core (core constraint matrix) to accommodate shape slack variables
  final_constr_list <- lapply(seq_len(nrow(A_core)), function(rr) {
    rowF <- numeric(total_vars_final) # New row with final total variable count
    rowF[1:total_vars_core] <- A_core[rr,] # Copy existing constraint coefficients
    rowF # Return the expanded row
  })
  final_dir <- ddir_core # Directions remain the same for core constraints
  final_rhs <- rhs_core  # RHS values remain the same
  
  # Starting index for shape slack variables in the full variable vector
  shapeSlack_start <- total_vars_core + 1
  
  # Add shape constraints (from shape_list) to the final constraint set
  for(sc_i in seq_along(shape_list)) {
    sc <- shape_list[[sc_i]] # Get one shape constraint definition
    rowCore <- sc$row         # Coefficients for core variables
    sense <- sc$sense       # ">=" or "<="
    sIndex <- sc$slackIndex  # Which shape slack variable (1 to shapeSlack_count)
    sCoeff <- sc$slackCoeff  # Coefficient for this slack in the constraint
    
    rowF <- numeric(total_vars_final) # Initialize new constraint row
    rowF[1:total_vars_core] <- rowCore # Set coefficients for core variables
    # Set coefficient for the specific shape slack: rowCore_vars + sCoeff * slack_var {sense} 0
    rowF[ shapeSlack_start + sIndex -1 ] <- sCoeff
    
    final_constr_list[[ length(final_constr_list)+1 ]] <- rowF # Add to list of constraints
    final_dir <- c(final_dir, sense) # Add its direction
    final_rhs<- c(final_rhs, 0)      # RHS is 0 for these shape constraints
  }
  
  # Constraint 6: Temporal smoothing constraint (if enabled)
  if(use_temporal_smoothing_constraint == TRUE){
    n_smooth <- L * (Num_steps - 1)
    smoothSlack_start <- total_vars_final + 1
    total_vars_final <- total_vars_final + n_smooth

    for(l_i in seq_len(L)) {
      for(t_i in 1:(Num_steps - 1)) {
        slack_idx <- smoothSlack_start + (l_i - 1) * (Num_steps - 1) + t_i - 1
        row_pos <- numeric(total_vars_final)
        row_pos[idx_area(l_i, t_i + 1)] <- 1
        row_pos[idx_area(l_i, t_i)]     <- -2
        row_pos[idx_area(l_i, t_i - 1)] <- 1
        row_pos[slack_idx] <- -1
        final_constr_list[[length(final_constr_list) + 1]] <- row_pos
        final_dir <- c(final_dir, "<=")
        final_rhs <- c(final_rhs, 0)

        row_neg <- -row_pos
        row_neg[slack_idx] <- -1
        final_constr_list[[length(final_constr_list) + 1]] <- row_neg
        final_dir <- c(final_dir, "<=")
        final_rhs <- c(final_rhs, 0)
      }
    }

    final_constr_list <- lapply(final_constr_list, function(row) {
      if (length(row) < total_vars_final) {
        c(row, rep(0, total_vars_final - length(row)))
      } else {
        row
      }
    })
  }
  
  # Combine all constraints into final matrix, direction vector, and RHS vector
  A <- do.call(rbind, final_constr_list)
  ddir <- final_dir
  rhs  <- final_rhs
  
  # E) Build objective function with normalized weighting for penalties
  obj <- numeric(total_vars_final) # Initialize objective function coefficients
  
  # Define start/end indices for slack variables in the 'obj' vector (not strictly needed here but good for clarity)
  devLower_start <- n_x + 1
  devLower_end   <- n_x + n_lower
  devUpper_start <- devLower_end + 1
  devUpper_end   <- devLower_end + n_upper
  shapeSlack_end <- shapeSlack_start + shapeSlack_count - 1 # Max index for shape slacks
  
  # Normalise transition and shape slack penalties by 1 / initial_amount(LULC)
  safe_init <- pmax(subset_lulc_exp_areas$init_area, 1e-6) # Prevent division by zero if init_area is 0
  weight_i   <- 1 / safe_init # Normalization weight for each LULC type
  
  # Penalties for transition bound violations (devLower, devUpper slacks)
  for(t_i in 0:(Num_steps-1)) { # For each time interval
    for(i_i in seq_len(L)) { # For each source LULC i
      w <- weight_i[i_i]  # Penalty weight depends on initial amount of source LULC 'i'
      for(j_j in seq_len(L)) { # For each destination LULC j
        dL_idx <- idx_devLower(i_i,j_j,t_i) # Index of devLower slack
        dU_idx <- idx_devUpper(i_i,j_j,t_i) # Index of devUpper slack
        # Objective coefficient = lambda_bounds * (1 / initial_amount_i)
        obj[dL_idx] <- lambda_bounds * w
        obj[dU_idx] <- lambda_bounds * w
      }
    }
  }
  
  # Penalties for shape deviations (shapeSlacks)
  # Each shape slack (s_i) belongs to a specific LULC (shapeSlack_meta[s_i])
  for(s_i in seq_len(shapeSlack_count)) { # For each shape slack variable
    l_i <- shapeSlack_meta[s_i] # Get the LULC type this slack is associated with
    w   <- weight_i[l_i]        # Get its normalization weight
    # Objective coefficient = mu_shape * (1 / initial_amount_l)
    obj[ shapeSlack_start + s_i - 1 ] <- mu_shape * w
  }
  
  # conditional logic for temporal smoothing constraint
  if(use_temporal_smoothing_constraint == TRUE){
    for(l_i in seq_len(L)) {
      w <- weight_i[l_i]
      for(t_i in 1:(Num_steps - 1)) {
        idx <- smoothSlack_start + (l_i - 1)*(Num_steps - 1) + t_i - 1
        obj[idx] <- mu_temporal_smoothness * w
      }
    }
  }
  
  # F) Solve the Linear Program
  sol <- lp(
    direction="min",         # Minimize the objective function (sum of weighted penalties)
    objective.in= obj,       # Coefficients of the objective function
    const.mat= A,            # Constraint matrix
    const.dir= ddir,         # Constraint directions (<=, >=, =)
    const.rhs= rhs,          # Constraint right-hand side values
    all.int= FALSE           # Variables are continuous
  )
  if(sol$status != 0) {
    # 0 indicates a successful solution was found
    warning(paste("LP solver failed, status:", sol$status))
    return(NULL) # Return NULL if no solution or an error occurred
  }
  solVec <- sol$solution # Vector of optimal values for all decision variables
  
  # Reconstruct final LULC areas from the solution vector
  get_area_val <- function(l,t) { # Helper to get area_l,t from solVec
    idx_area_base <- n_x + n_lower + n_upper # Offset to where area variables start
    solVec[idx_area_base + t*L + l] # l is 1-indexed, t is 0-indexed for idx_area
  }
  df_areas_list <- list() # List to store area data frames
  for(t_i in 0:Num_steps) { # Iterate through all time points (0 to Num_steps)
    for(l_i in seq_len(L)) { # For each LULC type
      valA <- get_area_val(l_i, t_i) # Get optimized area
      df_areas_list[[length(df_areas_list)+1]] <- data.frame(
        LULC= lulcs[l_i],
        timeStep= t_i, # 0-indexed time step
        Year= Time_steps[t_i+1], # Corresponding calendar year
        area= valA
      )
    }
  }
  df_areas <- do.call(rbind, df_areas_list) # Combine into a single data frame
  
  # Reconstruct transition amounts (x_values) and optimized rates
  get_x_val <- function(i,j,t) solVec[ idx_x(i,j,t) ] # Helper for x_i,j,t
  df_trans_list <- list()
  for(t_i in 0:(Num_steps-1)) { # Iterate through time intervals
    area_t <- df_areas %>% filter(timeStep==t_i) # Get areas at the start of interval t_i
    area_map<- setNames(area_t$area, area_t$LULC) # Map LULC to its area for rate calculation
    
    for(i_i in seq_len(L)) { # For each source LULC
      for(j_j in seq_len(L)) { # For each destination LULC
        xv <- get_x_val(i_i,j_j,t_i) # Optimized transition amount
        initA_lulc_i <- area_map[lulcs[i_i]] # Area of source LULC i at time t_i
        # Calculate optimized transition rate; handle division by zero
        r_ij <- if(initA_lulc_i > 1e-9) xv / initA_lulc_i else 0
        df_trans_list[[length(df_trans_list)+1]] <- data.frame(
          timeStep= t_i,
          YearFrom= Time_steps[t_i+1], # Start year of interval
          YearTo=   Time_steps[t_i+2], # End year of interval
          LULC_from= lulcs[i_i],
          LULC_to=   lulcs[j_j],
          x_value=   xv, # Absolute amount transitioned
          optimized_rate= r_ij # Transition rate relative to source LULC area
        )
      }
    }
  }
  df_trans <- do.call(rbind, df_trans_list) # Combine into a single data frame
  
  # Return results: status, objective value, areas, shapes, and transitions
  output <- list(
    status= sol$status,
    objectiveVal= sol$objval, # Final objective function value
    final_areas= df_areas,
    shape_map= subset_lulc_exp_areas %>% select(LULC, chosen_shape), # LULC to chosen shape mapping
    transitions= df_trans
  )
  return(output)
}


#' @description
#' a wrapper function to solve LULC transitions rates using the function
#' `lulcc.simulationtransitionratesolver` for multiple scenarios and regions.
#' #' @param lulc_exp_areas A data frame containing LULC classes, their initial
#' areas at the start of the simulation, expected areas at the end of the
#' simulation, and the shapes of areal changes curves to be approximated by the solver
#' according to each scenario and region.
#' @param region_col A string specifying the column name for region identifiers
#' @param scenario_col A string specifying the column name for scenario identifiers
#' @param trans_rates A data frame containing the min/max transition rates for each Initial and Final LULC class
#' @return
#' # Returns a list containing:
#' # - "Solutions": A named list of data frames with objective values for each scenario and region.
#' # - "Scenario_areas": A named list of data frames with LULC areas for each scenario and region.
#' # - "scenario_trans_rates": A named list of data frames with transition rates for each scenario and region.
lulcc.solvescenariosregions <- function(
  lulc_exp_areas = lulc_exp_areas_period,  
  region_col = "Region", # Region identifier
  scenario_col = "Scenario",
  trans_rates = trans_rates, # Scenario identifier
  Time_steps = current_period_steps,
  Step_length = Step_length, 
  lambda_bounds = lambda_bounds, 
  mu_shape = mu_shape, 
  margin = margin, 
  use_temporal_smoothing_constraint = use_temporal_smoothing_constraint,
  mu_temporal_smoothness = mu_temporal_smoothness
  ){
  
  regions   <- sort(unique(lulc_exp_areas[[region_col]])) # Get all unique regions
  scenarios <- sort(unique(lulc_exp_areas[[scenario_col]])) # Get all unique scenarios
  
  # calculate Num_steps from the Time_steps vector
  Num_steps <- length(Time_steps)-1
  
  # Initialize lists to store results from all runs
  all_solutions <- list() # For summary of solutions (objective values)
  df_areas_all  <- list() # For LULC areas from all solutions
  df_trans_all  <- list() # For transition data from all solutions
  
  # loop over scenarios
  for(scn in scenarios){
    
    # loop over regions
    for(rg in regions) {
  
      cat("\n============================================\n")
      cat("Solving for Scenario = ", scn, "and Region =", rg, "\n")
    
      # Subset data for the current region and scenario
      subset_lulc_exp_areas <- lulc_exp_areas[lulc_exp_areas[[region_col]] == rg & lulc_exp_areas[[scenario_col]] == scn,]

      # Run the LP model with specified penalty weights
      sol <- lulcc.simulationtransitionratesolver(
        subset_lulc_exp_areas,
        trans_rates = trans_rates, 
        Num_steps = Num_steps,
        Step_length = Step_length, 
        Time_steps = Time_steps,
        lambda_bounds = lambda_bounds, 
        mu_shape = mu_shape, 
        margin = margin, 
        use_temporal_smoothing_constraint = use_temporal_smoothing_constraint,
        mu_temporal_smoothness = mu_temporal_smoothness
        )
      
      # Check if solution is valid and successful
      if(!is.null(sol) && sol$status == 0) { 
        cat(" => Found solution. Obj = ", sol$objectiveVal, "\n")
        all_solutions[[length(all_solutions)+1]] <- data.frame(
          Region= rg,
          Scenario= scn,
          objective= sol$objectiveVal
        )
        
        # Extract final areas from the solution
        dfA <- sol$final_areas

        # Add the 'chosen_shape' information and the region and scenario identifiers
        dfA <- dfA %>%
          left_join(sol$shape_map, by="LULC") %>%
          mutate(Region = rg, Scenario = scn)
        
        # Add the areas data frame to the list
        df_areas_all[[length(df_areas_all)+1]] <- dfA
        
        # Extract transitions from the solution
        dfT <- sol$transitions
        
        # Add region and scenario identifiers to the transitions data frame
        dfT <- dfT %>%
          mutate(Region = rg, Scenario = scn)
        
        # Add the transitions data frame to the list
        df_trans_all[[length(df_trans_all)+1]] <- dfT
        } else { # else report invalid solution
        cat(" => Infeasible or no solution found for Region=", rg, " Scenario=", scn, ".\n")
      } # close statement checking solution validity
    } # close loop over regions
  } # close loop over scenarios
  
  # rbind each list
  Solution_summary <- do.call(rbind, all_solutions) # Summary of objective values
  Area_extraps <- do.call(rbind, df_areas_all)   # All LULC area projections
  Trans_extraps <- do.call(rbind, df_trans_all)   # All transition extrapolations
  
  # Pivot area data to wide
  Area_extraps_wide <-  Area_extraps %>%
  select(Scenario, Region, LULC, chosen_shape, Year, area) %>%
  mutate(Year_col = paste0("Year_", Year)) %>% # Create year column names like "Year_2022"
  select(-Year) %>% # Remove original Year column
  pivot_wider(names_from= Year_col, values_from= area) %>% # Pivot
  arrange(Scenario, Region, LULC)%>% dplyr::rename(shape = chosen_shape) # Rename chosen_shape to shape for output
  
  # Prepare trans data for pivoting to wide
  Trans_extraps <- Trans_extraps %>%
  mutate(interval = paste(YearFrom, YearTo, sep="_")) %>% # Create interval string like "2022_2024"
  select(Scenario, Region, LULC_from, LULC_to, interval, x_value, optimized_rate)
  
  # Seperate the transition areal amounts (x_value) and pivot to wide format
  Trans_extraps_areas <- Trans_extraps %>%
  select(Scenario, Region, LULC_from, LULC_to, interval, x_value) %>%
  pivot_wider(names_from= interval, values_from= x_value) %>%
  arrange(Scenario, Region, LULC_from, LULC_to)
  
  # vector indices of year cols
  year_cols <- colnames(Trans_extraps_areas)[5:ncol(Trans_extraps_areas)]

  # Remove rows where all selected columns are 0
  Trans_extraps_areas <- Trans_extraps_areas[!apply(Trans_extraps_areas[ , year_cols], 1, function(row) all(row == 0)), ]

  # Seperate transition rates (optimized_rate) and pivot to wide format
  Trans_extraps_rates <- Trans_extraps %>%
  select(Scenario, Region, LULC_from, LULC_to, interval, optimized_rate) %>%
  pivot_wider(names_from= interval, values_from= optimized_rate) %>%
  arrange(Scenario, Region, LULC_from, LULC_to)

  # Remove rows where all selected columns are 0
  Trans_extraps_rates <- Trans_extraps_rates[!apply(Trans_extraps_rates[ , year_cols], 1, function(row) all(row == 0)), ]
  
  # Combine outputs into a list
  outputs <- list("Solution_summary" = Solution_summary, 
              "Area_extraps" = Area_extraps_wide,
              "Trans_area_extraps" = Trans_extraps_areas,
              "Trans_rate_extraps" = Trans_extraps_rates
              )
  
  return(outputs)
} # close function


#' @description
#' A wrapper function to solve LULC transitions for multiple future periods.
#' @param lulc_exp_areas A data frame containing LULC classes, their initial
#' areas at the start of the simulation, expected areas at the end of each future time period
#' and the shapes of areal changes curves to be approximated by the solver
#' according to each scenario and region.
#' @param Step_length A numeric vector specifying the length of each time step in years.
#' @param Simulation_start A numeric value indicating the start year of the simulation.
#' @param Period_years A numeric vector specifying the end years of each future period
#' @param trans_rates A data frame containing the min/max transition rates for each Initial and Final LULC class
#' @param lambda_bounds A numeric value specifying the bounds for transition rate penalties.
#' @param mu_shape A numeric value specifying the shape penalty weight.
#' @param margin A numeric value specifying the margin for transition rate bounds.
#' @param use_temporal_smoothing_constraint A logical value indicating whether to apply temporal smoothing constraints.
#' @param mu_temporal_smoothness A numeric value specifying the weight for temporal smoothing constraints.
#' @param output_dir A string specifying the directory where the output files will be saved.

lulcc.solvemultiplefutureperiods <- function(
  lulc_exp_areas = lulc_exp_areas,
  Step_length = Step_length,
  Simulation_start = Simulation_start,
  Period_years = Step_years,
  trans_rates = trans_rates,
  lambda_bounds = 0.1, 
  mu_shape = 15,
  margin = 0.01,
  use_temporal_smoothing_constraint = TRUE,
  mu_temporal_smoothness = 100,
  output_dir
){
  
  # generate vectors of the Time_steps for each of the Period years using Simulation_start and Step_length
  Future_trans_periods <- list()
 
  # loop over the Period_years vector to create vector of time steps for each period
  for(i in 1:length(Period_years)){
   
    # if the Period_end is the 1st in the Period_years vector, then use the Simulation_start as the Period_start
    if(i == 1){
      Period_start <- Simulation_start
      Period_seq <- seq(Period_start, Period_years[i], Step_length)
    } else {
      # use the previous entry in Period_years as the Period start
      Period_start <- Period_years[i-1]
      Period_seq <- seq(Period_start, Period_years[i], Step_length)
    }
   
   # add the Period_seq to the Future_trans_period list
   Future_trans_periods[[paste0(Period_start, "-", Period_years[i])]] <- Period_seq
 }
 
  # Loop over Future_trans_periods list
  Multiperiod_extraps <- lapply(1:length(Future_trans_periods), function(x) {
   
    # Get the current period's time steps
    current_period_steps <- Future_trans_periods[[x]]
   
    # Get period name
    period_name <- names(Future_trans_periods)[[x]]
  
    # get end year of current period
    period_end_year <- current_period_steps[length(current_period_steps)]
  
    # get start year of current period
    period_start_year <- current_period_steps[1]
  
    # if period start year is the same as the Simulation_start, then the init_area
    # does not need to be changed but if it is different then replace the init_area
    # column with the column that contains the period_start_year
    if(period_start_year != Simulation_start){
      lulc_exp_areas$init_area <- lulc_exp_areas[[paste0("final_area_", period_start_year)]]
    }
  
    # Subset the lulc_exp_areas for the current period
    # identify column in lulc_exp_areas that contains the end year
    lulc_exp_areas$final_area <- lulc_exp_areas[[paste0("final_area_", period_end_year)]]
  
    # remove the other final_area_ columns
    lulc_exp_areas_period <- lulc_exp_areas %>%
      select(-starts_with("final_area_")) %>%
      mutate(Period = period_name) # Add a column for the period name
  
    # run the wrapper function looping over the scenarios and regions
    Scenario_region_results <- lulcc.solvescenariosregions(
      lulc_exp_areas = lulc_exp_areas_period,
      region_col = "Region",
      scenario_col = "Scenario",
      trans_rates = viable_trans_rates,
      Step_length = Step_length,
      Time_steps = current_period_steps,
      lambda_bounds = 0.1,
      mu_shape = 15,
      margin = 0.01,
      use_temporal_smoothing_constraint = TRUE,
      mu_temporal_smoothness = 100
    )
    return(Scenario_region_results)
  })
  names(Multiperiod_extraps) <- names(Future_trans_periods)

  # get names of the 2nd level list items
  obj_names <- names(Multiperiod_extraps[[1]])
 
  # Initialize an empty list to store combined data frames
  Combined_extraps <- list()

  # Loop over the names of the 2nd level list items
  for(name in obj_names) {
  # Get the data frames for the current name
  dfs <- lapply(Multiperiod_extraps, function(x) x[[name]])
  
  #if the object name is "Solution_summary", then rbind
  if(name == "Solution_summary") {
    dfs <- lapply(1:length(dfs), function(i) {
      df <- dfs[[i]]
      df$Period <- names(dfs)[[i]] # Add Period column
      return(df)
    })
    combined_df <- do.call(rbind, dfs) # Combine all data frames into one
  } else {
    
    # Identify the non-Year_ columns as those that do not contain any 4 digit numerics in the names
    non_year_cols <- c("Region", "Scenario", "LULC", "shape", "Period", "LULC_from", "LULC_to")
    
    # subset to those found in this data frame
    non_year_cols <- non_year_cols[non_year_cols %in% colnames(dfs[[1]])]
    
    # Combine all data frames into one by matching on the non-Year_ columns
    combined_df <- Reduce(function(x, y) {
      merge(x, y, by=non_year_cols, all=TRUE)
    }, dfs)
    
    # because ther eis overlap in the start and prediction years we need to
    #take an average of the values for columns that have ".x" and ".y" suffixes
    duplicate_cols <- grep("\\.x$|\\.y$", colnames(combined_df), value=TRUE)
    if(length(duplicate_cols) == 0) {
      # No duplicate columns, just return the combined_df
    } else {
        # extract 4 diggit numeric from names of duplicate columns
        dup_year <- unique(str_extract(duplicate_cols, "\\d{4}"))
        
        # Create a new column name without the suffix
        combined_df[paste0("Year_", dup_year)] <- rowMeans(combined_df[duplicate_cols], na.rm=TRUE)
        
        # Remove the original duplicate columns
        combined_df <- combined_df[ , !colnames(combined_df) %in% duplicate_cols]
    }
  }
  
  # Assign the combined data frame back to the list
  Combined_extraps[[name]] <- combined_df
}

  # save each object in the Combined_extraps list to a CSV file
  write.csv(
    Combined_extraps$Solution_summary,
    file = file.path(output_dir, "Transition_solver_solution_summary.csv"),
    row.names = FALSE
  )

  # Save the Area extrapolations to a CSV file
  write.csv(
    Combined_extraps$Area_extraps,
    file = file.path(output_dir, "Extrapolated_LULC_areas.csv"),
    row.names = FALSE
  )

  # Remove any rows where the LULC_from and LULC_to have the same value
  Combined_extraps$Trans_area_extraps <- Combined_extraps$Trans_area_extraps[Combined_extraps$Trans_area_extraps$LULC_from != Combined_extraps$Trans_area_extraps$LULC_to, ]
  Combined_extraps$Trans_rate_extraps <- Combined_extraps$Trans_rate_extraps[Combined_extraps$Trans_rate_extraps$LULC_from != Combined_extraps$Trans_rate_extraps$LULC_to, ]
  
  # Remove the rows where any values are 0, negative or NA
  # identify rows containing either 0s, NAs or negative values
  NA_rows <- apply(Combined_extraps$Trans_area_extraps, 1, function(row) {
    any(is.na(row))
  })  
  
  # Remove rows with NAs
  Combined_extraps$Trans_area_extraps <- Combined_extraps$Trans_area_extraps[!NA_rows, ]
  Combined_extraps$Trans_rate_extraps <- Combined_extraps$Trans_rate_extraps[!NA_rows, ]
    
  # Identify rows where all values are 0 or negative
  Neg_zero_rows <- rowSums(Combined_extraps$Trans_area_extraps[, -c(1:4)]) <= 0

  # Remove rows with all values 0 or negative
  Combined_extraps$Trans_area_extraps <- Combined_extraps$Trans_area_extraps[!Neg_zero_rows, ]
  Combined_extraps$Trans_rate_extraps <- Combined_extraps$Trans_rate_extraps[!Neg_zero_rows, ]
  
  # Save the Transition areas extrapolations to a CSV file
  write.csv(
    Combined_extraps$Trans_area_extraps,
    file = file.path(output_dir, "Extrapolated_LULC_transition_areas.csv"),
    row.names = FALSE
  )
  
  # Save the Transition rates extrapolations to a CSV file
  write.csv(
    Combined_extraps$Trans_rate_extraps,
    file = file.path(output_dir, "Extrapolated_LULC_transition_rates.csv"),
    row.names = FALSE
  )

  # pivot the Area_extraps back to long
  Combined_Area_extraps_long <- Combined_extraps$Area_extraps %>%
    pivot_longer(
      cols = starts_with("Year_"), # Columns that start with "Year_"
      names_to = "Year",            # New column for year identifiers
      values_to = "area"            # New column for area values
    ) %>%
    mutate(Year = str_remove(Year, "^Year_"), # Clean "Year_XXXX" to "XXXX"
           Year = as.numeric(Year))            # Convert Year to numeric


  # Plot line charts of the areas, transitions areas and transitions rates
  Areal_change_plot <- ggplot2::ggplot(Combined_Area_extraps_long, aes(x=Year, y=area, color=LULC)) +
    geom_line() +
    facet_wrap(~ Scenario) +
    labs(title="LULC Areas Over Time", x="Year", y="Area") +
    theme_minimal()
  
  # save the plot as png in the output directory
  ggsave(
    filename = file.path(output_dir, "Extrapolated_LULC_area_change.png"),
    plot = Areal_change_plot,
    width = 10, height = 6
  )

}




