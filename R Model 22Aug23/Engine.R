# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name Engine.R
#' @description Model Engine
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------

ifcollapse <- TRUE
shiftedOutcomes <- TRUE

# ---------- Declare Results Dataframes ----------
# Declare column names
cancer_stage_colnames <- paste0('Cancer',rep(1:n_cancer, rep(4,n_cancer)),rep(c('Stage1','Stage2','Stage3','Stage4'),n_cancer))
markov_trace_colnames <- c('No_Cancer','Deaths',cancer_stage_colnames)
stage_shift_markov_trace_colnames <- c()
for (i in 1:n_cancer){for (j in 1:4){for (k in 1:4){stage_shift_markov_trace_colnames <- c(stage_shift_markov_trace_colnames, paste0('stage ', k, ' to ', j))}}}

# Declare SoC results dataframes
res_soc_LYs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_soc_LYs) <- cancer_stage_colnames
res_soc_QALYs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_soc_QALYs) <- cancer_stage_colnames
res_soc_Costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_soc_Costs) <- cancer_stage_colnames
res_soc_Deaths <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_soc_Deaths) <- cancer_stage_colnames
res_soc_societal_costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_soc_societal_costs) <- cancer_stage_colnames

# Declare MCED results dataframes
res_MCED_LYs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_MCED_LYs) <- cancer_stage_colnames
res_MCED_QALYs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_MCED_QALYs) <- cancer_stage_colnames
res_MCED_Costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_MCED_Costs) <- cancer_stage_colnames
res_MCED_Deaths <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_MCED_Deaths) <- cancer_stage_colnames
res_MCED_societal_costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_MCED_societal_costs) <- cancer_stage_colnames

# Declare MCED time shifted results dataframes
res_MCED_LYs_TS <- res_MCED_LYs
res_MCED_QALYs_TS <- res_MCED_QALYs
res_MCED_Costs_TS <- res_MCED_Costs

# Declare overdiagnosis results dataframes
res_overdiag_QALYs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_overdiag_QALYs) <- cancer_stage_colnames
res_overdiag_costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_overdiag_costs) <- cancer_stage_colnames
res_overdiag_screen_costs <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer)
colnames(res_overdiag_screen_costs) <- cancer_stage_colnames

# ---------- SoC Engine Calculations ----------
soc_markov_trace <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer+2)
colnames(soc_markov_trace) <- markov_trace_colnames

last_col <- ncol(soc_markov_trace)
no_cancer <- rep(cohort_size, 4*n_cancer)

for(i in 1:n_cycle)
{
  if(model_start_age+i-1>=100 || i>n_cycle){
    soc_markov_trace[i,1:last_col] <- 0 # If the age goes above 100, no incidence
  } else {
    soc_markov_trace[i,3:last_col] <- no_cancer*incidence[,idx_age_band[i]] # else, calculate incidence
  }
  
  soc_markov_trace[i,1] <- no_cancer[1] - sum(soc_markov_trace[i,3:last_col]) # Update number with no_cancer after subtracting incidence
  
  soc_markov_trace[i,2] <- soc_markov_trace[i,1]*mortality[i]  # Update number of deaths
  if((model_start_age+i-1)==100){ soc_markov_trace[i,2] <- soc_markov_trace[i-1,1]} # at age 100, death = no cancer patients
  
  soc_markov_trace[i,1] <- soc_markov_trace[i,1]-soc_markov_trace[i,2] # Update number with no_cancer column post deaths
  
  no_cancer <- rep(soc_markov_trace[i,1],4*n_cancer)
}

#----------- SoC Post-diagnosis Calculations -----------
for(iCycle in 1:n_cycle){
  age_col <- idx_age_band[iCycle] # Set the age column index for getting survival
  for(iCancer in 1:n_cancer){
    if (incl_cancer[iCancer]){
      cancer_idx <- (iCancer-1)*4 # Set the cancer column index for looping
      for (iStage in 1:4){
        stage_survival <- min(survival[cancer_idx+iStage, age_col], n_cycle - iCycle) #limit survival to maximum age of 100
        deaths <- deaths_5y[cancer_idx+iStage, age_col]
        stage_iatrogenic <- prop_iatrogenic[cancer_idx+iStage, age_col]
        
        # Record LYs
        unit_array <- c(1,1,1,1,1,1)
        res_soc_LYs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
          calc_res_post_diag(stage_survival, iCycle-1, stage_iatrogenic, unit_array, disc_health, FALSE)
        
        # Record QALYs
        unit_array <- as.numeric(util_cancer[cancer_idx+iStage,])
        res_soc_QALYs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
          calc_post_diag_util(stage_survival, iCycle-1, stage_iatrogenic, unit_array, disc_health, FALSE)
        
        # Record costs
        unit_array <- as.numeric(costs_cancer_tx[cancer_idx+iStage,])
        unit_array <- unit_array * ((1+cost_growth_rate)^(iCycle - 1))
        res_soc_Costs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
          calc_res_post_diag(stage_survival, iCycle-1, stage_iatrogenic, unit_array, disc_cost, FALSE)
        
        # Record Societal costs
        unit_array <- as.numeric(rep(costs_societal[iCancer,iStage],6))
        res_soc_societal_costs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
          calc_res_post_diag(stage_survival, iCycle-1, stage_iatrogenic, unit_array, disc_cost, FALSE)
        
        # Record Deaths
        res_soc_Deaths[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*deaths
      }
    }
  }
}

# ---------- MCED Engine Calculations ----------
stage_shift_markov_trace <- matrix(data = 0, nrow = n_cycle, ncol = 4*4*n_cancer)
colnames(stage_shift_markov_trace) <- stage_shift_markov_trace_colnames
rownames(stage_shift_markov_trace) <- cycle_array

# ---- Stage Shift Calculations ----
for(iCancer in 1:n_cancer){
  if (incl_cancer[iCancer]){
    for(iCycle in 1:n_cycle){
      if((model_start_age + iCycle -1 <= mced_screen_max_age+20) && (model_start_age + iCycle -1 >= mced_screen_min_age)){
        stage_shift_matrix <- as.matrix(stage_shift[((iCancer-1)*4+1):((iCancer-1)*4+4),1:4]) # get stage shift matrix
        misdiag_accuracy <- accuracy[iCancer] # get accuracy
        
        # multiply stage shift matrix
        temp_df <- rep(as.matrix(soc_markov_trace[iCycle,(2+(iCancer-1)*4+1):(2+iCancer*4)]))*c(
          c(stage_shift_matrix[1,1], # stay in stage I
            stage_shift_matrix[1,2:4]*(rep(misdiag_accuracy,3)+rep(1-misdiag_accuracy,3)*perc_misdiagnosis)), # shift to stage I
          
          c(stage_shift_matrix[2,1],
            stage_shift_matrix[2,2]+
              stage_shift_matrix[1,2]*(1-misdiag_accuracy)*(1-perc_misdiagnosis),  # stay in stage II
            (stage_shift_matrix[2,3:4]+
               stage_shift_matrix[1,3:4]*rep(1-misdiag_accuracy,2)*(1-perc_misdiagnosis))*
              (rep(misdiag_accuracy,2)+rep(1-misdiag_accuracy,2)*perc_misdiagnosis)),  # shift to stage II
          
          c(stage_shift_matrix[3,1:2],
            stage_shift_matrix[3,3]+
              (stage_shift_matrix[2,3]+
                 stage_shift_matrix[1,3]*(1-misdiag_accuracy))*(1-misdiag_accuracy)*(1-perc_misdiagnosis),  # stay in stage III
            (stage_shift_matrix[3,4]+(stage_shift_matrix[2,3]+stage_shift_matrix[1,3]*(1-misdiag_accuracy)*(1-perc_misdiagnosis))
             *(1-misdiag_accuracy)*(1-perc_misdiagnosis))*(misdiag_accuracy+(1-misdiag_accuracy)*perc_misdiagnosis)), # shift to stage III
          
          c(stage_shift_matrix[4,1:3],
            stage_shift_matrix[4,4]+
              (stage_shift_matrix[3,4]+
                 (stage_shift_matrix[2,3]*(1-misdiag_accuracy*(1-perc_misdiagnosis))+
                    stage_shift_matrix[1,3])*(1-misdiag_accuracy)*(1-perc_misdiagnosis))*(1-misdiag_accuracy)*(1-perc_misdiagnosis))  # stay in stage IV
        )
      }else{
        temp_df <- soc_markov_trace[iCycle,(2+(iCancer-1)*4+1):(2+iCancer*4)] * c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
      }
      stage_shift_markov_trace[iCycle, ((iCancer-1)*4*4+1):((iCancer-1)*4*4+16)] <- temp_df
    }
  }
}

# ---- Misdiagnosis ----
misdiagnosed_trace <- matrix(data = 0, nrow = n_cycle, ncol = n_cancer)

for(iCancer in 1:n_cancer){
  if (incl_cancer[iCancer]){
    for(iCycle in 1:n_cycle){
      if((model_start_age + iCycle -1 <= mced_screen_max_age+20) && (model_start_age + iCycle -1 >= mced_screen_min_age)){
        stage_shift_matrix <- as.matrix(stage_shift[((iCancer-1)*4+1):((iCancer-1)*4+4),1:4]) # get stage shift matrix
        misdiag_accuracy <- accuracy[iCancer] # get accuracy
        
        # Count number of misdiagnosis
        misdiagnosed_trace[iCycle, iCancer] <-
          sum(
            rep(soc_markov_trace[iCycle,(2+(iCancer-1)*4+1):(2+iCancer*4)])*c(
              c(0, # stay in stage I
                stage_shift_matrix[1,2:4]*rep(1-misdiag_accuracy,3)), # shift to stage I
              c(stage_shift_matrix[2,1],
                0,  # stay in stage II
                (stage_shift_matrix[2,3:4]+stage_shift_matrix[1,3:4]*rep(1-misdiag_accuracy,2))*rep(1-misdiag_accuracy,2)),  # shift to stage II
              c(stage_shift_matrix[3,1:2],
                0,  # stay in stage III
                (stage_shift_matrix[3,4]+(stage_shift_matrix[2,3]+stage_shift_matrix[1,3]*(1-misdiag_accuracy))*(1-misdiag_accuracy))*(1-misdiag_accuracy)), # shift to stage III
              c(stage_shift_matrix[4,1:3],
                0)  # stay in stage IV
            )
          )
      }
    }
  }
}

# ---- Time Shift and Post-diagnoses Calculations ----
time_shift_markov_trace <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer+2)
colnames(time_shift_markov_trace) <- markov_trace_colnames
time_shift_markov_trace_TS <- time_shift_markov_trace    # for tracing time shifted individuals

for(iCycle in 1:n_cycle){
  for (iCancer in 1:n_cancer){
    if (incl_cancer[iCancer]){
      #-----------To Stage IV
      iStage <- 4
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # IV to IV (no shift)
      time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
      # Calc post-diag outcomes
      calc_post_diag_res_MCED(iCycle, iCancer, iStage, 0, time_shift_markov_trace[iCycle,col_time_idx], res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, iCycle, FALSE)
      #-----------To Stage III
      iStage <- 3
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # III to III (no shift)
      time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
      # Calc post-diag outcomes
      calc_post_diag_res_MCED(iCycle, iCancer, iStage, 0, time_shift_markov_trace[iCycle,col_time_idx], res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, iCycle, FALSE)
      # IV to III
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20) {  # max time shift is 20 years
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j # time shifted year
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year < (mced_screen_min_age-model_start_age+1)) { # time-shifted year <= 1, Collapse
            # In case of collapse, time to collapse is the time till current cycle
            time_to_collapse <- iCycle-1
            
            # record mean time shift times
            mean_time_shift_4to3 <- mean_time_shift_array[(col_stage_idx+1)]
            # calculate probability of being time shifted
            prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)   # probability of not being shifted, and should be moved back
            perc1 <- (1-prob4to3) # proportion that needs to be shifted to stage 3
            if (time_to_collapse == 0) perc2 <- 1 else perc2 <- (prob4to3)  # proportion that needs to be shifted back to stage 4
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
            }
            
            # Calculate patient count in cycle 1
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
            
            # Calculate post-diagnosis results
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{  # no collapse
            if(calc_year+model_start_age-1 <= mced_screen_max_age){ # time shift falls within max screening age
              # Time shift, calculate patient count
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted

              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{  # time shift falls outside max screening age
              # No time shift, move patients back to original stage
              time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
      
      #-----------To Stage II
      iStage <- 2
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # II to II (no shift)
      time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
      # Calc post-diag outcomes
      calc_post_diag_res_MCED(iCycle, iCancer, iStage, 0, time_shift_markov_trace[iCycle,col_time_idx], res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, iCycle, FALSE)
      # III to II
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20){  # max time shift is 20 years
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j # time shifted year
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) { # time-shifted year <= 1, Collapse
            # In case of collapse, time to collapse is the time till current cycle
            time_to_collapse <- iCycle-1
            
            mean_time_shift_3to2 <- mean_time_shift_array[(col_stage_idx+1)]
            prob3to2<-exp(-time_to_collapse/mean_time_shift_3to2)   # probability of not being shifted, and should be moved back
            perc1 <- (1-prob3to2) # proportion that needs to be shifted to stage 2
            if (time_to_collapse == 0) perc2 <- 1 else perc2 <- (prob3to2)  # proportion that needs to be shifted back to stage 3
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
            }
            
            # Calculate patient count in cycle 1
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
            
            # Calculate post-diagnosis results
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{  # no collapse
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Time shift, calculate patient count
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
              
              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{  # time shift falls outside max screening age
              # No time shift, move patients back to original stage
              time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
      
      # IV to II
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*timeShift4to2[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            time_to_collapse <- iCycle-1
            
            mean_time_shift_4to2 <- mean_time_shift_array[(col_stage_idx+2)]
            mean_time_shift_4to3 <- mean_time_shift_array[(col_stage_idx+2+4)]
            prob4to2<-exp(-time_to_collapse/mean_time_shift_4to2)
            prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)
            perc1 <- (1-prob4to2) # proportion that needs to be shifted to stage 2
            perc2 <- (prob4to2-prob4to3) # proportion that needs to be shifted back to stage 3
            if (time_to_collapse == 0) perc3 <- 1 else perc3 <- (prob4to3)  # proportion that needs to be shifted back to stage 4
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
              perc3 <- 0
            }
            
            # Calculate patient count in cycle 1
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1 # Update pts shifting to stage II
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2 # Update pts shifting to stage III
            time_shift_markov_trace[1, col_time_idx+2] <- time_shift_markov_trace[1, col_time_idx+2] + n_shifted*perc3 # Update pts shifting to stage IV
            
            # Calculate post-diagnosis results
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+2, 0, n_shifted*perc3, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1 # Update pts shifting to stage II
              time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2 # Update pts shifting to stage III
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Time shift, calculate patient count
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
              
              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{
              # No time shift, move patients back to original stage
              time_shift_markov_trace[iCycle, col_time_idx+2] <- time_shift_markov_trace[iCycle, col_time_idx+2] + n_shifted
              # Calculate post-diagnosis results
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+2, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
      
      #-----------To Stage I
      iStage <- 1
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # I to I (no shift)
      time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
      # Calc post-diag outcomes
      calc_post_diag_res_MCED(iCycle, iCancer, iStage, 0, time_shift_markov_trace[iCycle,col_time_idx], res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, iCycle, FALSE)
      # II to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            time_to_collapse <- iCycle-1
            
            mean_time_shift_2to1 <- mean_time_shift_array[col_stage_idx+1]
            
            prob2to1<-exp(-time_to_collapse/mean_time_shift_2to1)   # probability of not being shifted, and should be moved back
            perc1 <- (1-prob2to1) # proportion that needs to be shifted to stage 1
            if (time_to_collapse == 0) perc2 <- 1 else perc2 <- (prob2to1)  # proportion that needs to be shifted back to stage 2
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
            }
            
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
            
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
              
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{
              time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
      # III to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*timeShift3to1[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            time_to_collapse <- iCycle-1
            
            mean_time_shift_3to1 <- mean_time_shift_array[col_stage_idx+2]
            mean_time_shift_3to2 <- mean_time_shift_array[(col_stage_idx+2+4)]
            prob3to1<-exp(-time_to_collapse/mean_time_shift_3to1)   # probability of not being shifted, and should be moved back
            prob3to2<-exp(-time_to_collapse/mean_time_shift_3to2)
            perc1 <- (1-prob3to1) # proportion that needs to be shifted to stage 1
            perc2 <- (prob3to1-prob3to2) # proportion that needs to be shifted back to stage 2
            if (time_to_collapse == 0) perc3 <- 1 else perc3 <- (prob3to2) # proportion that needs to be shifted back to stage 3
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
              perc3 <- 0
            }
            
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
            time_shift_markov_trace[1, col_time_idx+2] <- time_shift_markov_trace[1, col_time_idx+2] + n_shifted*perc3
            
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+2, 0, n_shifted*perc3, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
              time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
              
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{
              time_shift_markov_trace[iCycle, col_time_idx+2] <- time_shift_markov_trace[iCycle, col_time_idx+2] + n_shifted
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+2, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
      # IV to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+3], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+3]*timeShift4to1[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            time_to_collapse <- iCycle-1
            
            mean_time_shift_4to1 <- mean_time_shift_array[col_stage_idx+3]
            mean_time_shift_4to2 <- mean_time_shift_array[(col_stage_idx+3+4)]
            mean_time_shift_4to3 <- mean_time_shift_array[(col_stage_idx+3+8)]
            prob4to1<-exp(-time_to_collapse/mean_time_shift_4to1)   # probability of not being shifted, and should be moved back
            prob4to2<-exp(-time_to_collapse/mean_time_shift_4to2)
            prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)
            perc1 <- (1-prob4to1) # proportion that needs to be shifted to stage 1
            perc2 <- (prob4to1-prob4to2) # proportion that needs to be shifted back to stage 2
            perc3 <- (prob4to2-prob4to3) # proportion that needs to be shifted back to stage 3
            if (time_to_collapse == 0) perc4 <- 1 else perc4 <- (prob4to3)
            
            if (!ifcollapse) {
              perc1 <- 1
              perc2 <- 0
              perc3 <- 0
              perc4 <- 0
            }
            
            time_shift_markov_trace[1, col_time_idx] <- time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
            time_shift_markov_trace[1, col_time_idx+1] <- time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
            time_shift_markov_trace[1, col_time_idx+2] <- time_shift_markov_trace[1, col_time_idx+2] + n_shifted*perc3
            time_shift_markov_trace[1, col_time_idx+3] <- time_shift_markov_trace[1, col_time_idx+3] + n_shifted*perc4
            
            calc_post_diag_res_MCED(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+2, iCycle - 1, n_shifted*perc3, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            calc_post_diag_res_MCED(iCycle, iCancer, iStage+3, 0, n_shifted*perc4, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, 1, FALSE)
            
            if (shiftedOutcomes) {
              time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
              time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2
              time_shift_markov_trace_TS[1, col_time_idx+2] <- time_shift_markov_trace_TS[1, col_time_idx+2] + n_shifted*perc3
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, iCycle - 1, n_shifted*perc1, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage+1, iCycle - 1, n_shifted*perc2, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
              calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage+2, iCycle - 1, n_shifted*perc3, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, 1, FALSE)
            }
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
              
              calc_post_diag_res_MCED(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, FALSE)
              
              if (shiftedOutcomes) {
                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                calc_post_diag_res_MCED_TS(iCycle, iCancer, iStage, j, n_shifted, res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS, calc_year, FALSE)
              }
            }else{
              time_shift_markov_trace[iCycle, col_time_idx+3] <- time_shift_markov_trace[iCycle, col_time_idx+3] + n_shifted
              calc_post_diag_res_MCED(iCycle, iCancer, iStage+3, 0, n_shifted, res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths, calc_year, TRUE)
            }
          }
        }
      }
    }
  }
}

# ---- Repeat Time Shift for Misdiagnosis Adjustment ----
misdiag_adjustment <- matrix(data = 0, nrow = n_cycle, ncol = n_cancer)
for(iCycle in 1:n_cycle){
  for (iCancer in 1:n_cancer){
    if (incl_cancer[iCancer]){
      #-----------To Stage IV
      #-----------To Stage III
      iStage <- 3
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      # IV to III
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20) {  # max time shift is 20 years
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j # time shifted year
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year < (mced_screen_min_age-model_start_age+1)) { # time-shifted year <= 1, Collapse
            # Do nothing
          }else{  # no collapse
            if(calc_year+model_start_age-1 <= mced_screen_max_age){ # time shift falls within max screening age
              # Do nothing
            }else{  # time shift falls outside max screening age
              # Record un-shifted patients for misdiagnosis adjustment
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
      
      #-----------To Stage II
      iStage <- 2
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      # III to II
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20){  # max time shift is 20 years
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j # time shifted year
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) { # time-shifted year <= 1, Collapse
            # Do nothing
          }else{  # no collapse
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Do nothing
            }else{  # time shift falls outside max screening age
              # Record un-shifted patients for misdiagnosis adjustment
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
      
      # IV to II
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*timeShift4to2[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            # Do nothing
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Do nothing
            }else{
              # Record un-shifted patients for misdiagnosis adjustment
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
      
      #-----------To Stage I
      iStage <- 1
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      # II to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*timeShiftDist[tDist_idx,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            # Do nothing
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Do nothing
            }else{
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
      # III to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*timeShift3to1[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            # Do nothing
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Do nothing
            }else{
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
      # IV to I
      tDist_idx <- match(mean_time_shift_array[col_stage_idx+3], timeShiftDistMean)
      for(j in 1:20){
        if (timeShiftDist[tDist_idx,j] != 0) {
          calc_year <- iCycle - j
          n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+3]*timeShift4to1[iCancer,j]
          if (calc_year <= (mced_screen_min_age-model_start_age+1)) {
            # Do nothing
          }else{
            if(calc_year+model_start_age-1 <= mced_screen_max_age){
              # Do nothing
            }else{
              misdiag_adjustment[iCycle, iCancer] <- misdiag_adjustment[iCycle, iCancer] + n_shifted
            }
          }
        }
      }
    }
  }
}

# ---- Update Deaths and No-cancer ----
# Assign deaths from Soc markov trace to keep deaths equal
time_shift_markov_trace[,2] <- soc_markov_trace[,2]
# Calculate no-cancer column
for(iCycle in 1:n_cycle){
  if (iCycle == 1){
    time_shift_markov_trace[iCycle, 1] <- cohort_size - sum(time_shift_markov_trace[iCycle, 2:last_col])
  } else {
    time_shift_markov_trace[iCycle, 1] <- 
      time_shift_markov_trace[iCycle-1, 1] - sum(time_shift_markov_trace[iCycle, 3:last_col]) - time_shift_markov_trace[iCycle, 2]
  }
}

#----------- Over-diagnosis Calculations -----------
overdiagnosis_stage_shift_markov_trace <- matrix(data = 0, nrow = n_cycle, ncol = 4*4*n_cancer)
overdiagnosis_markov <- matrix(data = 0, nrow=n_cycle, ncol=4*n_cancer)
overdiagnosed_misdiagnosed_trace <- matrix(data = 0, nrow = n_cycle, ncol = n_cancer)
dead_stage <-  matrix(data = 0, nrow = 4, ncol = 1)

# ---- OD Stage Shift Calculations ----
for(iCancer in 1:n_cancer){
  if (incl_cancer[iCancer]){
    for(iCycle in 1:n_cycle){
      if(model_start_age+iCycle-1 <= mced_screen_max_age+20 && model_start_age+iCycle-1 >= mced_screen_min_age){
        # Calculate OD count
        n_dead <- soc_markov_trace[iCycle,2]*perc_overdiagnosis
        sum_incidence_all <- sum(soc_markov_trace[iCycle,3:ncol(soc_markov_trace)])
        sum_incidence_cancer <- sum(soc_markov_trace[iCycle, (2+(iCancer-1)*4+1):(2+(iCancer-1)*4+overdiagnosis_end_stage)])
        for (iStage in 1:overdiagnosis_end_stage){
          dead_stage[iStage] <- n_dead*soc_markov_trace[iCycle, (2+(iCancer-1)*4+iStage)]/sum_incidence_all
        }
        
        stage_shift_matrix <- as.matrix(stage_shift[((iCancer-1)*4+1):((iCancer-1)*4+4),1:4]) # get stage shift matrix
        misdiag_accuracy <- accuracy[iCancer] # get accuracy
        
        # Multiply stage shift matrix
        temp_df <- rep(as.matrix(dead_stage[1:4]))*c(
          c(stage_shift_matrix[1,1], # stay in stage I
            stage_shift_matrix[1,2:4]*(rep(misdiag_accuracy, 3)+rep(1-misdiag_accuracy,3)*perc_misdiagnosis)), # shift to stage I
          
          c(stage_shift_matrix[2,1], 
            stage_shift_matrix[2,2]+
              stage_shift_matrix[1,2]*(1-misdiag_accuracy)*(1-perc_misdiagnosis),  # stay in stage II
            (stage_shift_matrix[2,3:4]+
               stage_shift_matrix[1,3:4]*rep(1-misdiag_accuracy,2)*(1-perc_misdiagnosis))*
              (rep(misdiag_accuracy,2)+rep(1-misdiag_accuracy,2)*perc_misdiagnosis)),  # shift to stage II
          
          c(stage_shift_matrix[3,1:2],
            stage_shift_matrix[3,3]+
              (stage_shift_matrix[2,3]+
                 stage_shift_matrix[1,3]*(1-misdiag_accuracy))*(1-misdiag_accuracy)*(1-perc_misdiagnosis),  # stay in stage III
            (stage_shift_matrix[3,4]+(stage_shift_matrix[2,3]+stage_shift_matrix[1,3]*(1-misdiag_accuracy)*(1-perc_misdiagnosis))
             *(1-misdiag_accuracy)*(1-perc_misdiagnosis))*(misdiag_accuracy+(1-misdiag_accuracy)*perc_misdiagnosis)), # shift to stage III
          
          c(stage_shift_matrix[4,1:3],
            stage_shift_matrix[4,4]+
              (stage_shift_matrix[3,4]+
                 (stage_shift_matrix[2,3]*(1-misdiag_accuracy*(1-perc_misdiagnosis))+
                    stage_shift_matrix[1,3])*(1-misdiag_accuracy)*(1-perc_misdiagnosis))*(1-misdiag_accuracy)*(1-perc_misdiagnosis))  # stay in stage IV
        )
        
        overdiagnosed_misdiagnosed_trace[iCycle, iCancer] <- 
          sum(
            rep(dead_stage[1:4])*c(
              c(0, # stay in stage I
                stage_shift_matrix[1,2:4]*rep(1-misdiag_accuracy,3)), # shift to stage I
              c(stage_shift_matrix[2,1],
                0,  # stay in stage II
                (stage_shift_matrix[2,3:4]+stage_shift_matrix[1,3:4]*rep(1-misdiag_accuracy,2))*rep(1-misdiag_accuracy,2)),  # shift to stage II
              c(stage_shift_matrix[3,1:2],
                0,  # stay in stage III
                (stage_shift_matrix[3,4]+(stage_shift_matrix[2,3]+stage_shift_matrix[1,3]*(1-misdiag_accuracy))*(1-misdiag_accuracy))*(1-misdiag_accuracy)), # shift to stage III
              c(stage_shift_matrix[4,1:3],
                0)  # stay in stage IV
            )
          )
      } else { # No stage shift
        temp_df <- dead_stage[1:4] * c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
      }
      overdiagnosis_stage_shift_markov_trace[iCycle, ((iCancer-1)*4*4+1):((iCancer-1)*4*4+16)] <- temp_df
    }
  }
}

# Remove unshifted patients
for (iCycle in 1:n_cycle){
  for (iCancer in 1:n_cancer){
    if (incl_cancer[iCancer]){
      overdiagnosis_stage_shift_markov_trace[iCycle, (iCancer-1)*16+1] <- 0 #Stage I->I
      overdiagnosis_stage_shift_markov_trace[iCycle, (iCancer-1)*16+6] <- 0 #Stage II->II
      overdiagnosis_stage_shift_markov_trace[iCycle, (iCancer-1)*16+11] <- 0 #Stage III->III
      overdiagnosis_stage_shift_markov_trace[iCycle, (iCancer-1)*16+16] <- 0 #Stage IV->IV
    }
  }
}

# ---- OD Time Shift Calculations ----
overdiagnosed_time_shift_markov_trace <- matrix(data = 0, nrow = n_cycle, ncol = 4*n_cancer+2)

for(iCycle in 1:n_cycle){
  for (iCancer in 1:n_cancer){
    if (incl_cancer[iCancer]){
      # Stage III
      iStage <- 3
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # IV to III
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+1])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+1]
      # record mean time shift times
      mean_time_shift_4to3 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)   # probability of not being shifted, and should be moved back
        perc1 <- (1-prob4to3) # proportion that needs to be shifted to stage 3

        if (!ifcollapse) {
          perc1 <- 1
        }
        
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to3, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to3, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
      
      # Stage II
      iStage <- 2
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # III to II
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+1])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+1]
      # record mean time shift times
      mean_time_shift_3to2 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob3to2<-exp(-time_to_collapse/mean_time_shift_3to2)   # probability of not being shifted, and should be moved back
        perc1 <- (1-prob3to2) # proportion that needs to be shifted to stage 3
        
        if (!ifcollapse) {
          perc1 <- 1
        }
        
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_3to2, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_3to2, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
      # IV to II
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+2])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+2]
      # record mean time shift times
      mean_time_shift_4to2 <- rnd(mean_time_shift_array_OD[(col_stage_idx+2)])
      mean_time_shift_4to3 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob4to2<-exp(-time_to_collapse/mean_time_shift_4to2)   # probability of not being shifted, and should be moved back
        prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)
        perc1 <- (1-prob4to2) # proportion that needs to be shifted to stage 2
        perc2 <- (prob4to2-prob4to3) # proportion that needs to be shifted back to stage 3
        if (!ifcollapse) {
          perc1 <- 1
          perc2 <- 0
        }
        
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to2, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage+1, mean_time_shift_4to3, 0, n_shifted*perc2, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
        overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to2, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
      
      # Stage I
      iStage <- 1
      col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
      col_time_idx <- 2+(iCancer-1)*4+iStage
      # II to I
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+1])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+1]
      # record mean time shift times
      mean_time_shift_2to1 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob2to1<-exp(-time_to_collapse/mean_time_shift_2to1)   # probability of not being shifted, and should be moved back
        perc1 <- (1-prob2to1) # proportion that needs to be shifted to stage 3
        
        if (!ifcollapse) {
          perc1 <- 1
        }
        
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_2to1, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_2to1, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
      # III to I
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+2])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+2]
      # record mean time shift times
      mean_time_shift_3to1 <- rnd(mean_time_shift_array_OD[(col_stage_idx+2)])
      mean_time_shift_3to2 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob3to1<-exp(-time_to_collapse/mean_time_shift_3to1)   # probability of not being shifted, and should be moved back
        prob3to2<-exp(-time_to_collapse/mean_time_shift_3to2)
        perc1 <- (1-prob3to1) # proportion that needs to be shifted to stage 1
        perc2 <- (prob3to1-prob3to2)
        if (!ifcollapse) {
          perc1 <- 1
          perc2 <- 0
        }
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_3to1, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage+1, mean_time_shift_3to2, 0, n_shifted*perc2, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
        overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_3to1, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
      # IV to I
      calc_year <- rnd(iCycle - mean_time_shift_array_OD[col_stage_idx+3])
      n_shifted <- overdiagnosis_stage_shift_markov_trace[iCycle, col_stage_idx+3]
      # record mean time shift times
      mean_time_shift_4to1 <- rnd(mean_time_shift_array_OD[(col_stage_idx+3)])
      mean_time_shift_4to2 <- rnd(mean_time_shift_array_OD[(col_stage_idx+2)])
      mean_time_shift_4to3 <- rnd(mean_time_shift_array_OD[(col_stage_idx+1)])
      if (calc_year <= (mced_screen_min_age-model_start_age+1)){ # Collapse
        time_to_collapse <- iCycle-1 # time to collapse is the time till current cycle
        
        # calculate probability of patients
        prob4to1<-exp(-time_to_collapse/mean_time_shift_4to1)   # probability of not being shifted, and should be moved back
        prob4to2<-exp(-time_to_collapse/mean_time_shift_4to2)
        prob4to3<-exp(-time_to_collapse/mean_time_shift_4to3)
        perc1 <- (1-prob4to1) # proportion that needs to be shifted to stage 1
        perc2 <- (prob4to1-prob4to2) # proportion that needs to be shifted back to stage 2
        perc3 <- (prob4to2-prob4to3) # proportion that needs to be shifted back to stage 3
        if (!ifcollapse) {
          perc1 <- 1
          perc2 <- 0
          perc3 <- 0
        }
        
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to1, 0, n_shifted*perc1, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage+1, mean_time_shift_4to2, 0, n_shifted*perc2, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        calc_post_diag_res_overdiag(iCycle, iCancer, iStage+2, mean_time_shift_4to3, 0, n_shifted*perc3, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
        overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
        overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
        overdiagnosed_time_shift_markov_trace[1, col_time_idx+2] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+2] + n_shifted*perc3
      }else{
        if(calc_year+model_start_age-1 <= mced_screen_max_age){
          calc_post_diag_res_overdiag(iCycle, iCancer, iStage, mean_time_shift_4to1, 0, n_shifted, res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs)
          overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
        }
      }
    }
  }
}


# ---------- Side Calculations ----------
# Remove excluded cancer results
for (iCancer in 1:n_cancer){
  if (!incl_cancer[iCancer]){
    soc_markov_trace[,((iCancer-1)*4+2+1):((iCancer-1)*4+2+4)] <- 0
    time_shift_markov_trace[,((iCancer-1)*4+2+1):((iCancer-1)*4+2+4)] <- 0
  } 
}

res_soc_soc_screen <- calc_soc_screen_outputs(soc_markov_trace) # calculate SoC screening outputs for SoC arm
res_MCED_soc_screen <- calc_soc_screen_outputs(time_shift_markov_trace) # calculate SoC screening outputs for MCED arm
res_MCED_MCED_screen <- calc_MCED_screen_outputs(time_shift_markov_trace) # calculate MCED screening outputs

# No-cancer diagnosis calculations
res_soc_total_diagnosed <- sum(rowSums(soc_markov_trace[,3:ncol(soc_markov_trace)]))
res_soc_no_diag <- calc_no_diag_health_outcomes(res_soc_total_diagnosed)
res_MCED_total_diagnosed <- sum(rowSums(time_shift_markov_trace[,3:ncol(time_shift_markov_trace)]))
res_MCED_total_diagnosed_TS <- sum(rowSums(time_shift_markov_trace_TS[,3:ncol(time_shift_markov_trace_TS)]))
res_MCED_no_diag <- calc_no_diag_health_outcomes(res_MCED_total_diagnosed)

# Pre-diagnosis calculations
res_soc_pre_diag <- calc_res_pre_diag(soc_markov_trace)
res_MCED_pre_diag <- calc_res_pre_diag(time_shift_markov_trace)

# Disutilities due to FPs
res_soc_disutil_FPs <- sum(res_soc_soc_screen$soc_FPs*disutil_FP*dur_workup_FP*discount_health)/cohort_size
res_MCED_disutil_FPs <- (sum(res_MCED_soc_screen$soc_FPs*disutil_FP*dur_workup_FP*discount_health) +
                           sum(res_MCED_MCED_screen$MCED_FPs*disutil_FP*dur_workup_FP*discount_health))/cohort_size

# Adjust misdiagnosis to account for only shifted patients
misdiagnosis <- matrix(data = 0, nrow = n_cycle, ncol = n_cancer)
for (iCycle in 1:n_cycle) {
  for (iCancer in 1:n_cancer) {
    if (incl_cancer[iCancer]) {
      misdiagnosis[iCycle, iCancer] <- (misdiagnosed_trace[iCycle, iCancer] + overdiagnosed_misdiagnosed_trace[iCycle, iCancer])*(1-misdiag_adjustment[iCycle, iCancer]/sum(stage_shift_markov_trace[iCycle, ((iCancer-1)*16+1):((iCancer-1)*16+16)]))
    }
  }
}
misdiagnosis[is.nan(misdiagnosis)] <- 0

# Disutilities due to misdiagnosis
res_MCED_disutil_misdiagnosis <- sum(rowSums(misdiagnosis)[1:n_cycle]*discount_health*disutil_misdiagnosis*dur_workup_misdiagnosis)/cohort_size

