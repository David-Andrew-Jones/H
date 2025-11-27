# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name InterceptModel.R
#' @description Intercept Module
#' @version 1.0
#' @author Evidera 
# ----------------------------------------------------------------------------------------------------------------------------------------

# Load required libraries
require(tidyverse)
require(cowplot)
require(ggalluvial)

#' @name effective_sens
#' @description modify using slip rate
#' intercept using slip rate, slip to next
#' okay: this multiplies a final destination IR: all the people who wind up at some final destination
#' so everything scales within itself
#' assume stage IV destination unless the cases are stopped earlier
#' @returns effective sensitivity used in stage shift calculations
#' @author GRAIL
effective_sens<-function(cumulative_sens, dwell_detect_rate){
  detect<-rep(0,length(cumulative_sens))
  miss<-0
  i<-1
  arrive <- cumulative_sens[i]
  live<-arrive+miss
  detect[1]<-cumulative_sens[i]*dwell_detect_rate[i]
  miss<-cumulative_sens[i]*(1-dwell_detect_rate[i])
  if (length(cumulative_sens)>1){
    for (i in 2:length(cumulative_sens))
    {
      #newly detectable cases
      arrive<-cumulative_sens[i]-cumulative_sens[i-1]
      live<-arrive+miss #currently detectable is newly detectable + missed at earlier stages
      detect[i]<-live*dwell_detect_rate[i] #would detect all of them, but miss some of them due to timing
      miss<-live*(1-dwell_detect_rate[i])
    }
  }
  arrive<-1-cumulative_sens[i] #final miss
  live=arrive+miss
  # print(arrive)
  #detect[1-4] is detected at each stage
  #clinical detect= miss[4]
  list(intercept=detect,clinical=live,
       parrive=arrive,plive=live,
       pdetect=detect,pmiss=miss)
}

#' @name calc_stage_shift_matrix
#' @description Executes InterceptModel calculations to get stage shift matrix 
#' @returns stage shift matrix for all cancers in 4x4 format
#' @author Ameya Chavan, Evidera
calc_stage_shift_matrix<-function(param_dwell_times, param_screen_interval, param_sensitivity, param_incidence){
  # clear environment
  # rm(list = ls())
  
  # cancer names list 
  #TODO: update code to take the names list from Main.R
  cancer_names <- c('lung', 'colon', ' pancreas', ' liver', ' breast_hr_pos', 'esophagus', 'head_and_neck',
                    'stomach', 'ovary', 'kidney', 'prostate', 'breast_hr_neg', 'lymphoma', 'anus',
                    'uterus','bladder', 'cervix' , 'urothelial', 'other')
  
  n_cancer <- length(cancer_names)
  cancer_names <- rep(cancer_names,each=4)
  
  xStage <- rep(c('I','II','III','IV'), times=n_cancer)
  number_stage <- rep(seq(1,4),n_cancer)
  
  #master plot: runs intercept model under various scenarios
  date_code<-"20191002"
  
  
  #--------------------------build_slip_rate_scenario_table.R---------------------
  #generate slip rate for a given scenario
  #export to file as a save
  
  
  #do it tidy style
  #this function estimates the rate when a screening event happens within a 
  # randomly distributed length of time 
  # given the examples tf
  catch_rate_for_k<-function(tf,k){
    tt<-sum(pmin(tf,k))/length(tf)
    tt/k
  }
  
  
  # dwell_times <- read_csv(paste0(excel2R_dir,'dwell_times.csv'), col_names = F, show_col_types = F) #TODO: Update directory location 
  dwell_times <- param_dwell_times
  
  dwell_model_all_df <- cbind(cancer_names, dwell_times, xStage, number_stage)
  names(dwell_model_all_df) <- c('Cancer', 'dwell','Stage', 'number_stage')
  
  # write.csv(dwell_model_all_df, "dwellTimes.csv")
  #monte carlo generate shape of dwell time distribution
  ss=1
  set.seed(19680128)
  tfan<-rweibull(10000,shape=ss,scale=1)
  
  tfan<-tfan/mean(tfan)
  
  screen_interval<-param_screen_interval #initially 1
  
  #slip is "before clinical"
  #slip_clinical = "at stage of clinical detection"
  #completeness in modeling
  dwell_slip_df<-dwell_model_all_df %>%
    mutate(slip = sapply(dwell,function(z){1-catch_rate_for_k(tfan,screen_interval/z)}),
           slip_clinical=sapply(dwell*0.5,function(z){1-catch_rate_for_k(tfan,screen_interval/z)}))
  
  
  #----------------------------------iso_sens_joined------------------------------
  
  # iso_sens_incidence <- read_csv(paste0(excel2R_dir,'incidence.csv'), col_names = F, show_col_types = F)
  iso_sens_incidence <- param_incidence
  iso_sens_incidence_mean <-rowMeans(iso_sens_incidence)
  
  # iso_sensitivity <- read_csv(paste0(excel2R_dir,'sensitivity.csv'), col_names = F, show_col_types = F)
  iso_sensitivity <- param_sensitivity
  iso_sens_joined<- cbind(cancer_names, xStage, number_stage, iso_sensitivity, iso_sens_incidence_mean, iso_sensitivity)
  names(iso_sens_joined) <- c('Cancer', 'Stage', 'number_stage', 'original_sens', 'IR', 'iso_sens')
  
  
  #----------------------------compute_intercept_model.R--------------------------
  
  incidence_sens_source <- (iso_sens_joined)
  
  #set up all previous stages where cases could be intercepted given clinical detection
  incidence_set<-incidence_sens_source %>% 
    filter(Stage %in% xStage) %>%
    select(Cancer,Stage,IR) %>%
    mutate(number_stage=match(Stage,c("I","II","III","IV")),
           clinical=number_stage,
           unroll=number_stage) %>%
    uncount(unroll,.id="prequel")
  
  #just detection rate
  just_detection<-incidence_sens_source %>%
    mutate(number_stage=match(Stage,xStage),
           prequel=number_stage,
           detect=iso_sens) %>%
    select(Cancer,prequel,detect)
  
  #differences - marginal detection rate of remaining cases 
  #given that cases already detectable at earlier stage were removed or treated separately
  just_delta<- just_detection %>%
    group_by(Cancer) %>%
    arrange(prequel,.by_group=TRUE) %>%
    mutate(delta_detect=diff(c(0,detect))) %>%
    ungroup() %>%
    arrange(Cancer)
  
  #include modification of slip rate by clinical stage of detection
  #extra 'parameter'
  just_slip_delta_extra<-just_delta %>%
    left_join(dwell_slip_df %>% select(Cancer,prequel=number_stage,slip,slip_clinical), by = c("Cancer", "prequel")) %>%
    filter(!is.na(prequel) && slip>0) %>%
    mutate(unroll=4) %>%
    uncount(unroll,.id="clinical") %>%
    filter(clinical>=prequel) %>%
    mutate(modified_slip=case_when(prequel<clinical ~ slip,
                                   prequel==clinical ~ slip_clinical,
                                   TRUE ~ 1.0)) %>%
    arrange(Cancer,clinical,prequel) %>%
    group_by(Cancer,clinical) %>%
    mutate(sens_slip=effective_sens(detect,1-modified_slip)$intercept) %>%
    ungroup()
  
  
  just_slip_delta_extra<-just_slip_delta_extra %>% filter(slip>0)
  
  
  #updated: split effective slip rate in 2 for last stage
  #as the "time spent" should be halved
  #this involves a more elaborate model
  #note that "lives saved" is not affected, because those individuals are not stage shifted
  #this assumes that 'stage 4' is just automatically halved anyway
  incidence_intercepted<-incidence_set %>%
    left_join(just_slip_delta_extra, by = c("Cancer", "clinical", "prequel")) %>% 
    mutate(unroll=1+(number_stage==prequel)) %>%
    uncount(unroll,.id="found_clinical") %>%
    group_by(Cancer,clinical) %>%
    mutate(c_slip=cumsum(sens_slip),
           delta_detect=case_when(
             found_clinical==2 ~ 1-c_slip+sens_slip, #anyone not caught by new screening must be found clinically
             TRUE ~ sens_slip)) %>%
    mutate(caught=IR*delta_detect) %>%
    ungroup()
  
  # sort and format rows for stage shift calculations
  incidence_intercepted_shifted <- incidence_intercepted %>% group_by(Cancer, Stage) %>% mutate(shift_pct = caught/sum(caught))
  
  incidence_intercepted_shifted_pct <- incidence_intercepted_shifted %>% 
    group_by(Cancer, Stage, prequel)  %>% 
    mutate(shift_pct2 = sum(shift_pct)) %>%
    filter(found_clinical==1)
  
  # Convert calculated stage shift percentanges to matrix format
  for(iCancer in 1:n_cancer){
    idx=1
    temp_matrix <- matrix(data=rep(0,16), nrow=4, ncol=4)
    for (iStage in 1:4) {
      for (i in 1:iStage){
        temp_matrix[i,iStage] <- incidence_intercepted_shifted_pct$shift_pct2[(iCancer-1)*10+idx]
        idx=idx+1
      }
    }
    if (iCancer==1){
      res_df<-temp_matrix
    }else{
      res_df <- rbind(res_df, temp_matrix)
    }
  }
  
  return(res_df) # resultant dataframe that should be returned. Contains the stage_shift matrix in the correct format
}

#---- Generate Stage Shift Matrix ----
stage_shift_list <- list()
for (i in 1:5){
  stage_shift_list[[i]] <- calc_stage_shift_matrix(param_dwell_times = dwell_times_base_intercept, 
                                                   param_screen_interval = i, 
                                                   param_sensitivity = sensitivity_intercept, 
                                                   param_incidence = incidence_base_intercept)
}
templist <- vector("list", length=n_cancer_base) # stage shift matrix for no compliance
for (i in 1:n_cancer_base){
  templist[[i]] <- diag(1, 4, 4)
}
stage_shift_list[[6]] <- do.call(rbind, templist)
stage_shift_base <- stage_shift_list[[1]]*MCED_compliance[1] +
  stage_shift_list[[2]]*MCED_compliance[2] +
  stage_shift_list[[3]]*MCED_compliance[3] +
  stage_shift_list[[4]]*MCED_compliance[4] +
  stage_shift_list[[5]]*MCED_compliance[5] +
  stage_shift_list[[6]]*MCED_compliance[6]

# Generate adjusted stage shift matrix for patient heterogeneity
if (analysis_type == 2) {
  templist <- vector("list", length=n_cancer_hete) # stage shift matrix for no compliance
  for (i in 1:n_cancer_base){
    curStageShift <- stage_shift_base[(((i-1)*4+1):((i-1)*4+4)),]
    adjStageShift <- stage_shift_base[(((i-1)*4+1):((i-1)*4+4)),]
    
    curStageShift[2, 2] <- max(curStageShift[2, 2] - (1-sensitivity[((i-1)*4+2)]), 0)
    curStageShift[3, 3] <- max(curStageShift[3, 3] - (1-sensitivity[((i-1)*4+3)]), 0)
    curStageShift[4, 4] <- max(curStageShift[4, 4] - (1-sensitivity[((i-1)*4+4)]), 0)
    
    adjStageShift[2, 2] <- curStageShift[2, 2]/sum(curStageShift[1:2, 2])
    adjStageShift[3, 3] <- curStageShift[3, 3]/sum(curStageShift[1:3, 3])
    adjStageShift[4, 4] <- curStageShift[4, 4]/sum(curStageShift[1:4, 4])
    
    adjStageShift[is.nan(data.matrix(adjStageShift))] <- 1
    
    adjStageShift[1, 2] <- curStageShift[1, 2]/sum(curStageShift[1:2, 2])
    adjStageShift[1, 3] <- curStageShift[1, 3]/sum(curStageShift[1:3, 3])
    adjStageShift[1, 4] <- curStageShift[1, 4]/sum(curStageShift[1:4, 4])
    adjStageShift[2, 3] <- curStageShift[2, 3]/sum(curStageShift[1:3, 3])
    adjStageShift[2, 4] <- curStageShift[2, 4]/sum(curStageShift[1:4, 4])
    adjStageShift[3, 4] <- curStageShift[3, 4]/sum(curStageShift[1:4, 4])
    
    adjStageShift[is.nan(data.matrix(adjStageShift))] <- 0
    
    templist[[((i-1)*2+1)]] <- data.frame(adjStageShift)
    templist[[((i-1)*2+2)]] <- data.frame(diag(1, 4, 4))
  }
}
stage_shift_hete <- do.call(rbind, templist)
if (analysis_type == 1){stage_shift <- stage_shift_base} else {stage_shift <- stage_shift_hete}
