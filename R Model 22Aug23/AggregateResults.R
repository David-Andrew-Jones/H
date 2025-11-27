# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name AggregateResults.R
#' @description GRAIL Model
#' @version 0.1
#' @author Evidera ? 
# ----------------------------------------------------------------------------------------------------------------------------------------


# ---------- Initialize environment ----------
rm(list = ls())
args<-commandArgs(TRUE)

# Load required libraries
require(tidyr)
require(tidyverse)
require(dplyr)

# Set directory location
individual_results_dir <- paste0(args[1],"/Results/Intermediate Results/")
setwd(args[1])  # CHANGE - Uncomment this line while deploying
# setwd('C:/Local Folder/GRAIL/R conversion/') # CHANGE - Comment this line while deploying

# Set number of outcomes and columns for comparators
res_n_outcomes<- 45
res_n_col <- 2

# Read in age- and cohort- weight arrays
weight_age <- as.data.frame(read_csv(paste0(getwd(),"/Excel2R/weightAge.csv"), col_names = FALSE, show_col_types = FALSE))
weight_cohort <- as.data.frame(read_csv(paste0(getwd(),"/Excel2R/weightCohort.csv"), col_names = FALSE, show_col_types = FALSE))

# ---------- Read in individual results files ----------
for (i in 1:nrow(weight_age)) {
  for (j in 1:ncol(weight_age)){
    
    # create name for variable
    dfName <- paste0("res_",i,"-",j) 
    
    if(weight_age[i,j]>0){
      # Read in results file if weight > 0.0
      assign(dfName,
             read_csv(paste0(getwd(),"/Results/Intermediate Results/results_",i,"-",j,".csv"),col_names = T,col_select = c(2,3))*weight_age[i,j], 
             pos = 1) 
      }else{
        # Else, assign empty dataframe
        assign(dfName, 
               matrix(data = 0, nrow = res_n_outcomes, ncol = res_n_col), 
               pos = 1) 
      }
  }
}

# ---------- Aggregate results based on age-based weights ----------
for( i in 1:nrow(weight_age)){
  dfName <- paste0("res_agg_",i)
  temp_df <- matrix(data = 0, nrow = res_n_outcomes, ncol = res_n_col)
  for ( j in 1:ncol(weight_age)){
    temp_df <- temp_df + (get(paste0("res_",i,"-",j)))
  }
  assign(dfName, temp_df, pos = 1)
}

# Release objects from memory as these are not required anymore
rm(temp_df)
for (i in 1:9){
  rm(list = ls()[grep(paste0("^res_",i), ls())]) 
}

# ---------- Aggregate results based on cohort weights ----------
res_aggregated <- matrix(data = 0, nrow = res_n_outcomes, ncol = res_n_col)
for(i in 1:nrow(weight_cohort)){
  res_aggregated <- res_aggregated + (get(paste0("res_agg_",i)))*weight_cohort[i,1]
}

# Release objects from memory as these are not required anymore
for (i in 1:9){
  rm(list = ls()[grep(paste0("^res_agg_",i), ls())]) 
}

# ---------- Write aggregated results CSV ---------- 
write.csv(res_aggregated,paste0(getwd(),'/Results/results_aggregated.csv'))
