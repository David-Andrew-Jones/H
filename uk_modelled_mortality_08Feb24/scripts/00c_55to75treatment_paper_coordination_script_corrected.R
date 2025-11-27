library(tidyverse)
library(readxl)
library(openxlsx)
library(zoo)
library(gt)
library(Iso)

###############################
#Use alternative versions of 07_... scripts to generate alternative corrected resutls
###############################

#version output
date_code<-"29_05_2024_treatment_paper_results"
#define parameters used in the analysis
source("scripts/00_x_global_parameters_for55to75.R")

#useful functions
'%!in%' <- function(x,y)!('%in%'(x,y)) #function to select not

#settings
options(scipen = 100) #remove scientific notation 

#utility functions
source("R/compute_stage_shift.R") #changed to 2, included prevalence round shifts
source("R/compute_lead_time.R")
source("R/slip_rate_from_dwell.R") 
source("R/compute_mortality_models.R")
source("R/compute_split_survival.R")

#execute actual operations
source("scripts/prep_inc_spline.R") 

#source("scripts/old/prep_inc.R") 
source("scripts/01_get_incidence.R") 
source("scripts/02_retrieve_dwell.R") 

#generate isotonic adjusted sensitivity and use
source("scripts/prep_sens.R")
source("scripts/03_sensitivity_table.R") 

final_sens_table <- final_sens_table %>%
  mutate(sens = sens * 0.5)

#do interception model stage shift for all scenarios, ages, simultaneously
source("scripts/04_execute_parallel_interception.R") 

#start doing survival/mortality analysis
#precompute interpolated survival curves by age
source("scripts/prep_surv.R")
source("scripts/generate_css_by_age.R")

#adjust models for fraction of cancers that behave metastatically
source("scripts/05_merge_models.R")
#just 5 years survival, combine with interception
#split survival for cfdna+ and cfdna-
source("scripts/06_add_split_survival_to_models.R") # to include results for the sensitivity analyses

#Open cohort and single birth cohort through the full screening programme results from full combinatorial results
#source("scripts/07_cohorts_results_stage_skip_correct_w.R") #age-weighted based on attrition assumptions
source("scripts/07_cohorts_results_stage_skip_correct_w_140324.R")

source("scripts/07b_treatment_paper_stage_distribution.R") 
