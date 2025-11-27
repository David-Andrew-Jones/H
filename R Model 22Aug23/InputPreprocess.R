# ----------------------------------------------------------------------------------------------------------------------------------------
#' @project EVA-34279-00
#' @name InputPreprocess.R
#' @description Input preprocess module
#' @version 1.0
#' @author Evidera
# ----------------------------------------------------------------------------------------------------------------------------------------


# ---------- Inputs pre-processing ----------
#---- Pre-process Global variables ----
n_cycle <- 101-model_start_age
cycle_array <- seq(1:n_cycle)

cohort_size <- as.numeric(cohort_size) 

model_start_age <- max(as.numeric(model_start_age),1)

incl_cancer_base <- as.logical(as.vector(incl_cancer[,1]))
incl_cancer_hete <- as.logical(as.vector(incl_cancer_hete[,1]))
n_cancer_base <- length(incl_cancer_base)
n_cancer_hete <- length(incl_cancer_hete)
if (analysis_type == 1){n_cancer <- n_cancer_base} else {n_cancer <- n_cancer_hete}
if (analysis_type == 1){incl_cancer <- incl_cancer_base} else {incl_cancer <- incl_cancer_hete}

# Set age bands indices
idx_age_band <- rep(start_age_band:max_age_band, each=age_band_interval)
idx_age_band <- c(idx_age_band, max_age_band)

# Get discount/inflation arrays
discount_health <- (1/(1+disc_health)^(1:n_cycle))
discount_cost <- (1/(1+disc_cost)^(1:n_cycle))
cost_growth_rates <- ((1+cost_growth_rate)^(0:(n_cycle-1)))
cost_growth_rates_screening <- ((1+cost_growth_rate_screening)^(0:(n_cycle-1)))


# Set general population mortality
gen_pop_mortality <- as.numeric(as.vector(gen_pop_mortality[,1]))
mortality <- gen_pop_mortality[(model_start_age:length(gen_pop_mortality))]

# Set general population utility
gen_pop_utility <- as.numeric(as.vector(gen_pop_utility[1,]))

# Set cancer utility multiplier for year 5+ to 1
util_cancer[,6] <- 1

# Convert incidence numbers to per 100,000
incidence_base <- incidence/100000
incidence_Intercept <- incidence_Intercept/100000
incidence_hete <- incidence_hete/100000
if (analysis_type == 1){incidence <- incidence_base} else {incidence <- incidence_hete}

#---- Pre-process SoC screening Inputs ----
soc_test_costs <- as.numeric(as.vector(soc_test_costs[,1])) * incl_cancer

soc_test_compliance <- as.numeric(as.vector(soc_test_compliance[,1]))
soc_test_frequency <- as.numeric(as.vector(soc_test_frequency[,1]))
soc_screen_compliance <- soc_test_compliance * soc_test_frequency * incl_cancer

soc_test_specificity <- as.numeric(as.vector(soc_test_specificity[,1]))
soc_test_specificity[!incl_cancer] <- 1

soc_test_eligibility[soc_test_eligibility=='Both'] <- 1
soc_test_eligibility[soc_test_eligibility=='Male'] <- 1 - perc_female
soc_test_eligibility[soc_test_eligibility=='Female'] <- perc_female
soc_test_eligibility <- as.numeric(as.vector(soc_test_eligibility[,1]))

#---- Pre-process MCED screening Inputs ----
incidence_base_intercept <- incidence_Intercept

sensitivity <- as.numeric(as.vector(sensitivity[,1]))
sensitivity_intercept <- as.data.frame(sensitivity)

dwell_times_base <- as.numeric(as.vector(dwell_times[,1]))
dwell_times_base_intercept <- as.data.frame(dwell_times_base)
dwell_times_hete <- as.numeric(as.vector(dwell_times_hete[,1]))
if (analysis_type == 1){dwell_times <- dwell_times_base} else {dwell_times <- dwell_times_hete}

# Calculate MCED compliance 
MCED_compliance <- rep(0,6)
MCED_compliance[1] <- adherence*annual_compliance
for(i in c(2:4)){ MCED_compliance[i] <- MCED_compliance[i-1]*(1-annual_compliance) }
MCED_compliance[5] <- adherence-sum(MCED_compliance[1:4])
MCED_compliance[6] <- 1-adherence

MCED_compliance_array <- rep(0, n_cycle)
MCED_compliance_array[mced_screen_min_age-model_start_age+1] <- 1 - MCED_compliance[6]
for(i in (mced_screen_min_age-model_start_age+2):n_cycle){
  MCED_compliance_array[i] <- 
    ((i - (mced_screen_min_age - model_start_age)) %% 1 == 0)*MCED_compliance[1] +
    ((i - (mced_screen_min_age - model_start_age)) %% 2 == 1)*MCED_compliance[2] +
    ((i - (mced_screen_min_age - model_start_age)) %% 3 == 1)*MCED_compliance[3] +
    ((i - (mced_screen_min_age - model_start_age)) %% 4 == 1)*MCED_compliance[4] +
    ((i - (mced_screen_min_age - model_start_age)) %% 5 == 1)*MCED_compliance[5]
  if (i>n_cycle | i>mced_screen_max_age-model_start_age+1) { MCED_compliance_array[i] <- 0} 
}

# Create mean time shift array
mean_time_shift_array <- rep(0, 16*n_cancer)
for(iCancer in 1:n_cancer){
  for (i in 1:3){
    for(j in (i+1):4){
      mean_time_shift_idx <- (iCancer-1)*16+(i-1)*4+j
      dwell_idx <- (iCancer-1)*4
      mean_time_shift_array[mean_time_shift_idx] <- sum(dwell_times[dwell_idx+c(i:(j-1))]) + dwell_times[dwell_idx+j]/2
    }
  }
}
mean_time_shift_array = rnd(mean_time_shift_array*4)/4  # round mean time shift to nearest 0.25
mean_time_shift_array_OD <- rnd(mean_time_shift_array) # round mean time shift for OD

# Convert mean time shift lookup dataframe to array
timeShiftDistMean<-as.numeric(as.vector(timeShiftDistMean[, 1]))

# MCED accuracy (tissue of origin)
accuracy_base <- as.numeric(as.vector(accuracy[,1]))
accuracy_hete <- as.numeric(as.vector(accuracy_hete[,1]))
accuracy_base[!incl_cancer_base] <- 1
accuracy_hete[!incl_cancer_hete] <- 1
if (analysis_type == 1){accuracy <- accuracy_base} else {accuracy <- accuracy_hete}

#Calculate overdiagnosis end stage
overdiagnosis_end_stage <- 2
if (booAsympStageIII){
  overdiagnosis_end_stage <- 3
  if (booAsympStageIV){
    overdiagnosis_end_stage <- 4
  }
}
