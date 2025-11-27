#______________________________________________________________________________#
#### 01_run_models.R

#' Run the NICE disease severity modifier analysis
#'
library(tidyverse)
library(data.table)


#______________________________________________________________________________#

#rm(list=ls())
df_aggregate_absoluteprop_sf <- read.csv(file = "~/GalleriCEmodel/data-raw/NICE_modifier/df_absprop_combined.csv")
df_aggregate_absoluteprop_heatplot <- df_aggregate_absoluteprop_sf %>%
        slice(1:30) %>%
        mutate(across(everything(), as.factor)) %>%
        mutate(cycle = row_number()) %>%
        pivot_longer(-cycle, names_to = "Combination", values_to = "Modifier")

ggplot(df_aggregate_absoluteprop_heatplot, aes( Combination, cycle, fill=Modifier))+
        geom_tile() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_manual(drop=FALSE, values=colorRampPalette(c("lightblue","darkblue"))(3),  name="Modifier")

test_hete <- f_run_model(run_desc = "test_hete",
            export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
            data_dir = "~/GalleriCEmodel/data-raw/US_data_082023/",
            return_M_trace = TRUE,
            NICE_modifier_bool = TRUE,
            NICE_modifier_matrix = df_aggregate_absoluteprop_sf,
            # From iSetting in original
            model_start_age = 50,
            soc_screen_min_age = 50,
            soc_screen_max_age = 85,
            mced_screen_min_age = 50,
            mced_screen_max_age = 79,
            time_horizon = 100,
            perc_female = 0.533333333333333,
            disc_health = 0.03,
            disc_cost = 0.03,
            cost_growth_rate_screening = 0,
            cost_growth_rate = 0,
            dur_iatrogenic = 0.25,
            perspective = 1,
            analysis_type = 2,
            cohort_size = 100000,
            # From iBooleanSettings in original
            booAsympStageIII = TRUE, # Allow stage shift in stage III for patients with asymptomatic cancer
            booAsympStageIV = TRUE, # Allow stage shift in stage IV for patients with asymptomatic cancer
            booIatrogenic = 0, # Include iatrogenic harm
            # From iMisc in original
            mced_cost = 949, # MCED cost
            adherence = 1, # Adherence
            annual_compliance = 0.9, # Annual compliance
            mced_specificity = 0.995, # MCED specificity
            util_iatrogenic	= 0, # Iatrogenic harm utility
            disutil_FP = 0.05, # Disutility associated with false positive diagnosis
            disutil_misdiagnosis = 0.05, # Disutility associated with misdiagnosis
            perc_overdiagnosis = 0.05, # % of dead patients with asymptomatic cancer (for overdiagnosis calculation)
            perc_misdiagnosis = 1, # % of patients correctly diagnosed after misdiagnosis
            dur_workup_misdiagnosis = 0.5, # Additional work-up period for misdiagnosis
            dur_workup_FP = 0.5, # Additional work-up period for false positive diagnosis
            iPop = 1, # VBA index for cohort
            start_age_band = 11, # VBA index for age
            max_age_band = 20, # VBA index for age
            age_band_interval = 5, # Age band interval
            age_option = 1, # 1=min age
            run_type = "deterministic",
            nPSA = 1000, # PSA Number of simulations
            nDSA = 18, # DSA number of parameters
            optPSA_soc_test_costs = 1, # SoC Costs SE variation option
            optPSA_costs_cancer_tx = 1, # Cancer treatment costs variation option
            optPSA_costs_workup_FP = 2, # FP workup costs variation option
            optPSA_costs_workup_misdiagnosis = 2, # Misdiagnosis workup costs variation option
            optPSA_util_cancer = 2,	 # Disutility variation option
            pctPSA_soc_test_cost = 0, # Value of SoC Costs SE variation option
            pctPSA_costs_cancer_tx = 0, # Value of Cancer treatment costs variation option
            pctPSA_costs_workup_FP = 0.2, # Value of FP workup costs variation option
            pctPSA_costs_workup_misdiagnosis = 0.2, # Value of Misdiagnosis workup costs variation option
            pctPSA_util_cancer = 0.2, # Value of Disutility variation option
            UtilMultiplier =  1  # Multiplier applied to difference between cancer utility and general population utility
)

df_aggregate_absoluteprop_sf <- read.csv(file = "~/GalleriCEmodel/data-raw/NICE_modifier/df_absprop_combined.csv")
df_aggregate_absoluteprop_sf[df_aggregate_absoluteprop_sf > 1] <- 1

test_NOmult <- f_run_model(run_desc = "test_NoMult",
                         export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                         data_dir = "~/GalleriCEmodel/data-raw/US_data_082023/",
                         return_M_trace = TRUE,
                         NICE_modifier_bool = TRUE,
                         NICE_modifier_matrix = df_aggregate_absoluteprop_sf,
                         # From iSetting in original
                         model_start_age = 50,
                         soc_screen_min_age = 50,
                         soc_screen_max_age = 85,
                         mced_screen_min_age = 50,
                         mced_screen_max_age = 79,
                         time_horizon = 100,
                         perc_female = 0.533333333333333,
                         disc_health = 0.03,
                         disc_cost = 0.03,
                         cost_growth_rate_screening = 0,
                         cost_growth_rate = 0,
                         dur_iatrogenic = 0.25,
                         perspective = 1,
                         analysis_type = 1,
                         cohort_size = 100000,
                         # From iBooleanSettings in original
                         booAsympStageIII = TRUE, # Allow stage shift in stage III for patients with asymptomatic cancer
                         booAsympStageIV = TRUE, # Allow stage shift in stage IV for patients with asymptomatic cancer
                         booIatrogenic = 0, # Include iatrogenic harm
                         # From iMisc in original
                         mced_cost = 949, # MCED cost
                         adherence = 1, # Adherence
                         annual_compliance = 0.9, # Annual compliance
                         mced_specificity = 0.995, # MCED specificity
                         util_iatrogenic	= 0, # Iatrogenic harm utility
                         disutil_FP = 0.05, # Disutility associated with false positive diagnosis
                         disutil_misdiagnosis = 0.05, # Disutility associated with misdiagnosis
                         perc_overdiagnosis = 0.05, # % of dead patients with asymptomatic cancer (for overdiagnosis calculation)
                         perc_misdiagnosis = 1, # % of patients correctly diagnosed after misdiagnosis
                         dur_workup_misdiagnosis = 0.5, # Additional work-up period for misdiagnosis
                         dur_workup_FP = 0.5, # Additional work-up period for false positive diagnosis
                         iPop = 1, # VBA index for cohort
                         start_age_band = 11, # VBA index for age
                         max_age_band = 20, # VBA index for age
                         age_band_interval = 5, # Age band interval
                         age_option = 1, # 1=min age
                         run_type = "deterministic",
                         nPSA = 1000, # PSA Number of simulations
                         nDSA = 18, # DSA number of parameters
                         optPSA_soc_test_costs = 1, # SoC Costs SE variation option
                         optPSA_costs_cancer_tx = 1, # Cancer treatment costs variation option
                         optPSA_costs_workup_FP = 2, # FP workup costs variation option
                         optPSA_costs_workup_misdiagnosis = 2, # Misdiagnosis workup costs variation option
                         optPSA_util_cancer = 2,	 # Disutility variation option
                         pctPSA_soc_test_cost = 0, # Value of SoC Costs SE variation option
                         pctPSA_costs_cancer_tx = 0, # Value of Cancer treatment costs variation option
                         pctPSA_costs_workup_FP = 0.2, # Value of FP workup costs variation option
                         pctPSA_costs_workup_misdiagnosis = 0.2, # Value of Misdiagnosis workup costs variation option
                         pctPSA_util_cancer = 0.2, # Value of Disutility variation option
                         UtilMultiplier =  1  # Multiplier applied to difference between cancer utility and general population utility
)

cancer_names <- rep(c('lung', 'colon', ' pancreas', ' liver', ' breast_hr_pos',
                                    'esophagus', 'head_and_neck','stomach', 'ovary', 'kidney',
                                    'prostate', 'breast_hr_neg', 'lymphoma', 'anus',
                                    'uterus','bladder', 'cervix' , 'urothelial', 'other'), each = 4)
number_stage <- rep(seq(1,4),length(cancer_names)/4)

df_difference = as.data.frame(as.matrix(test_mult$res_MCED_QALYs) - as.matrix(test_NOmult$res_MCED_QALYs))
colnames(df_difference) <- paste0(cancer_stage,number_stage)


test_NOmult$time_shift_markov_trace

















