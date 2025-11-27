################################################################################

library(tidyverse)
library(data.table)


n_scenarios <- 10

l_res_scenarios <- vector("list", n_scenarios) %>% setNames(c("4to1", "3to1", "2to1", "1to1", "4to2",
                                                                      "3to2", "2to2", "4to3", "3to3", "4to4"))

start_ageofinterest <- 50

end_ageofinterest <- 79

for(s in 1:n_scenarios){

        SoC_trace_stage<- data.frame(one = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                     two = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                     three = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                     four = rep(0, length(start_ageofinterest:end_ageofinterest)))

        MCED_trace_stage <- data.frame(one = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                       two = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                       three = rep(0, length(start_ageofinterest:end_ageofinterest)),
                                       four = rep(0, length(start_ageofinterest:end_ageofinterest)))

        QALYs <- data.frame(one = rep(0, length(start_ageofinterest:end_ageofinterest)),
                            two = rep(0, length(start_ageofinterest:end_ageofinterest)),
                            three = rep(0, length(start_ageofinterest:end_ageofinterest)),
                            four = rep(0, length(start_ageofinterest:end_ageofinterest)))

        for(r in 1:length(start_ageofinterest:end_ageofinterest))
        {
                ################################################################################
                #### Input parameters - leave unchanged

                run_desc = "test"
                export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/"
                data_dir = "~/GalleriCEmodel/data-raw/US_data_082023/"
                return_M_trace = TRUE
                NICE_modifier_bool = FALSE
                NICE_modifier_matrix = NULL
                # From iSetting in original
                model_start_age = 50
                soc_screen_min_age = 50
                soc_screen_max_age = 85
                mced_screen_min_age = 50
                mced_screen_max_age = 100
                time_horizon = 100
                perc_female = 0.533333333333333
                disc_health = 0.035
                disc_cost = 0.035
                cost_growth_rate_screening = 0
                cost_growth_rate = 0
                dur_iatrogenic = 0.25
                perspective = 1
                analysis_type = 1
                cohort_size = 100000
                # From iBooleanSettings in original
                booAsympStageIII = TRUE # Allow stage shift in stage III for patients with asymptomatic cancer
                booAsympStageIV = TRUE # Allow stage shift in stage IV for patients with asymptomatic cancer
                booIatrogenic = 0 # Include iatrogenic harm
                # From iMisc in original
                mced_cost = 949 # MCED cost
                adherence = 1 # Adherence
                annual_compliance = 0.9 # Annual compliance
                mced_specificity = 0.995 # MCED specificity
                util_iatrogenic	= 0 # Iatrogenic harm utility
                disutil_FP = 0.05 # Disutility associated with false positive diagnosis
                disutil_misdiagnosis = 0.05 # Disutility associated with misdiagnosis
                perc_overdiagnosis = 0.05 # % of dead patients with asymptomatic cancer (for overdiagnosis calculation)
                perc_misdiagnosis = 1 # % of patients correctly diagnosed after misdiagnosis
                dur_workup_misdiagnosis = 0.5 # Additional work-up period for misdiagnosis
                dur_workup_FP = 0.5 # Additional work-up period for false positive diagnosis
                iPop = 1 # VBA index for cohort
                start_age_band = 11 # VBA index for age
                max_age_band = 20 # VBA index for age
                age_band_interval = 5 # Age band interval
                age_option = 1 # 1=min age
                run_type = "deterministic"
                nPSA = 1000 # PSA Number of simulations
                nDSA = 18 # DSA number of parameters
                optPSA_soc_test_costs = 1 # SoC Costs SE variation option
                optPSA_costs_cancer_tx = 1 # Cancer treatment costs variation option
                optPSA_costs_workup_FP = 2 # FP workup costs variation option
                optPSA_costs_workup_misdiagnosis = 2 # Misdiagnosis workup costs variation option
                optPSA_util_cancer = 2	 # Disutility variation option
                pctPSA_soc_test_cost = 0 # Value of SoC Costs SE variation option
                pctPSA_costs_cancer_tx = 0 # Value of Cancer treatment costs variation option
                pctPSA_costs_workup_FP = 0.2 # Value of FP workup costs variation option
                pctPSA_costs_workup_misdiagnosis = 0.2 # Value of Misdiagnosis workup costs variation option
                pctPSA_util_cancer = 0.2 # Value of Disutility variation option
                UtilMultiplier =  1  # Multiplier applied to difference between cancer utility and general population utility
                undiag_util = TRUE

                setwd(data_dir)
                # Place input in global environment
                list2env(as.list(formals(sys.function(sys.parent(n = 1)))), env = .GlobalEnv)

                # Put data in global environment
                l_data_file <- list.files(path = path.expand(data_dir), pattern="csv") %>%
                        map(~ as.data.frame(read_csv(.x, col_names = FALSE, show_col_types = FALSE)))

                names(l_data_file) <- tools::file_path_sans_ext(
                        list.files(path = path.expand(data_dir), pattern="csv")
                )

                list2env(l_data_file, env = .GlobalEnv)


                ################################################################################
                #### Constant input parameters - leave unchanged

                dwell_times[1:4] <- c(2,1,0.5,0.5)

                incl_cancer[2:19,1] <- FALSE

                accuracy[2:19,1] <- 0
                accuracy[1,1] <- 1
                annual_compliance <- 1

                gen_pop_mortality <- read.csv("~/GalleriCEmodel/data-raw/NICE_modifier/Eng_gen_pop_deathrate2021.csv")
                gen_pop_utility[1,11:20] <- c(0.822066667, 0.81, 0.797333333, 0.790533333, 0.795, 0.767666667, 0.746866667, 0.693266667, 0.663933333 ,0.663933333 )


                l_1_inc <- c(4.43,10.15,20.91,35.54,56.04,71.97,71.97,71.97,71.97,71.97)
                l_2_inc <- c(1.86,4.51,	9.62,15.24,23.26,30.00,30.00,30.00,30.00,30.00)
                l_3_inc <- c(6.72,14.49,26.97,41.37,58.42,73.28,73.28,73.28,73.28,73.28)
                l_4_inc <- c(17.52,35.51,65.09,99.62,138.45,169.87,169.87,169.87,169.87,169.87)
                incidence[1:76,1:20]  <- 0

                l_1_surv <- c(18.8356,	12.2228, 11.7285, 10.5351,8.415, 6.544,	6.544,	6.544,	6.544,	6.544)
                l_2_surv <- c(10.5772,	10.5772, 7.9647, 5.4702, 4.9261, 3.7348, 3.7348, 3.7348, 3.7348, 3.7348)
                l_3_surv <- c(3.4716,	2.9166,	2.8795,	2.547,	2.1653,	1.9359,	1.9359,	1.9359,	1.9359,	1.9359)
                l_4_surv <- c(1.6881,	1.1294,	1.0524,	0.8906,	0.8194,	0.7212,	0.7212,	0.7212,	0.7212,	0.7212)
                survival[1:76,1:20] <- 0

                l_1_utility <- util_cancer[1, ]
                l_2_utility <- util_cancer[2, ]
                l_3_utility <- util_cancer[3, ]
                l_4_utility <- util_cancer[4, ]

                util_cancer[1:76,1:6] <- 0


                ################################################################################
                model_start_age <- 49+r
                mced_screen_min_age <- 49+r

                if(model_start_age<55){start_age_band <- 11 } else if(model_start_age<60){start_age_band <- 12 } else if(model_start_age<65){start_age_band <- 13 }else
                        if(model_start_age<70){start_age_band <- 14 } else if(model_start_age<75){start_age_band <- 15 } else if(model_start_age<80){start_age_band <- 16 } else
                                if(85){start_age_band <- 17 } else if(model_start_age<90){start_age_band <- 18 } else if(model_start_age<95){start_age_band <- 19 } else
                                        if(model_start_age<100){start_age_band <- 20 }


                if(s == 1){ # 4 to 1
                        incidence[4,11:20]  <- l_4_inc
                        survival[1,11:20] <- l_4_surv
                        survival[2,11:20] <- l_4_surv
                        survival[3,11:20] <- l_4_surv
                        survival[4,11:20] <- l_4_surv

                        util_cancer[1,1:6] <- l_4_utility
                        util_cancer[2,1:6] <- l_4_utility
                        util_cancer[3,1:6] <- l_4_utility
                        util_cancer[4,1:6] <- l_4_utility

                } else if(s == 2){ # 3 to 1
                        incidence[3,11:20]  <- l_3_inc
                        survival[1,11:20] <- l_3_surv
                        survival[2,11:20] <- l_3_surv
                        survival[3,11:20] <- l_3_surv

                        util_cancer[1,1:6] <- l_3_utility
                        util_cancer[2,1:6] <- l_3_utility
                        util_cancer[3,1:6] <- l_3_utility

                } else if(s == 3){ # 2 to 1
                        incidence[2,11:20]  <- l_2_inc
                        survival[1,11:20] <- l_2_surv
                        survival[2,11:20] <- l_2_surv

                        util_cancer[1,1:6] <- l_2_utility
                        util_cancer[2,1:6] <- l_2_utility

                } else if(s == 4){ # 1 to 1
                        incidence[1,11:20]  <- l_1_inc
                        survival[1,11:20] <- l_1_surv

                        util_cancer[1,1:6] <- l_1_utility

                } else if(s == 5){ # 4 to 2
                        incidence[4,11:20]  <- l_4_inc
                        survival[1,11:20] <- l_4_surv
                        survival[2,11:20] <- l_4_surv
                        survival[3,11:20] <- l_4_surv
                        survival[4,11:20] <- l_4_surv

                        util_cancer[1,1:6] <- l_4_utility
                        util_cancer[2,1:6] <- l_4_utility
                        util_cancer[3,1:6] <- l_4_utility
                        util_cancer[4,1:6] <- l_4_utility

                } else if(s == 6){ # 3 to 2
                        incidence[3,11:20]  <- l_3_inc
                        survival[1,11:20] <- l_3_surv
                        survival[2,11:20] <- l_3_surv
                        survival[3,11:20] <- l_3_surv

                        util_cancer[1,1:6] <- l_3_utility
                        util_cancer[2,1:6] <- l_3_utility
                        util_cancer[3,1:6] <- l_3_utility

                } else if(s == 7){ # 2 to 2
                        incidence[2,11:20]  <- l_2_inc
                        survival[1,11:20] <- l_2_surv
                        survival[2,11:20] <- l_2_surv

                        util_cancer[1,1:6] <- l_2_utility
                        util_cancer[2,1:6] <- l_2_utility

                } else if(s == 8){ # 4 to 3
                        incidence[4,11:20]  <- l_4_inc
                        survival[1,11:20] <- l_4_surv
                        survival[2,11:20] <- l_4_surv
                        survival[3,11:20] <- l_4_surv
                        survival[4,11:20] <- l_4_surv

                        util_cancer[1,1:6] <- l_4_utility
                        util_cancer[2,1:6] <- l_4_utility
                        util_cancer[3,1:6] <- l_4_utility
                        util_cancer[4,1:6] <- l_4_utility

                } else if(s == 9){ # 3 to 3
                        incidence[3,11:20]  <- l_3_inc
                        survival[1,11:20] <- l_3_surv
                        survival[2,11:20] <- l_3_surv
                        survival[3,11:20] <- l_3_surv

                        util_cancer[1,1:6] <- l_3_utility
                        util_cancer[2,1:6] <- l_3_utility
                        util_cancer[3,1:6] <- l_3_utility

                } else if(s == 10){ # 4 to 4
                        incidence[4,11:20]  <- l_4_inc
                        survival[4,11:20] <- l_4_surv

                        util_cancer[4,1:6] <- l_4_utility

                }

                ################################################################################
                # Put other objects in global environment which do not evaluate if not
                n_cycle <- 101-model_start_age
                cycle_array <- seq(1:n_cycle)

                incl_cancer_base <- as.logical(unlist(incl_cancer[,1]))
                incl_cancer_hete <- as.logical(unlist(incl_cancer_hete[,1]))

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
                gen_pop_mortality <- as.numeric(unlist(gen_pop_mortality[,1]))
                mortality <- gen_pop_mortality[(model_start_age:length(gen_pop_mortality))]

                # Set general population utility
                gen_pop_utility <- as.numeric(unlist(gen_pop_utility[1,]))

                # Set cancer utility multiplier for year 5+ to 1
                util_cancer[,6] <- 1

                # Convert incidence numbers to per 100,000
                incidence_base <- incidence/100000
                incidence_Intercept <- incidence_Intercept/100000
                incidence_hete <- incidence_hete/100000
                if (analysis_type == 1){incidence <- incidence_base} else {incidence <- incidence_hete}

                #---- Pre-process SoC screening Inputs ----
                soc_test_costs <- as.numeric(unlist(soc_test_costs[,1])) * incl_cancer

                soc_test_compliance <- as.numeric(unlist(soc_test_compliance[,1]))
                soc_test_frequency <- as.numeric(unlist(soc_test_frequency[,1]))
                soc_screen_compliance <- soc_test_compliance * soc_test_frequency * incl_cancer

                soc_test_specificity <- as.numeric(unlist(soc_test_specificity[,1]))
                soc_test_specificity[!incl_cancer] <- 1

                soc_test_eligibility <- unlist(soc_test_eligibility)
                soc_test_eligibility[soc_test_eligibility=='Both'] <- 1
                soc_test_eligibility[soc_test_eligibility=='Male'] <- 1 - perc_female
                soc_test_eligibility[soc_test_eligibility=='Female'] <- perc_female
                soc_test_eligibility <- as.numeric(as.vector(soc_test_eligibility))

                incidence_base_intercept <- incidence_Intercept

                sensitivity <- as.numeric(unlist(sensitivity[,1]))
                sensitivity_intercept <- as.data.frame(sensitivity)

                dwell_times_base <- as.numeric(unlist(dwell_times[,1]))
                dwell_times_base_intercept <- as.data.frame(dwell_times_base)
                dwell_times_hete <- as.numeric(unlist(dwell_times_hete[,1]))
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
                timeShiftDistMean<-as.numeric(unlist(timeShiftDistMean[, 1]))

                # MCED accuracy (tissue of origin)
                accuracy_base <- as.numeric(unlist(accuracy[,1]))
                accuracy_hete <- as.numeric(unlist(accuracy_hete[,1]))
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

                list2env(list(n_cycle = n_cycle,
                              cycle_array = cycle_array,
                              incl_cancer = incl_cancer,
                              incl_cancer_base = incl_cancer_base,
                              incl_cancer_hete = incl_cancer_hete,
                              n_cancer = n_cancer,
                              n_cancer_base = n_cancer_base,
                              n_cancer_hete = n_cancer_hete,
                              idx_age_band = idx_age_band,
                              discount_health = discount_health,
                              discount_cost = discount_cost,
                              cost_growth_rates = cost_growth_rates,
                              cost_growth_rates_screening = cost_growth_rates_screening,
                              mortality = mortality,
                              gen_pop_utility = gen_pop_utility,
                              incidence_base = incidence_base,
                              incidence_Intercept = incidence_Intercept,
                              incidence_hete = incidence_hete,
                              incidence = incidence,
                              soc_test_costs = soc_test_costs,
                              soc_test_compliance = soc_test_compliance,
                              soc_test_frequency = soc_test_frequency,
                              soc_screen_compliance = soc_screen_compliance,
                              soc_test_specificity = soc_test_specificity,
                              soc_test_eligibility = soc_test_eligibility,
                              incidence_base_intercept =incidence_base_intercept,
                              sensitivity = sensitivity,
                              sensitivity_intercept = sensitivity_intercept,
                              dwell_times_base = dwell_times_base,
                              dwell_times_base_intercept = dwell_times_base_intercept,
                              dwell_times_hete = dwell_times_hete,
                              dwell_times = dwell_times,
                              MCED_compliance = MCED_compliance,
                              MCED_compliance_array = MCED_compliance_array,
                              mean_time_shift_array = mean_time_shift_array,
                              mean_time_shift_array_OD = mean_time_shift_array_OD,
                              timeShiftDistMean = timeShiftDistMean,
                              accuracy_base = accuracy_base,
                              accuracy_hete = accuracy_hete,
                              accuracy = accuracy,
                              overdiagnosis_end_stage = overdiagnosis_end_stage),
                         env = .GlobalEnv)

                if(NICE_modifier_bool && !is.null(NICE_modifier_matrix)) {

                        list2env(list(NICE_modifier_matrix = NICE_modifier_matrix),
                                 env = .GlobalEnv)

                } else if(NICE_modifier_bool && is.null(NICE_modifier_matrix)) {

                        stop("Please supply NICE_modifier_matrix")

                } else if(!NICE_modifier_bool && !is.null(NICE_modifier_matrix)) {

                        stop("Please use NICE_modifier = TRUE as argument")
                }


                #______________________________________________________________________#
                # For hete

                if (analysis_type == 1){survival <- survival} else {survival <- survival_hete}


                #______________________________________________________________________#
                # Generate Stage Shift Matrix
                # Taken from original InterceptModel.R at the bottom
                # Deploys functions of f_interception_model_functions.R

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

                ################################################################################
                ## For undgiagnosed 4 to 1


                if(s == 1){ # 4 to 1
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[1,4] <- 1

                } else if(s == 2){ # 3 to 1
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[1,3] <- 1

                } else if(s == 3){ # 2 to 1
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[1,2] <- 1

                } else if(s == 4){ # 1 to 1
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[1,1] <- 1

                } else if(s == 5){ # 4 to 2
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[2,4] <- 1

                } else if(s == 6){ # 3 to 2
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[2,3] <- 1

                } else if(s == 7){ # 2 to 2
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[2,2] <- 1

                } else if(s == 8){ # 4 to 3
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[3,4] <- 1

                } else if(s == 9){ # 3 to 3
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[3,3] <- 1

                } else if(s == 10){ # 4 to 4
                        stage_shift[1:4,1:4] <- 0
                        stage_shift[4,4] <- 1

                }


                ################################################################################

                #______________________________________________________________________#
                # Run the model
                # Taken from original Engine.R
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
                                soc_markov_trace[i,3:last_col] <- no_cancer*(unlist(incidence[,idx_age_band[i]])) # else, calculate incidence
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
                                                deaths <- unlist(deaths_5y[cancer_idx+iStage, age_col])
                                                stage_iatrogenic <- prop_iatrogenic[cancer_idx+iStage, age_col]

                                                # Record LYs
                                                unit_array <- c(1,1,1,1,1,1)
                                                res_soc_LYs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
                                                        calc_res_post_diag(surv = stage_survival,
                                                                           diag_age = iCycle-1,
                                                                           perc_iatrogenic = stage_iatrogenic,
                                                                           unit = unit_array,
                                                                           disc_rate = disc_health,
                                                                           flag_mced = FALSE)

                                                # Record QALYs
                                                unit_array <- as.numeric(util_cancer[cancer_idx+iStage,])
                                                res_soc_QALYs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
                                                        calc_post_diag_util(surv = stage_survival,
                                                                            diag_age = iCycle-1,
                                                                            perc_iatrogenic = stage_iatrogenic,
                                                                            unit = unit_array,
                                                                            disc_rate = disc_health,
                                                                            flag_mced = FALSE,
                                                                            Stage = NA,
                                                                            Cycle = NA)

                                                # Record costs
                                                unit_array <- as.numeric(costs_cancer_tx[cancer_idx+iStage,])
                                                unit_array <- unit_array * ((1+cost_growth_rate)^(iCycle - 1))
                                                res_soc_Costs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
                                                        calc_res_post_diag(surv = stage_survival,
                                                                           diag_age = iCycle-1,
                                                                           perc_iatrogenic = stage_iatrogenic,
                                                                           unit = unit_array,
                                                                           disc_rate = disc_cost,
                                                                           flag_mced = FALSE)

                                                # Record Societal costs
                                                unit_array <- as.numeric(rep(costs_societal[iCancer,iStage],6))
                                                res_soc_societal_costs[iCycle, cancer_idx+iStage] <- soc_markov_trace[iCycle, 2+cancer_idx+iStage]*
                                                        calc_res_post_diag(surv = stage_survival,
                                                                           diag_age = iCycle-1,
                                                                           perc_iatrogenic = stage_iatrogenic,
                                                                           unit = unit_array,
                                                                           disc_rate = disc_cost,
                                                                           flag_mced = FALSE)
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
                                                misdiag_accuracy <- unlist(accuracy[iCancer]) # get accuracy

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

                for(iCycle in 1:n_cycle)
                {
                        for (iCancer in 1:n_cancer)
                        {
                                if (incl_cancer[iCancer])
                                {

                                        #______________________________________________________________________________#
                                        #__________ Patients diagnosed at Stage IV

                                        iStage <- 4
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage
                                        #__________ IV to IV (no shift)
                                        iOriginstage <- NA
                                        time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
                                        # Calc post-diag outcomes
                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                             iCancer = iCancer,
                                                                             iStage = iStage,
                                                                             iLeadTime = 0,
                                                                             iPts = time_shift_markov_trace[iCycle,col_time_idx],
                                                                             res_LYs = res_MCED_LYs,
                                                                             res_QALYs = res_MCED_QALYs,
                                                                             res_costs = res_MCED_Costs,
                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                             res_deaths = res_MCED_Deaths,
                                                                             calc_year = iCycle,
                                                                             maxScreening = FALSE,
                                                                             Originstage = iOriginstage
                                        )

                                        res_MCED_LYs <- temp_list[[1]]
                                        res_MCED_QALYs <- temp_list[[2]]
                                        res_MCED_Costs <-temp_list[[3]]
                                        res_MCED_societal_costs <- temp_list[[4]]
                                        res_MCED_Deaths <- temp_list[[5]]

                                        #______________________________________________________________________________#
                                        #__________ Patients diagnosed at Stage III
                                        iStage <- 3
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage


                                        #__________ III to III (no shift)
                                        iOriginstage <- NA
                                        time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
                                        # Calc post-diag outcomes
                                        temp_list <-  calc_post_diag_res_MCED(iCycle = iCycle,
                                                                              iCancer = iCancer,
                                                                              iStage = iStage,
                                                                              iLeadTime = 0,
                                                                              iPts = time_shift_markov_trace[iCycle,col_time_idx],
                                                                              res_LYs = res_MCED_LYs,
                                                                              res_QALYs = res_MCED_QALYs,
                                                                              res_costs = res_MCED_Costs,
                                                                              res_societal_costs = res_MCED_societal_costs,
                                                                              res_deaths = res_MCED_Deaths,
                                                                              calc_year = iCycle,
                                                                              maxScreening = FALSE,
                                                                              Originstage = iOriginstage
                                        )

                                        res_MCED_LYs <- temp_list[[1]]
                                        res_MCED_QALYs <- temp_list[[2]]
                                        res_MCED_Costs <-temp_list[[3]]
                                        res_MCED_societal_costs <- temp_list[[4]]
                                        res_MCED_Deaths <- temp_list[[5]]

                                        #__________ IV to III
                                        iOriginstage <- 4

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
                                        for(j in 1:20) {  # max time shift is 20 years
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j # time shifted year
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*(unlist(timeShiftDist[tDist_idx,j]))
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
                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts =n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                }
                                                        }else{  # no collapse
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){ # time shift falls within max screening age
                                                                        # Time shift, calculate patient count
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage
                                                                        )

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage
                                                                                )

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                        }
                                                                }else{  # time shift falls outside max screening age
                                                                        # No time shift, move patients back to original stage
                                                                        time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+1,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
                                                                }
                                                        }
                                                }
                                        }

                                        #______________________________________________________________________________#
                                        #__________ Patients diagnosed at Stage II

                                        iStage <- 2
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage

                                        #__________ II to II (no shift)
                                        iOriginstage <- NA
                                        time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
                                        # Calc post-diag outcomes
                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                             iCancer = iCancer,
                                                                             iStage = iStage,
                                                                             iLeadTime = 0,
                                                                             iPts = time_shift_markov_trace[iCycle,col_time_idx],
                                                                             res_LYs = res_MCED_LYs,
                                                                             res_QALYs = res_MCED_QALYs,
                                                                             res_costs = res_MCED_Costs,
                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                             res_deaths = res_MCED_Deaths,
                                                                             calc_year = iCycle,
                                                                             maxScreening = FALSE,
                                                                             Originstage = iOriginstage)

                                        res_MCED_LYs <- temp_list[[1]]
                                        res_MCED_QALYs <- temp_list[[2]]
                                        res_MCED_Costs <-temp_list[[3]]
                                        res_MCED_societal_costs <- temp_list[[4]]
                                        res_MCED_Deaths <- temp_list[[5]]

                                        #__________ III to II
                                        iOriginstage <- 3

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], timeShiftDistMean)
                                        for(j in 1:20){  # max time shift is 20 years
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j # time shifted year
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*unlist(timeShiftDist[tDist_idx,j])
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
                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts = n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                }
                                                        }else{  # no collapse
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                                        # Time shift, calculate patient count
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted

                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage)

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                        }
                                                                }else{  # time shift falls outside max screening age
                                                                        # No time shift, move patients back to original stage
                                                                        time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+1,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
                                                                }
                                                        }
                                                }
                                        }

                                        #__________ IV to II
                                        iOriginstage <- 4

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], timeShiftDistMean)
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*unlist(timeShift4to2[iCancer,j])
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
                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage
                                                                )

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+2,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts = n_shifted*perc3,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1 # Update pts shifting to stage II
                                                                        time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2 # Update pts shifting to stage III

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage + 1,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc2,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                }
                                                        }else{
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                                        # Time shift, calculate patient count
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted

                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage)

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                        }
                                                                }else{
                                                                        # No time shift, move patients back to original stage
                                                                        time_shift_markov_trace[iCycle, col_time_idx+2] <- time_shift_markov_trace[iCycle, col_time_idx+2] + n_shifted
                                                                        # Calculate post-diagnosis results
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+2,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
                                                                }
                                                        }
                                                }
                                        }

                                        #______________________________________________________________________________#
                                        #__________ Patients diagnosed at Stage I

                                        iStage <- 1
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage

                                        #__________ I to I (no shift)
                                        iOriginstage <- NA
                                        time_shift_markov_trace[iCycle,col_time_idx] <- stage_shift_markov_trace[iCycle,col_stage_idx]
                                        # Calc post-diag outcomes
                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                             iCancer = iCancer,
                                                                             iStage = iStage,
                                                                             iLeadTime = 0,
                                                                             iPts = time_shift_markov_trace[iCycle,col_time_idx],
                                                                             res_LYs = res_MCED_LYs,
                                                                             res_QALYs = res_MCED_QALYs,
                                                                             res_costs = res_MCED_Costs,
                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                             res_deaths = res_MCED_Deaths,
                                                                             calc_year = iCycle,
                                                                             maxScreening = FALSE,
                                                                             Originstage = iOriginstage)

                                        res_MCED_LYs <- temp_list[[1]]
                                        res_MCED_QALYs <- temp_list[[2]]
                                        res_MCED_Costs <-temp_list[[3]]
                                        res_MCED_societal_costs <- temp_list[[4]]
                                        res_MCED_Deaths <- temp_list[[5]]

                                        #__________  II to I
                                        iOriginstage <- 2

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*unlist(timeShiftDist[tDist_idx,j])
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


                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts = n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                }
                                                        }else{
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted

                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage)

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                        }
                                                                }else{
                                                                        time_shift_markov_trace[iCycle, col_time_idx+1] <- time_shift_markov_trace[iCycle, col_time_idx+1] + n_shifted
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+1,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
                                                                }
                                                        }
                                                }
                                        }

                                        #__________ III to I
                                        iOriginstage <- 3

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*unlist(timeShift3to1[iCancer,j])
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

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+2,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts = n_shifted*perc3,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
                                                                        time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage + 1,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc2,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                }
                                                        }else{
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage)

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                        }
                                                                }else{
                                                                        time_shift_markov_trace[iCycle, col_time_idx+2] <- time_shift_markov_trace[iCycle, col_time_idx+2] + n_shifted

                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+2,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
                                                                }
                                                        }
                                                }
                                        }


                                        #__________ IV to I
                                        iOriginstage <- 4

                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+3], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+3]*unlist(timeShift4to1[iCancer,j])
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

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc1,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+1,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc2,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+2,
                                                                                                     iLeadTime = iCycle - 1,
                                                                                                     iPts = n_shifted*perc3,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = iOriginstage)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                     iCancer = iCancer,
                                                                                                     iStage = iStage+3,
                                                                                                     iLeadTime = 0,
                                                                                                     iPts = n_shifted*perc4,
                                                                                                     res_LYs = res_MCED_LYs,
                                                                                                     res_QALYs = res_MCED_QALYs,
                                                                                                     res_costs = res_MCED_Costs,
                                                                                                     res_societal_costs = res_MCED_societal_costs,
                                                                                                     res_deaths = res_MCED_Deaths,
                                                                                                     calc_year = 1,
                                                                                                     maxScreening = FALSE,
                                                                                                     Originstage = NA)

                                                                res_MCED_LYs <- temp_list[[1]]
                                                                res_MCED_QALYs <- temp_list[[2]]
                                                                res_MCED_Costs <-temp_list[[3]]
                                                                res_MCED_societal_costs <- temp_list[[4]]
                                                                res_MCED_Deaths <- temp_list[[5]]

                                                                if (shiftedOutcomes) {
                                                                        time_shift_markov_trace_TS[1, col_time_idx] <- time_shift_markov_trace_TS[1, col_time_idx] + n_shifted*perc1
                                                                        time_shift_markov_trace_TS[1, col_time_idx+1] <- time_shift_markov_trace_TS[1, col_time_idx+1] + n_shifted*perc2
                                                                        time_shift_markov_trace_TS[1, col_time_idx+2] <- time_shift_markov_trace_TS[1, col_time_idx+2] + n_shifted*perc3

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc1,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage + 1,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc2,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]

                                                                        temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                   iCancer = iCancer,
                                                                                                                   iStage = iStage + 2,
                                                                                                                   iLeadTime = iCycle - 1,
                                                                                                                   iPts = n_shifted*perc3,
                                                                                                                   res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                   res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                   res_costs_TS = res_MCED_Costs_TS,
                                                                                                                   calc_year = 1,
                                                                                                                   maxScreening = FALSE,
                                                                                                                   Originstage = iOriginstage)

                                                                        res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                        res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                        res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                }
                                                        }else{
                                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                                        time_shift_markov_trace[calc_year, col_time_idx] <- time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage,
                                                                                                             iLeadTime = j,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = iOriginstage)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]

                                                                        if (shiftedOutcomes) {
                                                                                time_shift_markov_trace_TS[calc_year, col_time_idx] <- time_shift_markov_trace_TS[calc_year, col_time_idx] + n_shifted
                                                                                temp_list_TS <- calc_post_diag_res_MCED_TS(iCycle = iCycle,
                                                                                                                           iCancer = iCancer,
                                                                                                                           iStage = iStage,
                                                                                                                           iLeadTime = j,
                                                                                                                           iPts = n_shifted,
                                                                                                                           res_LYs_TS = res_MCED_LYs_TS,
                                                                                                                           res_QALYs_TS = res_MCED_QALYs_TS,
                                                                                                                           res_costs_TS = res_MCED_Costs_TS,
                                                                                                                           calc_year = calc_year,
                                                                                                                           maxScreening = FALSE,
                                                                                                                           Originstage = iOriginstage)

                                                                                res_MCED_LYs_TS <-temp_list_TS[[1]]
                                                                                res_MCED_QALYs_TS <- temp_list_TS[[2]]
                                                                                res_MCED_Costs_TS <- temp_list_TS[[3]]
                                                                        }
                                                                }else{
                                                                        time_shift_markov_trace[iCycle, col_time_idx+3] <- time_shift_markov_trace[iCycle, col_time_idx+3] + n_shifted
                                                                        temp_list <- calc_post_diag_res_MCED(iCycle = iCycle,
                                                                                                             iCancer = iCancer,
                                                                                                             iStage = iStage+3,
                                                                                                             iLeadTime = 0,
                                                                                                             iPts = n_shifted,
                                                                                                             res_LYs = res_MCED_LYs,
                                                                                                             res_QALYs = res_MCED_QALYs,
                                                                                                             res_costs = res_MCED_Costs,
                                                                                                             res_societal_costs = res_MCED_societal_costs,
                                                                                                             res_deaths = res_MCED_Deaths,
                                                                                                             calc_year = calc_year,
                                                                                                             maxScreening = FALSE,
                                                                                                             Originstage = NA)

                                                                        res_MCED_LYs <- temp_list[[1]]
                                                                        res_MCED_QALYs <- temp_list[[2]]
                                                                        res_MCED_Costs <-temp_list[[3]]
                                                                        res_MCED_societal_costs <- temp_list[[4]]
                                                                        res_MCED_Deaths <- temp_list[[5]]
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], unlist(timeShiftDistMean))
                                        for(j in 1:20) {  # max time shift is 20 years
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j # time shifted year
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*unlist(timeShiftDist[tDist_idx,j])
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], unlist(timeShiftDistMean))
                                        for(j in 1:20){  # max time shift is 20 years
                                                if (unlist(timeShiftDist[tDist_idx,j] != 0)) {
                                                        calc_year <- iCycle - j # time shifted year
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*unlist(timeShiftDist[tDist_idx,j])
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*unlist(timeShift4to2[iCancer,j])
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+1], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+1]*unlist(timeShiftDist[tDist_idx,j])
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+2], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+2]*unlist(timeShift3to1[iCancer,j])
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
                                        tDist_idx <- match(mean_time_shift_array[col_stage_idx+3], unlist(timeShiftDistMean))
                                        for(j in 1:20){
                                                if (unlist(timeShiftDist[tDist_idx,j]) != 0) {
                                                        calc_year <- iCycle - j
                                                        n_shifted <- stage_shift_markov_trace[iCycle, col_stage_idx+3]*unlist(timeShift4to1[iCancer,j])
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
                                        iOriginstage <- NA
                                        iStage <- 3
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage

                                        # IV to III
                                        iOriginstage <- 4
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

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_4to3,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)

                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]


                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_4to3,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)

                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                        overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                }
                                        }

                                        # Stage II
                                        iOriginstage <- NA
                                        iStage <- 2
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage
                                        # III to II
                                        iOriginstage <- 3
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

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_3to2,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)

                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_3to2,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)

                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                        overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                }
                                        }
                                        # IV to II
                                        iOriginstage <- 4
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

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_4to2,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage+1,
                                                                                                  mean_time_shift_4to3,
                                                                                                  0,
                                                                                                  n_shifted*perc2,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]


                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_4to2,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)
                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                        overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                }
                                        }

                                        # Stage I
                                        iOriginstage <- NA
                                        iStage <- 1
                                        col_stage_idx <- (iCancer-1)*16+(iStage-1)*4+iStage
                                        col_time_idx <- 2+(iCancer-1)*4+iStage
                                        # II to I
                                        iOriginstage <- 2
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

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_2to1,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)

                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_2to1,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)

                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                        overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                }
                                        }
                                        # III to I
                                        iOriginstage <- 3
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
                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_3to1,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage+1,
                                                                                                  mean_time_shift_3to2,
                                                                                                  0,
                                                                                                  n_shifted*perc2,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_3to1,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)
                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                        overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] <- overdiagnosed_time_shift_markov_trace[calc_year, col_time_idx] + n_shifted
                                                }
                                        }
                                        # IV to I
                                        iOriginstage <- 4
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

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage,
                                                                                                  mean_time_shift_4to1,
                                                                                                  0,
                                                                                                  n_shifted*perc1,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage+1,
                                                                                                  mean_time_shift_4to2,
                                                                                                  0,
                                                                                                  n_shifted*perc2,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                  iCancer,
                                                                                                  iStage+2,
                                                                                                  mean_time_shift_4to3,
                                                                                                  0,
                                                                                                  n_shifted*perc3,
                                                                                                  res_overdiag_costs,
                                                                                                  res_overdiag_QALYs,
                                                                                                  res_overdiag_screen_costs,
                                                                                                  Originstage = iOriginstage)
                                                res_overdiag_costs <- temp_list_overdiag[[1]]
                                                res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                res_overdiag_screen_costs <- temp_list_overdiag[[3]]

                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx] + n_shifted*perc1
                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+1] + n_shifted*perc2
                                                overdiagnosed_time_shift_markov_trace[1, col_time_idx+2] <- overdiagnosed_time_shift_markov_trace[1, col_time_idx+2] + n_shifted*perc3
                                        }else{
                                                if(calc_year+model_start_age-1 <= mced_screen_max_age){
                                                        temp_list_overdiag <- calc_post_diag_res_overdiag(iCycle,
                                                                                                          iCancer,
                                                                                                          iStage,
                                                                                                          mean_time_shift_4to1,
                                                                                                          0,
                                                                                                          n_shifted,
                                                                                                          res_overdiag_costs,
                                                                                                          res_overdiag_QALYs,
                                                                                                          res_overdiag_screen_costs,
                                                                                                          Originstage = iOriginstage)
                                                        res_overdiag_costs <- temp_list_overdiag[[1]]
                                                        res_overdiag_QALYs <- temp_list_overdiag[[2]]
                                                        res_overdiag_screen_costs <- temp_list_overdiag[[3]]

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

                #______________________________________________________________________________#
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


                #______________________________________________________________________#
                # Prepare results for export
                # Taken from original ExportResults.R

                # # ---------- Summarize Results -----------
                # ---------- Aggregate Number Diagnosis Outcomes -----------
                # Cancer diagnosis
                res_soc_diagnosed_1 <- sum(rowSums(soc_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
                res_soc_diagnosed_2 <- sum(rowSums(soc_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
                res_soc_diagnosed_3 <- sum(rowSums(soc_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
                res_soc_diagnosed_4 <- sum(rowSums(soc_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_1 <- sum(rowSums(time_shift_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_2 <- sum(rowSums(time_shift_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_3 <- sum(rowSums(time_shift_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_4 <- sum(rowSums(time_shift_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_1_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(3,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_2_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(4,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_3_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(5,2+n_cancer*4,by=4))]))
                res_MCED_diagnosed_4_TS <- sum(rowSums(time_shift_markov_trace_TS[,c(seq(6,2+n_cancer*4,by=4))]))

                # Overdiagnosis
                res_MCED_total_overdiagnosed <- sum(overdiagnosed_time_shift_markov_trace)
                res_MCED_overdiagnosed_1 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(3,2+n_cancer*4,by=4))]))
                res_MCED_overdiagnosed_2 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(4,2+n_cancer*4,by=4))]))
                res_MCED_overdiagnosed_3 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(5,2+n_cancer*4,by=4))]))
                res_MCED_overdiagnosed_4 <- sum(rowSums(overdiagnosed_time_shift_markov_trace[,c(seq(6,2+n_cancer*4,by=4))]))

                # Total number outcomes
                res_number_outcomes <-
                        rbind(
                                c(res_MCED_total_diagnosed, res_soc_total_diagnosed), #Total number diagnosed
                                c(res_MCED_diagnosed_1, res_soc_diagnosed_1), #Total diagnosed stage I
                                c(res_MCED_diagnosed_2, res_soc_diagnosed_2), #Total diagnosed stage II
                                c(res_MCED_diagnosed_3, res_soc_diagnosed_3), #Total diagnosed stage III
                                c(res_MCED_diagnosed_4, res_soc_diagnosed_4), #Total diagnosed stage IV

                                c(res_MCED_total_overdiagnosed, 0), #Total number overdiagnosed
                                c(res_MCED_overdiagnosed_1, 0), #Total overdiagnosed stage I
                                c(res_MCED_overdiagnosed_2, 0),#Total overdiagnosed stage II
                                c(res_MCED_overdiagnosed_3, 0), #Total overdiagnosed stage III
                                c(res_MCED_overdiagnosed_4, 0), #Total overdiagnosed stage Iv

                                c(sum(res_MCED_soc_screen$soc_FPs,res_MCED_MCED_screen$MCED_FPs), sum(res_soc_soc_screen$soc_FPs)), # Total FPs
                                c(sum(misdiagnosis), 0) # Total misdiagnosed
                        )

                names_res_number_row <- c('diagnosed_total', 'diagnosed_1', 'diagnosed_2', 'diagnosed_3', 'diagnosed_4',
                                          'overdiagnosed_total', 'overdiagnosed_1', 'overdiagnosed_2', 'overdiagnosed_3', 'overdiagnosed_4',
                                          'total_FPs', 'total_misdiagnosed')
                row.names(res_number_outcomes) <- names_res_number_row

                # ---------- Aggregate Health Outcomes -----------
                # Pre-diagnosis LYs
                res_soc_LYs_pre_diag <- sum(res_soc_pre_diag$disc_LYs)/res_soc_total_diagnosed
                res_MCED_LYs_pre_diag <- sum(res_MCED_pre_diag$disc_LYs)/res_MCED_total_diagnosed

                # Post-diagnosis LYs
                res_soc_LYs_post_diag <- sum(rowSums(res_soc_LYs, na.rm = T))/res_soc_total_diagnosed
                res_MCED_LYs_post_diag <- sum(rowSums(res_MCED_LYs, na.rm = T))/res_MCED_total_diagnosed
                res_MCED_LYs_post_diag_TS <- sum(rowSums(res_MCED_LYs_TS, na.rm = T))/res_MCED_total_diagnosed_TS

                # Pre-diagnosis QALYs
                res_soc_QALYs_pre_diag <- sum(res_soc_pre_diag$disc_QALYs)/res_soc_total_diagnosed
                res_MCED_QALYs_pre_diag <- sum(res_MCED_pre_diag$disc_QALYs)/res_MCED_total_diagnosed

                # Post-diagnosis QALYs
                res_soc_QALYs_post_diag <- sum(rowSums(res_soc_QALYs, na.rm = T))/res_soc_total_diagnosed
                res_MCED_QALYs_post_diag <- sum(rowSums(res_MCED_QALYs, na.rm = T))/res_MCED_total_diagnosed
                res_MCED_QALYs_post_diag_TS <- sum(rowSums(res_MCED_QALYs_TS, na.rm = T))/res_MCED_total_diagnosed_TS

                # Total diagnosis LYs and QALYs
                res_soc_LYs_total_diag <- res_soc_LYs_pre_diag + res_soc_LYs_post_diag
                res_MCED_LYs_total_diag <- res_MCED_LYs_pre_diag + res_MCED_LYs_post_diag
                res_soc_QALYs_total_diag <- res_soc_QALYs_pre_diag + res_soc_QALYs_post_diag
                res_MCED_QALYs_total_diag <- res_MCED_QALYs_pre_diag + res_MCED_QALYs_post_diag

                # Total no-cancer diagnosis LYs and QALYs
                res_soc_LYs_no_diag <- sum(res_soc_no_diag[,'disc_LYs'])
                res_MCED_LYs_no_diag <- sum(res_MCED_no_diag[,'disc_LYs'])
                res_soc_QALYs_no_diag <- sum(res_soc_no_diag[,'disc_QALYs'])
                res_MCED_QALYs_no_diag <- sum(res_MCED_no_diag[,'disc_QALYs'])

                # Overdiagnosis results
                res_MCED_disutil_overdiag <- sum(res_overdiag_QALYs)/cohort_size
                res_MCED_costs_overdiag <- sum(res_overdiag_costs)/cohort_size
                res_MCED_screen_costs_overdiag <- sum(res_overdiag_screen_costs)/cohort_size

                # Record total LYs and QALYs
                res_soc_total_LYs <- (res_soc_LYs_no_diag*(cohort_size - res_soc_total_diagnosed) +
                                              res_soc_LYs_total_diag*res_soc_total_diagnosed)/cohort_size
                res_MCED_total_LYs <- (res_MCED_LYs_no_diag*(cohort_size - res_MCED_total_diagnosed) +
                                               res_MCED_LYs_total_diag*res_MCED_total_diagnosed)/cohort_size

                res_soc_total_QALYs <- (res_soc_QALYs_no_diag*(cohort_size-res_soc_total_diagnosed) +
                                                res_soc_QALYs_total_diag*res_soc_total_diagnosed)/cohort_size - res_soc_disutil_FPs
                res_MCED_total_QALYs <- (res_MCED_QALYs_no_diag*(cohort_size - res_MCED_total_diagnosed) +
                                                 res_MCED_QALYs_total_diag*res_MCED_total_diagnosed)/cohort_size - res_MCED_disutil_FPs - res_MCED_disutil_misdiagnosis - res_MCED_disutil_overdiag
                res_health_outcomes <-
                        rbind(
                                c(res_MCED_total_LYs, res_soc_total_LYs), # Total LYs
                                c(res_MCED_LYs_no_diag, res_soc_LYs_no_diag), # Total No Cancer diagnosis LYs
                                c(res_MCED_LYs_total_diag, res_soc_LYs_total_diag), # Total Cancer Diagnosis LYs
                                c(res_MCED_LYs_pre_diag, res_soc_LYs_pre_diag), # Pre-diagnosis LYs
                                c(res_MCED_LYs_post_diag, res_soc_LYs_post_diag), # Post-diagnosis LYs

                                c(res_MCED_total_QALYs, res_soc_total_QALYs), # Total QALYs
                                c(res_MCED_QALYs_no_diag, res_soc_QALYs_no_diag), # Total No Cancer diagnosis QALYs
                                c(res_MCED_QALYs_total_diag, res_soc_QALYs_total_diag), # Total Cancer Diagnosis QALYs
                                c(res_MCED_QALYs_pre_diag, res_soc_QALYs_pre_diag), # Pre-diagnosis QALYs
                                c(res_MCED_QALYs_post_diag, res_soc_QALYs_post_diag), # Post-diagnosis QALYs

                                c(-res_MCED_disutil_FPs, -res_soc_disutil_FPs),
                                c(-res_MCED_disutil_misdiagnosis, 0),
                                c(-res_MCED_disutil_overdiag, 0)
                        )
                names_res_health_row <- c('LYs_total','LYs_total_no_diag','LYs_total_diag','LYs_pre_diag','LYs_post_diag',
                                          'QALYs_total','QALYs_total_no_diag','QALYs_total_diag','QALYs_pre_diag','QALYs_post_diag',
                                          'disutility_FP', 'disutility_misdiag', 'disutility_OD')
                row.names(res_health_outcomes) <- names_res_health_row

                # ---------- Aggregate Deaths outcomes -----------
                res_soc_deaths_1 <- sum(rowSums(res_soc_Deaths[,c(seq(1,n_cancer*4,by=4))]))
                res_soc_deaths_2 <- sum(rowSums(res_soc_Deaths[,c(seq(2,n_cancer*4,by=4))]))
                res_soc_deaths_3 <- sum(rowSums(res_soc_Deaths[,c(seq(3,n_cancer*4,by=4))]))
                res_soc_deaths_4 <- sum(rowSums(res_soc_Deaths[,c(seq(4,n_cancer*4,by=4))]))
                res_MCED_deaths_1 <- sum(rowSums(res_MCED_Deaths[,c(seq(1,n_cancer*4,by=4))]))
                res_MCED_deaths_2 <- sum(rowSums(res_MCED_Deaths[,c(seq(2,n_cancer*4,by=4))]))
                res_MCED_deaths_3 <- sum(rowSums(res_MCED_Deaths[,c(seq(3,n_cancer*4,by=4))]))
                res_MCED_deaths_4 <- sum(rowSums(res_MCED_Deaths[,c(seq(4,n_cancer*4,by=4))]))

                res_soc_deaths_total <- res_soc_deaths_1 + res_soc_deaths_2 + res_soc_deaths_3 + res_soc_deaths_4
                res_MCED_deaths_total <- res_MCED_deaths_1 + res_MCED_deaths_2 + res_MCED_deaths_3 + res_MCED_deaths_4

                res_deaths_outcomes <-
                        rbind(
                                c(res_MCED_deaths_total, res_soc_deaths_total), # Total deaths
                                c(res_MCED_deaths_1, res_soc_deaths_1), # Deaths Stage I
                                c(res_MCED_deaths_2, res_soc_deaths_2), # Deaths Stage II
                                c(res_MCED_deaths_3, res_soc_deaths_3), # Deaths Stage III
                                c(res_MCED_deaths_4, res_soc_deaths_4) # Deaths Stage IV
                        )

                names_res_deaths_row <- c('deaths_total', 'deaths_1', 'deaths_2', 'deaths_3', 'deaths_4')
                row.names(res_deaths_outcomes) <- names_res_deaths_row

                # ---------- Aggregate Cost Outcomes -----------
                # Screening costs
                res_soc_scren_cost_pre_diag <- sum(res_soc_soc_screen[,'soc_screening_costs']*cost_growth_rates_screening*discount_cost)/cohort_size
                res_soc_scren_cost_post_diag <- 0
                res_soc_screen_cost_total <- res_soc_scren_cost_pre_diag + res_soc_scren_cost_post_diag

                res_MCED_scren_cost_pre_diag <- sum((res_MCED_soc_screen[,'soc_screening_costs']+res_MCED_MCED_screen['MCED_screening_costs'])*cost_growth_rates_screening*discount_cost)/cohort_size
                res_MCED_scren_cost_pre_diag <- res_MCED_scren_cost_pre_diag - res_MCED_screen_costs_overdiag
                res_MCED_scren_cost_post_diag <- 0
                res_MCED_screen_cost_total <- res_MCED_scren_cost_pre_diag + res_MCED_scren_cost_post_diag

                # Cancer treatment costs per individual
                soc_cancer_tx_cost_total <- sum(rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=1))]))
                soc_cancer_tx_cost_1 <- sum(rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=4))]))
                soc_cancer_tx_cost_2 <- sum(rowSums(res_soc_Costs[,c(seq(2,n_cancer*4,by=4))]))
                soc_cancer_tx_cost_3 <- sum(rowSums(res_soc_Costs[,c(seq(3,n_cancer*4,by=4))]))
                soc_cancer_tx_cost_4 <- sum(rowSums(res_soc_Costs[,c(seq(4,n_cancer*4,by=4))]))

                # without OD
                MCED_cancer_tx_cost_total <- sum(rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=1))]))
                MCED_cancer_tx_cost_1 <- sum(rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_2 <- sum(rowSums(res_MCED_Costs[,c(seq(2,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_3 <- sum(rowSums(res_MCED_Costs[,c(seq(3,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_4 <- sum(rowSums(res_MCED_Costs[,c(seq(4,n_cancer*4,by=4))]))

                # OD
                MCED_cancer_tx_cost_total_OD <- sum(rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=1))]))
                MCED_cancer_tx_cost_1_OD <- sum(rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_2_OD <- sum(rowSums(res_overdiag_costs[,c(seq(2,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_3_OD <- sum(rowSums(res_overdiag_costs[,c(seq(3,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_4_OD <- sum(rowSums(res_overdiag_costs[,c(seq(4,n_cancer*4,by=4))]))

                # Shifted individual (without OD)
                MCED_cancer_tx_cost_total_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(1,n_cancer*4,by=1))]))
                MCED_cancer_tx_cost_1_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(1,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_2_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(2,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_3_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(3,n_cancer*4,by=4))]))
                MCED_cancer_tx_cost_4_TS <- sum(rowSums(res_MCED_Costs_TS[,c(seq(4,n_cancer*4,by=4))]))

                # SoC total
                res_soc_cancer_tx_cost_total <- soc_cancer_tx_cost_total/cohort_size
                res_soc_cancer_tx_cost_1 <- soc_cancer_tx_cost_1/cohort_size
                res_soc_cancer_tx_cost_2 <- soc_cancer_tx_cost_2/cohort_size
                res_soc_cancer_tx_cost_3 <- soc_cancer_tx_cost_3/cohort_size
                res_soc_cancer_tx_cost_4 <- soc_cancer_tx_cost_4/cohort_size

                # MCED total
                res_MCED_cancer_tx_cost_total <-(MCED_cancer_tx_cost_total+MCED_cancer_tx_cost_total_OD)/cohort_size
                res_MCED_cancer_tx_cost_1 <- (MCED_cancer_tx_cost_1+MCED_cancer_tx_cost_1_OD)/cohort_size
                res_MCED_cancer_tx_cost_2 <- (MCED_cancer_tx_cost_2+MCED_cancer_tx_cost_2_OD)/cohort_size
                res_MCED_cancer_tx_cost_3 <- (MCED_cancer_tx_cost_3+MCED_cancer_tx_cost_3_OD)/cohort_size
                res_MCED_cancer_tx_cost_4 <- (MCED_cancer_tx_cost_4+MCED_cancer_tx_cost_4_OD)/cohort_size

                # Cancer treatment costs per diagnosed patients (without OD)
                res_soc_diag_cancer_tx_cost_total <- soc_cancer_tx_cost_total/res_soc_total_diagnosed
                res_soc_diag_cancer_tx_cost_1 <- soc_cancer_tx_cost_1/res_soc_total_diagnosed
                res_soc_diag_cancer_tx_cost_2 <- soc_cancer_tx_cost_2/res_soc_total_diagnosed
                res_soc_diag_cancer_tx_cost_3 <- soc_cancer_tx_cost_3/res_soc_total_diagnosed
                res_soc_diag_cancer_tx_cost_4 <- soc_cancer_tx_cost_4/res_soc_total_diagnosed

                res_MCED_diag_cancer_tx_cost_total <-MCED_cancer_tx_cost_total/res_MCED_total_diagnosed
                res_MCED_diag_cancer_tx_cost_1 <- MCED_cancer_tx_cost_1/res_MCED_total_diagnosed
                res_MCED_diag_cancer_tx_cost_2 <- MCED_cancer_tx_cost_2/res_MCED_total_diagnosed
                res_MCED_diag_cancer_tx_cost_3 <- MCED_cancer_tx_cost_3/res_MCED_total_diagnosed
                res_MCED_diag_cancer_tx_cost_4 <- MCED_cancer_tx_cost_4/res_MCED_total_diagnosed

                # Cancer treatment costs per shifted individuals (without OD)
                res_MCED_cancer_tx_cost_total_TS <-MCED_cancer_tx_cost_total_TS/res_MCED_total_diagnosed_TS
                res_MCED_cancer_tx_cost_1_TS <- MCED_cancer_tx_cost_1_TS/res_MCED_total_diagnosed_TS
                res_MCED_cancer_tx_cost_2_TS <- MCED_cancer_tx_cost_2_TS/res_MCED_total_diagnosed_TS
                res_MCED_cancer_tx_cost_3_TS <- MCED_cancer_tx_cost_3_TS/res_MCED_total_diagnosed_TS
                res_MCED_cancer_tx_cost_4_TS <- MCED_cancer_tx_cost_4_TS/res_MCED_total_diagnosed_TS

                # Record additional workup costs
                # Calculate misdiagnosed pts by cancer for application on misdiagnosis cost
                res_soc_cost_misdiagnosis <- 0
                res_MCED_cost_misdiagnosis <- sum(rowSums(t(t(misdiagnosis)*costs_workup_misdiagnosis[,1])*cost_growth_rates*discount_cost))/cohort_size

                # Calculate weighted average cost for FPs based on cancer incidence
                i<-1
                total_diagnosis_by_cancer <- sum(colSums(soc_markov_trace[,c(seq(3,n_cancer*4,by=4)[i]:seq(6,n_cancer*4+2,by=4)[i])]))
                for(i in 2:n_cancer){
                        total_diagnosis_by_cancer <- c(total_diagnosis_by_cancer,sum(colSums(soc_markov_trace[,c(seq(3,n_cancer*4,by=4)[i]:seq(6,n_cancer*4+2,by=4)[i])])))
                }
                cost_workup_FP <- sum(total_diagnosis_by_cancer*costs_workup_FP)/sum(total_diagnosis_by_cancer)

                res_soc_cost_FPs <- sum(res_soc_soc_screen[,'soc_FPs']*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size
                res_MCED_cost_FPs <- sum((res_MCED_soc_screen[,'soc_FPs']+res_MCED_MCED_screen[,'MCED_FPs'])*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size

                res_soc_total_cost_workup <- res_soc_cost_misdiagnosis + res_soc_cost_FPs
                res_MCED_total_cost_workup <- res_MCED_cost_misdiagnosis + res_MCED_cost_FPs

                # Record societal costs
                res_soc_societal_cost <- sum(rowSums(res_soc_societal_costs[,c(seq(1,n_cancer*4,by=1))]))
                res_MCED_societal_cost <- sum(rowSums(res_MCED_societal_costs[,c(seq(1,n_cancer*4,by=1))]))

                # Record Total Costs
                res_soc_costs_total <- res_soc_screen_cost_total + res_soc_cancer_tx_cost_total + res_soc_total_cost_workup + res_soc_societal_cost
                res_MCED_costs_total <- res_MCED_screen_cost_total + res_MCED_cancer_tx_cost_total + res_MCED_total_cost_workup + res_MCED_societal_cost

                res_cost_outcomes <-
                        rbind(
                                c(res_MCED_costs_total, res_soc_costs_total), # Total costs

                                c(res_MCED_screen_cost_total, res_soc_screen_cost_total), # Total screening costs
                                c(res_MCED_scren_cost_pre_diag, res_soc_scren_cost_pre_diag), # Pre-diagnosis Screening Costs
                                c(res_MCED_scren_cost_post_diag, res_soc_scren_cost_post_diag), # Post-diagnosis Screening Costs

                                c(res_MCED_cancer_tx_cost_total, res_soc_cancer_tx_cost_total), # Total Cancer tx costs
                                c(res_MCED_cancer_tx_cost_1, res_soc_cancer_tx_cost_1), # Cancer tx costs stage I
                                c(res_MCED_cancer_tx_cost_2, res_soc_cancer_tx_cost_2), # Cancer tx costs stage II
                                c(res_MCED_cancer_tx_cost_3, res_soc_cancer_tx_cost_3), # Cancer tx costs stage III
                                c(res_MCED_cancer_tx_cost_4, res_soc_cancer_tx_cost_4), # Cancer tx costs stage IV

                                c(res_MCED_total_cost_workup, res_soc_total_cost_workup), # Total workup costs
                                c(res_MCED_cost_misdiagnosis, res_soc_cost_misdiagnosis), # misdiagnosis costs
                                c(res_MCED_cost_FPs, res_soc_cost_FPs), # FPs costs

                                c(res_MCED_societal_cost, res_soc_societal_cost), # Societal costs

                                c(res_MCED_diag_cancer_tx_cost_total,res_soc_diag_cancer_tx_cost_total), # Total Cancer tx costs - diagnosed
                                c(res_MCED_diag_cancer_tx_cost_1, res_soc_diag_cancer_tx_cost_1), # Cancer tx costs stage I - diagnosed
                                c(res_MCED_diag_cancer_tx_cost_2, res_soc_diag_cancer_tx_cost_2), # Cancer tx costs stage II - diagnosed
                                c(res_MCED_diag_cancer_tx_cost_3, res_soc_diag_cancer_tx_cost_3), # Cancer tx costs stage III - diagnosed
                                c(res_MCED_diag_cancer_tx_cost_4, res_soc_diag_cancer_tx_cost_4) # Cancer tx costs stage IV - diagnosed
                        )
                names_res_costs_row <- c('costs_total', 'costs_total_screen','costs_screen_pre_diag','costs_screen_post_diag',
                                         'costs_total_cancer_tx', 'costs_cancer_tx_1', 'costs_cancer_tx_2', 'costs_cancer_tx_3', 'costs_cancer_tx_4',
                                         'costs_total_workup', 'costs_misdiagnosis', 'costs_FPs', 'costs_societal',
                                         'costs_diag_cancer_tx_total','costs_diag_cancer_tx_1','costs_diag_cancer_tx_2','costs_diag_cancer_tx_3','costs_diag_cancer_tx_4')
                row.names(res_cost_outcomes) <- names_res_costs_row

                # ---------- Aggregate Outcomes for Shifted Individuals -----------
                # Total number outcomes
                res_number_outcomes_TS <-
                        rbind(
                                c(res_MCED_total_diagnosed_TS, 0), #Total number diagnosed
                                c(res_MCED_diagnosed_1_TS, 0), #Total diagnosed stage I
                                c(res_MCED_diagnosed_2_TS, 0), #Total diagnosed stage II
                                c(res_MCED_diagnosed_3_TS, 0), #Total diagnosed stage III
                                c(res_MCED_diagnosed_4_TS, 0) #Total diagnosed stage IV
                        )
                names_res_number_row_TS <- c('diagnosed_total_TS', 'diagnosed_1_TS', 'diagnosed_2_TS', 'diagnosed_3_TS', 'diagnosed_4_TS')
                row.names(res_number_outcomes_TS) <- names_res_number_row_TS

                res_health_outcomes_TS <-
                        rbind(
                                c(res_MCED_LYs_post_diag_TS, 0), # Post-diagnosis LYs
                                c(res_MCED_QALYs_post_diag_TS, 0) # Post-diagnosis QALYs
                        )
                names_res_health_row_TS <- c('LYs_post_diag_TS', 'QALYs_post_diag_TS')
                row.names(res_health_outcomes_TS) <- names_res_health_row_TS

                res_cost_outcomes_TS <-
                        rbind(
                                c(res_MCED_cancer_tx_cost_total_TS,0), # Total Cancer tx costs - shifted individuals
                                c(res_MCED_cancer_tx_cost_1_TS, 0), # Cancer tx costs stage I - shifted individuals
                                c(res_MCED_cancer_tx_cost_2_TS, 0), # Cancer tx costs stage II - shifted individuals
                                c(res_MCED_cancer_tx_cost_3_TS, 0), # Cancer tx costs stage III - shifted individuals
                                c(res_MCED_cancer_tx_cost_4_TS, 0) # Cancer tx costs stage IV - shifted individuals
                        )
                names_res_costs_row_TS <- c('costs_cancer_tx_total_TS','costs_cancer_tx_1_TS','costs_cancer_tx_2_TS','costs_cancer_tx_3_TS','costs_cancer_tx_4_TS')
                row.names(res_cost_outcomes_TS) <- names_res_costs_row_TS

                #---- Updated 8/14/2023: Export total costs over time to find cost-neutral over X years
                # SoC total costs = Soc screening cost + SoC cancer treatment cost + SoC FP workup costs
                res_soc_costs_total_over_time <- (res_soc_soc_screen[,'soc_screening_costs']*cost_growth_rates_screening*discount_cost +
                                                          rowSums(res_soc_Costs[,c(seq(1,n_cancer*4,by=1))]) +
                                                          res_soc_soc_screen[,'soc_FPs']*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size
                # MCED total costs = MCED+SoC screening cost -
                #                    Overdiagnosis screening cost +
                #                    MCED cancer treatment cost +
                #                    Overdiagnosis cancer treatment cost +
                #                    MCED misdiagnosis workup costs +
                #                    MCED FP workup costs
                res_MCED_costs_total_over_time <- ((res_MCED_soc_screen[,'soc_screening_costs']+res_MCED_MCED_screen['MCED_screening_costs'])*cost_growth_rates_screening*discount_cost - res_overdiag_screen_costs +
                                                           rowSums(res_MCED_Costs[,c(seq(1,n_cancer*4,by=1))]) + rowSums(res_overdiag_costs[,c(seq(1,n_cancer*4,by=1))]) +
                                                           rowSums(t(t(misdiagnosis)*costs_workup_misdiagnosis[,1])*cost_growth_rates*discount_cost) +
                                                           (res_MCED_soc_screen[,'soc_FPs']+res_MCED_MCED_screen[,'MCED_FPs'])*cost_workup_FP*cost_growth_rates*discount_cost)/cohort_size

                res_cost_outcomes_over_time <-
                        rbind(
                                c(sum(res_MCED_costs_total_over_time[1:5,1]),sum(res_soc_costs_total_over_time[1:5])), # Total costs over 5 years
                                c(sum(res_MCED_costs_total_over_time[1:10,1]),sum(res_soc_costs_total_over_time[1:10])), # Total costs over 10 years
                                c(sum(res_MCED_costs_total_over_time[1:15,1]),sum(res_soc_costs_total_over_time[1:15])), # Total costs over 15 years
                                c(sum(res_MCED_costs_total_over_time[1:20,1]),sum(res_soc_costs_total_over_time[1:20])), # Total costs over 20 years
                                c(sum(res_MCED_costs_total_over_time[1:25,1]),sum(res_soc_costs_total_over_time[1:25])), # Total costs over 25 years
                                c(sum(res_MCED_costs_total_over_time[1:30,1]),sum(res_soc_costs_total_over_time[1:30])) # Total costs over 30 years
                        )
                names_res_costs_over_time_row <- c('total_costs_5_yr','total_costs_10_yr','total_costs_15_yr','total_costs_20_yr','total_costs_25_yr','total_costs_30_yr')
                row.names(res_cost_outcomes_over_time) <- names_res_costs_over_time_row

                #----------- Export results -----------
                res_export <- rbind(res_health_outcomes, res_number_outcomes, res_deaths_outcomes, res_cost_outcomes, res_number_outcomes_TS, res_health_outcomes_TS, res_cost_outcomes_TS, res_cost_outcomes_over_time)
                colnames(res_export) <- c('MCED+SoC','SoC')

                # ifelse(!dir.exists(file.path(paste0(export_path))), dir.create(file.path(paste0(export_path)), recursive = TRUE), FALSE)
                # write.csv(res_export, paste0(export_path, run_desc,'_','results_',iPop,'-',start_age_band,'.csv'))
                #
                # if(return_M_trace == TRUE){
                #         return(list(time_shift_markov_trace = time_shift_markov_trace, res_MCED_QALYs = res_MCED_QALYs,
                #                     res_export = res_export, soc_markov_trace = soc_markov_trace,
                #                     stage_shift_markov_trace = stage_shift_markov_trace))
                # }
                #
                #

                SoC_trace_stage[r,1:4] <- soc_markov_trace[1,3:6]
                MCED_trace_stage[r,1:4] <- time_shift_markov_trace[1,3:6]
                QALYs[r,1:4] <- res_MCED_QALYs[1,1:4]

        }

        l_res_scenarios[[s]] <- cbind(SoC_trace_stage, MCED_trace_stage, QALYs)

}

l_res_scenarios[1]  <- cbind(SoC_trace_stage, MCED_trace_stage, QALYs)
