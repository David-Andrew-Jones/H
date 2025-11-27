#' Calculate Standard of Care outcomes
#'
#' Calculates the number tested, FPs, and costs for SoC screening tests
#'
#' @param markov_trace Matrix of SoC patient state occupancy
#'
#' @return total number tested, total FPs, and total SoC screening costs
#' @examples
#' calc_soc_screen_outputs()
#' @export
calc_soc_screen_outputs <- function(markov_trace)
{
        # define no_cancer trace column
        markov_trace[0:(soc_screen_min_age-model_start_age),'No_Cancer'] <- 0 # minimum screening age
        markov_trace[(soc_screen_max_age-model_start_age+1):n_cycle,'No_Cancer'] <- 0 # maximum screening age
        no_cancer_trace <- c(cohort_size,markov_trace[,'No_Cancer'])
        no_cancer_trace <- no_cancer_trace[1:(length(no_cancer_trace)-2)] # get rid of the last 2 elements (age 100 and age 101)

        # define dataframe to be returned
        soc_screen <- as.data.frame(matrix(data = NA, nrow = n_cycle, ncol = 3))
        colnames(soc_screen) <- c('soc_number_tested','soc_FPs','soc_screening_costs')

        # SoC screenings
        soc_screen$soc_number_tested <- c(no_cancer_trace*sum(soc_screen_compliance*soc_test_eligibility),0)
        # SoC FPs
        soc_screen$soc_FPs <- c(no_cancer_trace*sum(soc_screen_compliance*soc_test_eligibility*(1 - soc_test_specificity)),0)
        # SoC Screening costs
        soc_screen$soc_screening_costs <- c(no_cancer_trace*sum(soc_screen_compliance*soc_test_eligibility*soc_test_costs),0)

        return (soc_screen)

}

#' Calculate MCED outcomes
#'
#' Calculates the number tested, FPs, and costs for MCED screening tests
#'
#' @param markov_trace Matrix of MCED patient state occupancy
#'
#' @return total number tested, total FPs, and total MCED screening costs
#' @examples
#' calc_MCED_screen_outputs()
#' @export
calc_MCED_screen_outputs <- function(markov_trace)
{
        # define no_cancer trace column
        no_cancer_trace <- c(cohort_size,markov_trace[,'No_Cancer'])
        # no_cancer_trace <- c(cohort_size, no_cancer_trace) # Add cohort_size for the first element
        no_cancer_trace <- no_cancer_trace[1:length(no_cancer_trace)-1] # get rid of the last element

        # define dataframe to be returned
        MCED_screen <- as.data.frame(matrix(data = NA, nrow = n_cycle, ncol = 3))
        colnames(MCED_screen) <- c('MCED_number_tested','MCED_FPs','MCED_screening_costs')

        # MCED screenings
        MCED_screen$MCED_number_tested <- no_cancer_trace*MCED_compliance_array
        # MCED FPs
        MCED_screen$MCED_FPs <- MCED_screen$MCED_number_tested*(1-mced_specificity)
        # MCED Screening costs
        MCED_screen$MCED_screening_costs <-MCED_screen$MCED_number_tested*mced_cost

        return (MCED_screen)
}

#' Calculate life years and QALYS gained pre-diagnosis
#'
#' Calculate life years and QALYS gained pre-diagnosis
#'
#' @param markov_trace Matrix of patient state occupancy
#'
#' @return dataframe with sum_stages, undiscounted LYs, discounted LYs, undiscounted QALYs, discounted QALYs
#' @examples
#' calc_res_pre_diag()
#' @export
calc_res_pre_diag <- function(markov_trace)
{
        # Pre-processing arrays for easy multiplication
        gen_pop_util_array <- rep(unlist(gen_pop_utility[start_age_band:max_age_band]), each=age_band_interval)
        gen_pop_util_array <- c(0.0, gen_pop_util_array)

        # define dataframe to be returned
        res_pre_diag <- as.data.frame(matrix(data = NA, nrow = n_cycle, ncol = 5))
        colnames(res_pre_diag) <- c('sum_stages','undisc_LYs','disc_LYs','undisc_QALYs','disc_QALYs')

        #Get total patient count in all stages
        res_pre_diag$sum_stages <- apply(markov_trace[,3:ncol(markov_trace)],1, function(X) sum(X))
        # Calculate undiscounted LYs
        res_pre_diag$undisc_LYs <- res_pre_diag$sum_stages*(cycle_array-1)
        # Calculate discounted LYs
        res_pre_diag$disc_LYs <- res_pre_diag$sum_stages*c(0.0,cumsum(discount_health[1:length(discount_health)-1]))
        # Calculate undiscounted QALYs
        res_pre_diag$undisc_QALYs <- res_pre_diag$sum_stages*cumsum(gen_pop_util_array[1:n_cycle])
        # Calculate discounted QALYs
        res_pre_diag$disc_QALYs <- res_pre_diag$sum_stages*cumsum(c(0.0,(discount_health[1:length(discount_health)-1]))*gen_pop_util_array[1:n_cycle])

        return (res_pre_diag)
}

#' Calculate life years and QALYS gained for never-cancer patient
#'
#' Calculate life years and QALYS by patients who never developed cancer
#'
#' @param total_diagnosed Matrix of patient state occupancy
#'
#' @return dataframe with undiscounted LYs, discounted LYs, undiscounted QALYs, discounted QALYs
#' @examples
#' calc_no_diag_health_outcomes()
#' @export
calc_no_diag_health_outcomes <- function(total_diagnosed)
{
        # # Pre-processing arrays for easy multiplication
        # gen_pop_util_array <- rep(unlist(gen_pop_utility[start_age_band:max_age_band]), each=age_band_interval)
        # # previously included code: restrict to n cycle length with the line after hashed out
        # #gen_pop_util_array <- gen_pop_util_array[1:n_cycle]
        #
        # if (n_cycle-length(gen_pop_util_array) > 1){
        #         gen_pop_util_array <- c(gen_pop_util_array, rep(unlist(gen_pop_utility[max_age_band]), n_cycle-length(gen_pop_util_array))) # pad end of array with last age band utility
        # }
        #
        # gen_pop_util_array <- gen_pop_util_array[1:n_cycle]

        # Below from original code
        # Pre-processing arrays for easy multiplication
        gen_pop_util_array <- rep(gen_pop_utility[start_age_band:max_age_band], each=age_band_interval)
        gen_pop_util_array <- c(gen_pop_util_array, rep(gen_pop_utility[max_age_band], n_cycle-length(gen_pop_util_array))) # pad end of array with last age band utility


        # define dataframe to be returned
        res_no_diag <- matrix(data = 0, nrow = n_cycle, ncol = 6)
        colnames(res_no_diag) <- c('alive','deaths','undisc_LYs','disc_LYs','undisc_QALYs','disc_QALYs')

        # define variables for first cycle
        res_no_diag[1,'alive'] <- (cohort_size - total_diagnosed)
        res_no_diag[1,'deaths'] <- res_no_diag[1,'alive']*mortality[1]
        res_no_diag[1,'alive'] <- res_no_diag[1,'alive'] - res_no_diag[1,'deaths']
        # res_no_diag[1,'undisc_LYs'] <- 1 - (res_no_diag[1,'deaths']/res_no_diag[1,'alive'])
        res_no_diag[1,'undisc_LYs'] <- ((cohort_size - total_diagnosed) - res_no_diag[1,'deaths'])/(cohort_size - total_diagnosed)

        # Estimate undiscounted LYs over time horizon
        for(i in 2:n_cycle){
                res_no_diag[i,'deaths'] <- res_no_diag[i-1,'alive']*mortality[i]
                res_no_diag[i,'alive'] <- res_no_diag[i-1, 'alive'] - res_no_diag[i,'deaths']
                # res_no_diag[i,'undisc_LYs'] <- 1 - sum(res_no_diag[1:i,'deaths'])/res_no_diag[1,'alive']
                res_no_diag[i,'undisc_LYs'] <- ((cohort_size - total_diagnosed) - sum(res_no_diag[1:i,'deaths']))/(cohort_size - total_diagnosed)
        }

        # Calculate discounted LYs, undiscounted QALYs, and discounted QALYs
        res_no_diag[,'disc_LYs'] <- res_no_diag[,'undisc_LYs']*discount_health
        res_no_diag[,'undisc_QALYs'] <- res_no_diag[,'undisc_LYs']*gen_pop_util_array
        res_no_diag[,'disc_QALYs'] <- res_no_diag[,'undisc_QALYs']*discount_health

        return(res_no_diag[,3:6])
}

#' Calculate discounted life years gained after diagnosis
#'
#' Calculate discounted life years gained by patients after cancer diagnosis
#'
#' @param surv Years lived after diagnosis
#' @param diag_age age at diagnosis
#' @param perc_iatrogenic Pre-specified percentage who receie iatrogenic harm
#' @param unit ???/
#' @param disc_rate Pre-specified health discount rate
#' @param flag_mced Whether this is for MCED or SoC scenario
#'
#' @return returns with discounted LYs
#' @examples
#' calc_res_post_diag()
#' @export
calc_res_post_diag <- function(surv,
                               diag_age,
                               perc_iatrogenic,
                               unit,
                               disc_rate,
                               flag_mced)
{
        # define variable for calculations
        sYr <- matrix(data = 0, nrow = 6)
        dYr <- sYr
        disc_unit <-dYr

        # Calculate discounted values in each year
        for(i in 1:5){
                sYr[i] <- ifelse(surv>i, 1, max(0,surv-(i-1))) # calculate survival in year
                dYr[i] <- diag_age + sum(sYr[c(1:i)]) - 1 # calculate discounted year
                disc_unit[i] <- unit[i]*sYr[i]*(1/(1+disc_rate)^dYr[i]) # calculate discounted value for year
        }
        # sYr[6] <- max(0,surv-5) # calculate survival for years 5+
        start_5p <- dYr[5]+1 # Get start time for year 5+
        end_5p <- surv+dYr[1] # Get end time for year 5+
        disc_unit[6] <- ifelse(sum(sYr)>=5, discrete_discount(unit[6],start_5p, end_5p, disc_rate), 0) # calculate discounted value for years 5+

        res_disc <- sum(disc_unit)

        # Adjust for pts experiencing iatrogenic harm
        if(booIatrogenic && perc_iatrogenic>0 && flag_mced){
                res_disc <- res_disc*(1-perc_iatrogenic)

                sYr1 <- ifelse(surv>1, 1, max(0,surv))
                dYr1 <- diag_age + sYr1
                disc_iatrogenic <- unit[1]*sYr1*(1/(1+disc_rate)^dYr1)

                res_disc <- res_disc + disc_iatrogenic*perc_iatrogenic
        }

        return(res_disc)
}

#' Calculates discounted QALYS gained after diagnosis
#'
#' Calculate discounted QALYS gained by patients after cancer diagnosis
#'
#' @param surv Years lived after diagnosis
#' @param diag_age age at diagnosis
#' @param perc_iatrogenic Pre-specified percentage who receive iatrogenic harm
#' @param unit ???/
#' @param disc_rate Pre-specified health discount rate
#' @param flag_mced Whether this is for MCED or SoC scenario
#'
#' @return returns with discounted QALYs
#' @examples
#' calc_post_diag_util()
#' @export
calc_post_diag_util <- function(
                surv,
                diag_age,
                perc_iatrogenic,
                unit,
                disc_rate,
                flag_mced,
                modifier
                )
{
        # define variable for calculations
        if(surv == 0){
                res_disc <- 0
        }else{
                sYr <- matrix(data = 0, nrow = ceiling(surv))
                dYr <- sYr
                disc_unit <-dYr
                gen_pop_util_array <- rep(unlist(gen_pop_utility[start_age_band:max_age_band]), each=age_band_interval)
                gen_pop_util_array <- gen_pop_util_array[diag_age+1:(diag_age+floor(surv)+1)]

                # Calculate discounted values in each year
                for(i in 1:ceiling(surv)){
                        sYr[i] <- ifelse(surv>i, 1, max(0,surv-(i-1))) # calculate survival in year
                        dYr[i] <- diag_age + sum(sYr[c(1:i)]) -1 # calculate discounted year
                        tempUtil <- max(gen_pop_util_array[i] - gen_pop_util_array[i] * (1-unit[min(i, 6)]) * UtilMultiplier, 0)
                        # disc_unit[i] <- sYr[i]*unit[min(i, 6)]*gen_pop_util_array[i]*(1/(1+disc_rate)^max(diag_age+i-1, 0)) # calculate discounted value for year
                        disc_unit[i] <- sYr[i]*tempUtil*(1/(1+disc_rate)^max(diag_age+i-1, 0)) # calculate discounted value for year
                }

                res_disc <- sum(disc_unit)

                # Adjust for pts experiencing iatrogenic harm
                if(booIatrogenic && perc_iatrogenic>0 && flag_mced){
                        res_disc <- res_disc*(1-perc_iatrogenic)

                        sYr1 <- ifelse(surv>1, 1, max(0,surv))
                        dYr1 <- diag_age + sYr1
                        # disc_iatrogenic <- unit[1]*sYr1*gen_pop_util_array[1]*dur_iatrogenic*(1/(1+disc_rate)^dYr1)
                        disc_iatrogenic <- util_iatrogenic * dur_iatrogenic * (1/(1+disc_rate)^dYr1)

                        res_disc <- res_disc + disc_iatrogenic*perc_iatrogenic
                }
                res_disc <- res_disc * modifier

        }
        return(res_disc)
}

#' Calculates LYs, QALYs, deaths, and costs after MCED diagnosis
#'
#' Calculates LYs, QALYs, deaths, and costs after MCED diagnosis
#'
#' @param iCycle x
#' @param iCancer x
#' @param iStage x
#' @param iLeadTime x
#' @param iPts x
#' @param res_LYs x
#' @param res_QALYs x
#' @param res_costs x
#' @param res_societal_costs x
#' @param res_deaths x
#' @param calc_year x
#' @param maxScreening x
#'
#' @return assigns to existing variables?
#' @examples
#' calc_post_diag_res_MCED()
#' @export
calc_post_diag_res_MCED <- function(iCycle,
                                    iCancer,
                                    iStage,
                                    iLeadTime,
                                    iPts,
                                    res_LYs,
                                    res_QALYs,
                                    res_costs,
                                    res_societal_costs,
                                    res_deaths,
                                    calc_year,
                                    maxScreening,
                                    didnt_shift
                                    )
{
        idx_Cycle <- iCycle - iLeadTime   # iLeadTime = iCycle - 1
        if (maxScreening == TRUE){idx_Cycle <- calc_year}
        # idx_Cycle <- iCycle - iLeadTime + 1
        cancer_idx <- (iCancer-1)*4 # Set the cancer column index for looping
        age_col_death <- idx_age_band[calc_year] # Set the age column index
        age_col <- idx_age_band[iCycle] # Set the age column index
        cancer_survival <- min(iLeadTime + unlist(survival[cancer_idx+iStage, age_col]), n_cycle - iCycle) #limit survival to maximum age of 100
        cancer_iatrogenic <- unlist(prop_iatrogenic[cancer_idx+iStage, age_col]) # set the percentage experiencing iatrogenic harm
        # print(cat("idx_Cycle:", idx_Cycle))
        # print(cat("cancer_idx:", cancer_idx))
        # print(cat("age_col_death:", age_col_death))
        # print(cat("age_col:", age_col))
        # print(cat("cancer_survival:", cancer_survival))
        # print(cat("cancer_iatrogenic:", cancer_iatrogenic))

        #Assign variables
        res_MCED_LYs <- res_LYs
        res_MCED_QALYs <- res_QALYs
        res_MCED_Costs <- res_costs
        res_MCED_societal_costs <- res_societal_costs
        res_MCED_Deaths <- res_deaths

        # Record LYs
        unit_array <- c(1,1,1,1,1,1)
        res_MCED_LYs[calc_year, cancer_idx+iStage] <- res_MCED_LYs[calc_year, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age = idx_Cycle-1,
                                   perc_iatrogenic = cancer_iatrogenic,
                                   unit = unit_array,
                                   disc_rate = disc_health,
                                   flag_mced = TRUE)

        # Record QALYs
        # df_modifier is a matrix shapped like `incidence` and `survival`. Rows are cancertype/stage, columns are age

        Modifier <- unlist(df_modifier[cancer_idx+iStage, model_start_age:100])
        Modifier <- Modifier[idx_Cycle]
        if(didnt_shift){
                Modifier <-1
        }


        unit_array <- as.numeric(util_cancer[cancer_idx+iStage,])
        res_MCED_QALYs[calc_year, cancer_idx+iStage] <- res_MCED_QALYs[calc_year, cancer_idx+iStage] + iPts*
                calc_post_diag_util(surv = cancer_survival,
                                    diag_age = idx_Cycle-1,
                                    perc_iatrogenic = cancer_iatrogenic,
                                    unit = unit_array,
                                    disc_rate = disc_health,
                                    flag_mced = TRUE,
                                    modifier = Modifier)

        # Record costs
        unit_array <- as.numeric(costs_cancer_tx[cancer_idx+iStage,])
        unit_array <- unit_array * ((1+cost_growth_rate)^(calc_year - 1))
        res_MCED_Costs[calc_year, cancer_idx+iStage] <- res_MCED_Costs[calc_year, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age = idx_Cycle-1,
                                   perc_iatrogenic = cancer_iatrogenic,
                                   unit = unit_array,
                                   disc_rate = disc_cost,
                                   flag_mced = TRUE)

        # Record Societal costs
        unit_array <- as.numeric(rep(costs_societal[iCancer,iStage],6))
        res_MCED_societal_costs[calc_year, cancer_idx+iStage] <- res_MCED_societal_costs[calc_year, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age = idx_Cycle-1,
                                   perc_iatrogenic = cancer_iatrogenic,
                                   unit = unit_array,
                                   disc_rate = disc_cost,
                                   flag_mced = TRUE)

        # Record deaths
        deaths <- unlist(deaths_5y[cancer_idx+iStage, age_col_death])
        res_MCED_Deaths[calc_year, cancer_idx+iStage] <- res_MCED_Deaths[calc_year, cancer_idx+iStage] + iPts*deaths

        # store updated dataframes in memory
        # Old code uses assign...
        # assign("res_MCED_LYs",res_MCED_LYs, pos=1)
        # assign("res_MCED_QALYs",res_MCED_QALYs, pos=1)
        # assign("res_MCED_Costs",res_MCED_Costs, pos=1)
        # assign("res_MCED_societal_costs",res_MCED_societal_costs, pos=1)
        # assign("res_MCED_Deaths",res_MCED_Deaths, pos=1)

        # Instead use return and change f_run_model with assign operator
        return(list(res_MCED_LYs, res_MCED_QALYs, res_MCED_Costs, res_MCED_societal_costs, res_MCED_Deaths))

}

#' Calculates LYs, QALYs, deaths, and costs after MCED diagnosis for time shifted individuals
#'
#' Calculates LYs, QALYs, deaths, and costs after MCED diagnosis for time shifted individuals
#'
#' @param iCycle x
#' @param iCancer x
#' @param iStage x
#' @param iLeadTime x
#' @param iPts x
#' @param res_LYs_TS x
#' @param res_QALYs_TS x
#' @param res_costs_TS x
#' @param calc_year x
#' @param maxScreening x
#' @param cost_growth_rate x
#'
#' @return assigns to existing variables?
#' @examples
#' calc_post_diag_res_MCED_TS()
#' @export
calc_post_diag_res_MCED_TS <- function(iCycle,
                                       iCancer,
                                       iStage,
                                       iLeadTime,
                                       iPts,
                                       res_LYs_TS,
                                       res_QALYs_TS,
                                       res_costs_TS,
                                       calc_year,
                                       maxScreening,
                                       didnt_shift
                                       )
{
        idx_Cycle <- iCycle - iLeadTime   # iLeadTime = iCycle - 1
        if (maxScreening == TRUE){idx_Cycle <- calc_year}
        # idx_Cycle <- iCycle - iLeadTime + 1
        cancer_idx <- (iCancer-1)*4 # Set the cancer column index for looping
        age_col_death <- idx_age_band[calc_year] # Set the age column index
        age_col <- idx_age_band[iCycle] # Set the age column index
        cancer_survival <- min(iLeadTime + survival[cancer_idx+iStage, age_col], n_cycle - iCycle) #limit survival to maximum age of 100
        cancer_iatrogenic <- prop_iatrogenic[cancer_idx+iStage, age_col] # set the percentage experiencing iatrogenic harm

        # addition 07/05/2025
        #modifier <- min(iLeadTime + survival[cancer_idx+iStage, age_col], n_cycle - iCycle) #limit survival to maximum age of 100

        #Assign variables
        res_MCED_LYs_TS <- res_LYs_TS
        res_MCED_QALYs_TS <- res_QALYs_TS
        res_MCED_Costs_TS <- res_costs_TS

        # Record LYs
        unit_array <- c(1,1,1,1,1,1)
        res_MCED_LYs_TS[calc_year, cancer_idx+iStage] <- res_MCED_LYs_TS[calc_year, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age = idx_Cycle-1,
                                   perc_iatrogenic = cancer_iatrogenic,
                                   unit = unit_array,
                                   disc_rate = disc_health,
                                   flag_mced = TRUE)

        # Record QALYs
        Modifier <- unlist(df_modifier[cancer_idx+iStage, model_start_age:100])
        Modifier <- Modifier[idx_Cycle]
        if(didnt_shift){
                Modifier <-1
        }

        unit_array <- as.numeric(util_cancer[cancer_idx+iStage,])
        res_MCED_QALYs_TS[calc_year, cancer_idx+iStage] <- res_MCED_QALYs_TS[calc_year, cancer_idx+iStage] + iPts*
                calc_post_diag_util(surv = cancer_survival,
                                    diag_age = idx_Cycle-1,
                                    perc_iatrogenic = cancer_iatrogenic,
                                    unit = unit_array,
                                    disc_rate = disc_health,
                                    flag_mced = TRUE,
                                    modifier = Modifier)
        # Record costs
        unit_array <- as.numeric(costs_cancer_tx[cancer_idx+iStage,])
        unit_array <- unit_array * ((1+cost_growth_rate)^(calc_year - 1))
        res_MCED_Costs_TS[calc_year, cancer_idx+iStage] <- res_MCED_Costs_TS[calc_year, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age = idx_Cycle-1,
                                   perc_iatrogenic = cancer_iatrogenic,
                                   unit = unit_array,
                                   disc_rate = disc_cost,
                                   flag_mced = TRUE)

        # store updated dataframes in memory
        # assign("res_MCED_LYs_TS",res_MCED_LYs_TS, pos=1)
        # assign("res_MCED_QALYs_TS",res_MCED_QALYs_TS, pos=1)
        # assign("res_MCED_Costs_TS",res_MCED_Costs_TS, pos=1)

        return(list(res_MCED_LYs_TS, res_MCED_QALYs_TS, res_MCED_Costs_TS))


}

#' Calculates QALYS and costs associated with overdiagnosis
#
#' Calculates QALYS and costs associated with overdiagnosis
#'
#' @param iCycle x
#' @param iCancer x
#' @param iStage x
#' @param mean_time_shift x
#' @param iLeadTime x
#' @param iPts x
#' @param res_costs x
#' @param res_QALYs x
#' @param res_screen x
#'
#' @return assigns to existing variables?
#' @examples
#' calc_post_diag_res_overdiag()
#' @export
calc_post_diag_res_overdiag <- function(iCycle,
                                        iCancer,
                                        iStage,
                                        mean_time_shift,
                                        iLeadTime,
                                        iPts,
                                        res_costs,
                                        res_QALYs,
                                        res_screen
                                        )
{
        idx_Cycle <- iCycle
        cancer_idx <- (iCancer-1)*4 # Set the cancer column index for looping
        age_col <- idx_age_band[iCycle] # Set the age column index
        cancer_survival <- ceiling(mean_time_shift)
        calcYear = max(rnd(iCycle - mean_time_shift), (mced_screen_min_age-model_start_age+1))
        if (calcYear <= mced_screen_min_age-model_start_age+1) {cancer_survival <- max(iCycle - (mced_screen_min_age-model_start_age+1), 0)}

        # Assign variables
        res_overdiag_costs <- res_costs
        res_overdiag_QALYs <- res_QALYs
        res_overdiag_screen_costs <- res_screen

        # Record QALYs
        Modifier <- 1

        unit_array <- as.numeric(util_cancer[cancer_idx+iStage,])
        tempQALY <- iPts* calc_post_diag_util(surv = cancer_survival,
                                              diag_age = calcYear-1,
                                              perc_iatrogenic = 0,
                                              unit = c(1, 1, 1, 1, 1, 1),
                                              disc_rate = disc_health,
                                              flag_mced = TRUE,
                                              modifier = Modifier) -
                iPts*calc_post_diag_util(surv = cancer_survival,
                                         diag_age = calcYear-1,
                                         perc_iatrogenic = 0,
                                         unit = unit_array,
                                         disc_rate = disc_health,
                                         flag_mced = TRUE,
                                         modifier = Modifier)

        res_overdiag_QALYs[calcYear, cancer_idx+iStage] <- res_overdiag_QALYs[calcYear, cancer_idx+iStage] + tempQALY

        # Record costs
        unit_array <- as.numeric(costs_cancer_tx[cancer_idx+iStage,])
        unit_array <- unit_array * ((1+cost_growth_rate)^(calcYear - 1))
        res_overdiag_costs[calcYear, cancer_idx+iStage] <- res_overdiag_costs[calcYear, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age= calcYear-1,
                                   perc_iatrogenic = 0,
                                   unit = unit_array,
                                   disc_rate = disc_cost,
                                   flag_mced = TRUE)

        # Record screening costs
        unit_array <- as.numeric(rep(mced_cost, 6))
        res_overdiag_screen_costs[calcYear, cancer_idx+iStage] <- res_overdiag_screen_costs[calcYear, cancer_idx+iStage] + iPts*
                calc_res_post_diag(surv = cancer_survival,
                                   diag_age= calcYear-1,
                                   perc_iatrogenic = 0,
                                   unit = unit_array,
                                   disc_rate = disc_cost,
                                   flag_mced = TRUE)

        # store updated dataframes in memory
        # assign("res_overdiag_costs",res_overdiag_costs, pos=1)
        # assign("res_overdiag_QALYs",res_overdiag_QALYs, pos=1)
        # assign("res_overdiag_screen_costs",res_overdiag_screen_costs, pos=1)

        return(list(res_overdiag_costs, res_overdiag_QALYs, res_overdiag_screen_costs))


}

#' Calculates discretely discounted area
#'
#' Calculate discretely discounted area
#'
#' @param start_value x
#' @param start_time x
#' @param end_time x
#' @param disc_rate x
#'
#' @return discounted area
#' @examples
#' discrete_discount()
#' @export
discrete_discount <- function(start_value, start_time, end_time, disc_rate)
{
        disc_result <- sum(
                ifelse((end_time-start_time)>=1,sum(start_value*(1/((1+disc_rate)^(start_time+seq(length(start_time:end_time)-1)-1)))), 0), # discounted rate for "whole numbered" years
                ifelse(end_time>5,start_value*(end_time-floor(end_time))*(1/((1+disc_rate)^(floor(end_time)))), 0) # discounted result for the last "fractional" year
        )

        return(disc_result)
}

#' Custom round function
#'
#' Custom round function
#'
#' @param x numeric vector
#'
#' @return rounded numeric vector
#' @examples
#' rnd()
#' @export
rnd <- function(x) trunc(x+sign(x)*0.5)

#' Random sample from beta distribution
#'
#' Random draws from a beta distribution using mean (p1) and either SD/SE or sample size (p2) as arguments
#'
#' @param n number of samples
#' @param p1 mean
#' @param p2 sample size or SD/SE
#'
#' @return rounded numeric vector
#' @examples
#' f_rbeta()
#' @export
f_rbeta <- function (n, p1, p2) {
        if (p2<1) { # p2 is not a sample size, so it's an SE
                if (p2>0) {
                        # tmp <- max(p1*(1-p1)/p2^2-1,0)
                        tmp <- p1*(1-p1)/p2^2-1
                        a <- p1*tmp
                        b <- (1-p1)*tmp
                        res <- rbeta(n, a, b)
                        res[is.na(res)] <- p1 # replace NAs with mean
                        return(res)
                } else {
                        return(rep(p1, n))
                }
        } else { # SE would be lower than 1, so p2 is a sample size
                a <- p1*p2
                b <- (1-p1)*p2
                return(rbeta(n, a, b))
        }
}

#' Random sample from beta distribution constrained by values in samples_high
#'
#' Random draws from a beta distribution constrained by values in samples_high
#'
#' @param n x
#' @param p x
#' @param ss x
#' @param p_high x
#' @param ss_high x
#' @param samples_high x
#'
#' @return rounded numeric vector
#' @examples
#' f_rbeta_constrained()
#' @export
f_rbeta_constrained <- function (n, p, ss, p_high, ss_high, samples_high) {
        if (p>0 & p_high>0) {
                if (p_high>p) {
                        a <- p*ss
                        b <- (1-p)*ss
                        v <- a*b/((a+b)^2*(a+b+1))
                        a_high <- p_high*ss_high
                        b_high <- (1-p_high)*ss_high
                        v_high <- a_high*b_high/((a_high+b_high)^2*(a_high+b_high+1))

                        aux_p <- p/p_high
                        aux_v <- max((v-v_high*aux_p^2)/(v+p_high^2), 0.00000001)

                        aux_a <- aux_p*(aux_p*(1*aux_p)/aux_v-1)
                        aux_b <- (1-aux_p)*(aux_p*(1*aux_p)/aux_v-1)
                        aux_samp <- rbeta(n, aux_a, aux_b)

                        return(aux_samp*samples_high[1:n])
                } else {
                        return(samples_high[1:n])
                }

        } else {
                return(rep(0,n))
        }
}


#' Random sample from gamma distribution
#'
#' Random draws from a beta distribution using using mean and SD/SE as arguments
#'
#' @param n number of samples
#' @param m mean
#' @param s SD/SE
#'
#' @return rounded numeric vector
#' @examples
#' f_rgamma()
#' @export
f_rgamma <- function(n, m, s) {
        if (m>0) {
                b <- m/s^2
                a <- m*b
                return(rgamma(n, a, b))
        } else {
                return(rep(0, n))
        }
}

