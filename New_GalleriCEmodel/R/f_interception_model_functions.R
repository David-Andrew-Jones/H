#' Calculates
#'
#' Calculates ... based on the effective sensitivity, i.e taking into account
#'
#' @param cumulative_sens x
#' @param dwell_detect_rate x
#'
#' @return returns list(intercept = detect, clinical = live, parrive = arrive, plive = live, pdetect = detect, pmiss = miss)
#' @examples
#' effective_sens()
#' @export

effective_sens<-function(
                cumulative_sens,
                dwell_detect_rate){
        detect <- rep(0, length(cumulative_sens))
        miss <- 0
        i <- 1
        arrive <- cumulative_sens[i]
        live <- arrive + miss

        detect[1] <- cumulative_sens[i] * dwell_detect_rate[i]
        miss <- cumulative_sens[i] * (1-dwell_detect_rate[i])

        if (length(cumulative_sens) > 1){
                for (i in 2:length(cumulative_sens))
                {
                        #newly detectable cases
                        arrive <- cumulative_sens[i]-cumulative_sens[i-1]
                        live <- arrive+miss #currently detectable is newly detectable + missed at earlier stages
                        detect[i] <- live*dwell_detect_rate[i] #would detect all of them, but miss some of them due to timing
                        miss< - live*(1-dwell_detect_rate[i])
                }
        }
        arrive <- 1 - cumulative_sens[i] #final miss
        live = arrive + miss
        # print(arrive)
        #detect[1-4] is detected at each stage
        #clinical detect= miss[4]
        list(intercept = detect,
             clinical = live,
             parrive = arrive,
             plive = live,
             pdetect = detect,
             pmiss = miss)
}

#' Calculates event rate for interval
#'
#' Estimates the rate when a screening event happens within randomly distributed length of time
#'
#' @param MC_shape_dwell_time_dist Monte Carlo generation of the shape of the distribution of the dwell time, based on exp dist
#' @param event_rate_dwell_interval Screening interval divided by the dwell time
#'
#' @return Scalar, rate of screening event for a given length of time
#' @examples
#' rate_given_exptimedist_dwell_screeninginterval()
#' @export
rate_given_exptimedist_dwell_screeninginterval <- function(
                MC_shape_dwell_time_dist,
                event_rate_dwell_interval){

        prop_beforeorat <- sum(pmin(MC_shape_dwell_time_dist, event_rate_dwell_interval)) / length(MC_shape_dwell_time_dist)
        prop_beforeorat / event_rate_dwell_interval

}

#' Calculates stage shift matrix
#'
#' Executes InterceptModel calculations to get stage shift matrix
#'
#' @param param_dwell_times dataframe of dwell times in years by cancer and by stage
#' @param param_screen_interval interval between MCED tests, e.g years
#' @param param_sensitivity dataframe of MCED test sensitivity by cancer and by stage
#' @param param_incidence dataframe of annual cancer incidence rates
#'
#' @return returns stage shift matrix for all cancers in 4x4 format
#' @examples
#' calc_stage_shift_matrix()
#' @export
calc_stage_shift_matrix<-function(
                param_dwell_times,
                param_screen_interval,
                param_sensitivity,
                param_incidence){
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

        #### DJ re-written below as catch_rate_for_k as seperate function rate_given_exptimedist_dwell_screeninginterval
        #--------------------------build_slip_rate_scenario_table.R---------------------
        #generate slip rate for a given scenario
        #export to file as a save


        #do it tidy style
        #this function estimates the rate when a screening event happens within a
        # randomly distributed length of time
        # given the examples tf
        # catch_rate_for_k<- function(tf, k){
        #         tt <- sum(pmin(tf, k)) / length(tf)
        #         tt / k
        # }



        # dwell_times <- read_csv(paste0(excel2R_dir,'dwell_times.csv'), col_names = F, show_col_types = F) #TODO: Update directory location
        dwell_times <- param_dwell_times

        dwell_model_all_df <- cbind(cancer_names, dwell_times, xStage, number_stage)
        names(dwell_model_all_df) <- c('Cancer', 'dwell','Stage', 'number_stage')

        # write.csv(dwell_model_all_df, "dwellTimes.csv")
        #monte carlo generate shape of dwell time distribution
        ss = 1
        set.seed(19680128)
        tfan <- rweibull(10000, shape = ss, scale = 1)

        tfan <- tfan / mean(tfan)

        screen_interval <- param_screen_interval #initially 1

        #slip is "before clinical"
        #slip_clinical = "at stage of clinical detection"
        #completeness in modeling

        #### Re-written below to include rate_given_exptimedist_dwell_screeninginterval instead of catch_rate_for_k
        # dwell_slip_df <- dwell_model_all_df %>%
        #         mutate(slip = sapply(dwell, function(z) {
        #                 1 - catch_rate_for_k(tfan, screen_interval / z)
        #                 }),
        #                slip_clinical = sapply(dwell*0.5, function(z){   # assumption of half dwell time
        #                        1 - catch_rate_for_k(tfan, screen_interval / z)
        #                        }))

        dwell_slip_df <- dwell_model_all_df %>%
                mutate(slip = sapply(dwell, function(z) {
                        1 - rate_given_exptimedist_dwell_screeninginterval(tfan, screen_interval / z)
                }),
                slip_clinical = sapply(dwell*0.5, function(z){   # assumption of half dwell time
                        1 - rate_given_exptimedist_dwell_screeninginterval(tfan, screen_interval / z)
                }))


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
