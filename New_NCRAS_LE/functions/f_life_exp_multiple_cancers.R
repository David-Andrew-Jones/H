#' Modal life expectancy using M-spline
#'
#' Modal life expectancy using M-spline
#'
#' @param survival list of dataframe
#' @param include_additive_hzds True or False to include background hazard and use additive hazards framework
#' @param include_cure_fraction True or False to use mixture-cure model
#' @param plotsurvhz True or False to output plots
#' @param survextrap_add_knotts_interval Knots arguement to feed through to survextrap function
#' @param survextrap_fit_method Fit method arguement to feed through to survextrap function
#' @param survextrap_niter number of iterations for MCMC to feed through to survextrap function
#'
#' @return RMST
#' @examples
#' f_life_exp_multiple_cancers()
#' @export
f_life_exp_multiple_cancers <- function(survival,
                                        include_additive_hzds,
                                        include_cure_fraction,
                                        plotsurvhz,
                                        survextrap_add_knotts_interval,
                                        survextrap_fit_method,
                                        survextrap_niter)
{

        #' The survival dataframe here covers all the specified cancer types
        survival <- survival %>%
                #' The below case_when is needed only for the the MM net survival data names.
                #' the [ and /causes problems for string matching below
                mutate(NCRAS_Draw = case_when(NCRAS_Draw == "[OTHER]" ~ "Other",
                                              NCRAS_Draw == "Colon/Rectum" ~ "Colorectal",
                                              NCRAS_Draw == "Liver/Bile-duct" ~ "Liver",
                                              .default = NCRAS_Draw)) %>%

                mutate(across(where(is.factor), as.character)) %>%
                mutate(across(where(is.character), as.factor))

        #' get the cancer type names to be used later
        v_cancer <- levels(survival$NCRAS_Draw)

        get_age <- as.numeric(substr(survival$Age[1], start = 1, stop = 2)) + 2.5
        get_sex <- as.character(survival$Sex[1])


        if(include_additive_hzds){

                get_bckgrnd_hz <- df_gen_pop_hz %>%
                        filter(age >= get_age) %>%
                        filter(sex == get_sex) %>%
                        mutate(time = row_number() - 1) %>%
                        select(time, hazard)
        }

        if(is.na(survextrap_add_knotts_interval)){

                knotts = NULL
        } else {
                knotts = seq.int(from = 10, to = 100 - get_age, by = survextrap_add_knotts_interval)
        }

        #' Time-to-event data reconstruction
        #' First, create a function to perform getting survival times for a cancer type
        internal_f_surv_times <- function(data){

                surv_times <- bind_rows(data.frame(stop = 0, CSS_HR_adjusted = 1), data %>%
                                                select(stop, CSS_HR_adjusted)) %>%
                        rename(tk = stop,
                               sk = CSS_HR_adjusted) %>%
                        mutate(k = row_number()) %>%
                        select(k, tk, sk)

        }

        #' Next, split the survival dataframe into seperate cancer types and map across the previous function.
        #' creates a list of dataframes, 1 for each cancer type
        combined_surv_times <- map(.x = split(survival, list( survival$NCRAS_Draw)),
                                   .f =  ~internal_f_surv_times(data = .x))

        #' Write the survival files for the digitise function seperately for each cancer type
        combined_surv_times %>%
                names(.) %>%
                map(~ write.table(combined_surv_times[[.]], paste0(specify_res_save_path, "surv_times_cancom", .x ,".txt"), sep = "\t",
                                  row.names = FALSE))

        #' Next, create a function to generate the n risk tables for a cancer type
        internal_f_n_risk <- function(data){

                surv_nrisk <- data %>%
                        filter(start %in% c(0,1,2,3,4,4.75)) %>%
                        rename(trisk = start,
                               nrisk = n_patients) %>%
                        mutate(i = row_number()) %>%
                        select(i, trisk, nrisk ) %>%
                        mutate(lower = c(1,9,13,17,21,25),
                               upper = c(8,12,16,20,24,25)) %>%
                        select(i, trisk, lower, upper, nrisk)

        }

        #' Split, the survival dataframe into seperate cancer types again and map across the previous function.
        #' Creates a list of dataframes, 1 for each cancer type
        combined_nrisk <- map(.x = split(survival, list( survival$NCRAS_Draw)),
                              .f =  ~internal_f_n_risk(data = .x))

        #' Write the nrisk files for the digitise function
        combined_nrisk %>%
                names(.) %>%
                map(~ write.table(combined_nrisk[[.]], paste0(specify_res_save_path, "surv_nrisk_cancom", .x ,".txt"), sep = "\t",
                                  row.names = FALSE))

        #' Load back in files. Intersect used here as there may be other cancer type files in the folder from previous runs
        #' that we dont want included in this caluclations

        l_files_nrisk <- intersect(list.files(path = specify_res_save_path, pattern = "surv_nrisk_cancom"),
                                   list.files(path = specify_res_save_path, paste0(v_cancer, collapse="|")))

        l_file_surv_time <- intersect(list.files(path = specify_res_save_path, pattern = "surv_times_cancom"),
                                      list.files(path = specify_res_save_path, paste0(v_cancer, collapse="|")))

        #' Next, create a function to generate the reconstructed KM and time to event data separately each cancer type
        #' (saves to file)
        internal_f_digitise <- function(surv_string, nrisk_string, name_string){

                survHE::digitise(surv_inp = paste0(specify_res_save_path, surv_string),
                                 nrisk_inp = paste0(specify_res_save_path, nrisk_string),
                                 km_output = paste0(specify_res_save_path, "surv_KMdata_", name_string ,".txt"),
                                 ipd_output = paste0(specify_res_save_path, "surv_IPDdata_", name_string ,".txt"))
        }

        #' map the surv time and nrisk files over the above function to create the reconstructed
        #' time to event data for each cancer type
        pmap(list(l_file_surv_time, l_files_nrisk, v_cancer),  internal_f_digitise)

        #' Load back in the created time to event data and combine! The cancer types are now all combined
        surv_IPDdata <- map_df(intersect(list.files(path = specify_res_save_path, pattern = "IPDdata_"),
                                         list.files(path = specify_res_save_path, paste0(v_cancer, collapse="|"))),
                               ~read.delim(paste0(specify_res_save_path, .x)))

        #' Do survival extrapolation as in f_life_exp()
        mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = surv_IPDdata, add_knots = knotts)

        if(include_additive_hzds){

                if(include_cure_fraction){

                        model <- survextrap::survextrap(Surv(time, event) ~ 1,
                                                        data=surv_IPDdata, mspline=mspline, backhaz = get_bckgrnd_hz,
                                                        cure = TRUE, fit_method = survextrap_fit_method)
                        print("Model with backhaz and cure probability")

                } else {

                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                                        fit_method = survextrap_fit_method)
                        print("Model with backhaz")
                }

        } else {

                model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                mspline=mspline, fit_method = survextrap_fit_method)
                print("Model on observations only")
        }

        if(include_additive_hzds){

                rmst_mod <- rmst(model, t = 100 - get_age, niter = survextrap_niter)
        } else {

                rmst_mod <- rmst(model, t = 12, niter = survextrap_niter)
        }


        if(plotsurvhz){

                plot_surv <- survextrap::plot_survival(model, tmax = 100 - get_age, niter = survextrap_niter)
                plot_hz <- survextrap::plot_hazard(model, show_knots = TRUE, tmax = 100 - get_age, niter = survextrap_niter)

                return(list(rmst_mod, plot_surv, plot_hz, model))

        } else {

                return(rmst_mod)
        }

}

