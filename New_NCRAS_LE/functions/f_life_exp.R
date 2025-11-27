#' Modal life expectancy using M-spline
#'
#' Modal life expectancy using M-spline
#'
#' @param survival Data frame input
#' @param include_additive_hzds True or False to include background hazard and use additive hazards framework
#' @param include_cure_fraction True or False to use mixture-cure model
#' @param plotsurvhz True or False to output plots
#' @param survextrap_add_knotts_interval Knots arguement to feed through to survextrap function
#' @param survextrap_fit_method Fit method arguement to feed through to survextrap function
#' @param survextrap_niter number of iterations for MCMC to feed through to survextrap function
#'
#' @return RMST
#' @examples
#' f_life_exp
#' @export
f_life_exp <- function(survival,
                       aggregate_surv,
                       include_additive_hzds,
                       include_cure_fraction,
                       include_Loo,
                       plotsurvhz,
                       survextrap_add_knotts_interval,
                       survextrap_fit_method,
                       survextrap_niter)
{

        #' Get age and sex of patient presentation
        get_age <- as.numeric(substr(survival$Age[1], start = 1, stop = 2)) + 2.5
        get_sex <- as.character(survival$Sex[1])

        #' Then get related general population mortality.
        #' include_additive_hzds added as an argument in case we want to look at the case
        #' when we're extrapolated without. But in the LE analysis we do want the background
        #' mortality
        if(include_additive_hzds){

                get_bckgrnd_hz <- df_gen_pop_hz %>%
                        filter(age >= get_age) %>%
                        filter(sex == get_sex) %>%
                        mutate(time = row_number() - 1) %>%
                        select(time, hazard)
        }

        #' Bit of extra functionality but not used in this analysis
        if(is.na(survextrap_add_knotts_interval)){

                knotts = NULL
        } else {
                knotts = seq.int(from = 10, to = 100 - get_age, by = survextrap_add_knotts_interval)
        }

        if(aggregate_surv){

                #' If TRUE, Put survival into right format and naming for the survextrap()
                #' `external` argument (Start time | End time | Alive at | Still alive at )
                #' NOTE THIS WILL NEED CHANGING DEPENDING ON FORMAT OF NEW DATA

                survival <- survival %>%
                        mutate(`r` = lead(n_patients), .after = n_patients) %>%
                        rename(`n` = n_patients) %>%
                        filter(stop != max(stop) )

        } else {
                #' If FALSE, here, the aggregate survival data is reconstructed into time to event data using the Guyot (2012)
                #' Which is implemented using the survHE package (Gianluca Baio)
                #' This was done because of the structure of the aggregate net survival data, which gives time interval,
                #' number of patients, and net survival - this may not be needed with the overall survival data, as that should
                #' be able to go straight into the external argument of the survextrap() function

                #' For the Guyot routine, two inputs are needed. Firstly, "The first input data file
                #' required for the algorithm contains the extracted x-axis coordinates,
                #' T k , and y-axis coordinates, S k , for k = 1,..., N points on the KM curve."
                #' These are the time of the kinks for the "KM" curve,
                #' and the survival at the bottom of the kink. A row is given a row number for the next step
                #' The 5-y net survival data extract is in this form
                surv_times <- bind_rows(data.frame(stop = 0, CSS_HR_adjusted = 1), survival %>%
                                                select(stop, CSS_HR_adjusted)) %>%
                        rename(tk = stop,
                               sk = CSS_HR_adjusted) %>%
                        mutate(k = row_number()) %>%
                        select(k, tk, sk)

                #' Secondly: "The second input data file required for the algorithm contains
                #'  information on the reported numbers at risk.
                #'  The curve is split into i = 1,.., nint intervals,
                #'  for each we have the reported number at risk at the start of that interval,
                #'  nrisk i , the time at which the number at risk is provided, trisk i ,
                #'  the first row number of the extracted co-ordinates for that time interval lower i ,
                #'  and the last row number of the extracted co-ordinates for that time interval upper i ."
                surv_nrisk <- survival %>%
                        filter(start %in% c(0,1,2,3,4,4.75)) %>%
                        rename(trisk = start,
                               nrisk = n_patients) %>%
                        mutate(i = row_number()) %>%
                        select(i, trisk, nrisk ) %>%
                        mutate(lower = c(1,9,13,17,21,25),
                               upper = c(8,12,16,20,24,25)) %>%
                        select(i, trisk, lower, upper, nrisk)

                #' Write these out for the survHE function "digitise" which performs the Guyot algorithm
                #' and creates the recontructed time to event data time to event
                write.table(surv_times, file = paste0(specify_res_save_path, "surv_times.txt"), sep = "\t",
                            row.names = FALSE)
                write.table(surv_nrisk, file = paste0(specify_res_save_path, "surv_nrisk.txt"), sep = "\t",
                            row.names = FALSE)

                survHE::digitise(surv_inp = paste0(specify_res_save_path, "surv_times.txt"),
                                 nrisk_inp = paste0(specify_res_save_path, "surv_nrisk.txt"),
                                 km_output = paste0(specify_res_save_path, "surv_KMdata.txt"),
                                 ipd_output = paste0(specify_res_save_path, "surv_IPDdata.txt"))

                surv_IPDdata <- read.delim(paste0(specify_res_save_path, "surv_IPDdata.txt"))

                #' Create the basic M-Spline model for the survextrap function
                mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = surv_IPDdata, add_knots = knotts)
        }

        #' Create survival object based on args selected

        if(aggregate_surv){
                if(include_additive_hzds){
                        if(include_cure_fraction){
                                if(include_Loo){

                                        model <- survextrap::survextrap(formula = ~ 1, external = survival,
                                                                         backhaz = get_bckgrnd_hz,
                                                                        cure = TRUE, fit_method = survextrap_fit_method)
                                        print("Model based on aggregate survival, backhaz, cure probability and, LOO")

                                } else {

                                        model <- survextrap::survextrap(formula = ~ 1, external = survival,
                                                                        backhaz = get_bckgrnd_hz,
                                                                        cure = TRUE, fit_method = survextrap_fit_method, loo = FALSE)
                                        print("Model based on aggregate survival, backhaz, and cure probability")

                                }

                        } else if(include_Loo){

                                model <- survextrap::survextrap(formula = ~ 1, external = survival,
                                                                backhaz = get_bckgrnd_hz,
                                                                cure = FALSE, fit_method = survextrap_fit_method)
                                print("Model based on aggregate survival, backhaz, and LOO")


                        } else {

                                model <- survextrap::survextrap(formula = ~ 1, external = survival,
                                                                backhaz = get_bckgrnd_hz,
                                                                cure = FALSE, fit_method = survextrap_fit_method, loo = FALSE)
                                print("Model based on aggregate survival, and backhaz")
                        }

                } else {

                        model <- survextrap::survextrap(formula = ~ 1, external = survival,
                                                        fit_method = survextrap_fit_method)
                        print("Model on observations only")
                }
        } else {
                if(include_additive_hzds){
                        if(include_cure_fraction){
                                if(include_Loo){

                                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                        cure = TRUE, fit_method = survextrap_fit_method)
                                        print("Model based on recontructed IPD, backhaz, cure probability and, LOO")

                                } else {

                                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                                        mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                        cure = TRUE, fit_method = survextrap_fit_method, loo = FALSE)
                                        print("Model based on recontructed IPD, backhaz, and cure probability")

                                }

                        } else if(include_Loo){

                                model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                                mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                cure = FALSE, fit_method = survextrap_fit_method)
                                print("Model based on recontructed IPD, backhaz, and LOO")


                        } else {

                                model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                                mspline=mspline, backhaz = get_bckgrnd_hz,
                                                                cure = FALSE, fit_method = survextrap_fit_method, loo = FALSE)
                                print("Model based on recontructed IPD, and backhaz")
                        }

                } else {

                        model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                        mspline=mspline, fit_method = survextrap_fit_method)
                        print("Model on recontructed IPD")
                }

        }

        #__________________________________#
        #' specify M-spline model based on options
        if(include_additive_hzds){

                rmst_mod <- rmst(model, t=100 - get_age, niter = survextrap_niter)
        } else {

                rmst_mod <- rmst(model, t=12, niter = survextrap_niter)
        }

        #' Choose plot options
        if(plotsurvhz){

                plot_surv <- survextrap::plot_survival(model, tmax = 100 - get_age, niter = survextrap_niter)
                plot_hz <- survextrap::plot_hazard(model, show_knots = TRUE, tmax = 100 - get_age, niter = survextrap_niter)

                return(list(rmst_mod, plot_surv, plot_hz, model))

        } else {

                return(rmst_mod)
        }


}

