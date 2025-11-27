#' Modal life expectancy using M-spline
#'
#' Modal life expectancy using M-spline
#'
#' @param survival list of dataframe
#' @param include_additive_hzds True or False to include background hazard and use additive hazards framework
#' @param plotsurvhz True or False to output plots
#' @param survextrap_add_knotts_interval Knots arguement to feed through to survextrap function
#' @param survextrap_fit_method Fit method arguement to feed through to survextrap function
#' @param survextrap_niter number of iterations for MCMC to feed through to survextrap function
#'
#' @return RMST
#' @examples
#' f_life_exp_from_5year_netsurv()
#' @export
f_life_exp_from_5year_net_surv_multiple_cancers <- function(survival,
                                          include_additive_hzds,
                                          plotsurvhz,
                                          survextrap_add_knotts_interval,
                                          survextrap_fit_method,
                                          survextrap_niter)
{

        ## start
        survival <- survival %>%
                mutate(across(where(is.factor), as.character)) %>%
                mutate(across(where(is.character), as.factor))

        # Cancers for later
        v_cancer <- levels(survival$NCRAS_Draw) %>% substr(start = 1, stop = 5)

        get_age <- as.numeric(substr(survival$Age[1], start = 1, stop = 2)) + 3
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

        internal_f_surv_times <- function(data){

                surv_times <- bind_rows(data.frame(stop = 0, CSS_HR_adjusted = 1), data %>%
                                                select(stop, CSS_HR_adjusted)) %>%
                        rename(tk = stop,
                               sk = CSS_HR_adjusted) %>%
                        mutate(k = row_number()) %>%
                        select(k, tk, sk)

        }

        combined_surv_times <- map(.x = split(survival, list( survival$NCRAS_Draw)),
                                   .f =  ~internal_f_surv_times(data = .x))


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

        combined_nrisk <- map(.x = split(survival, list( survival$NCRAS_Draw)),
                              .f =  ~internal_f_n_risk(data = .x))

        # Write nrisk files for the digitise function
        combined_nrisk %>%
                names(.) %>%
                map(~ write.table(combined_nrisk[[.]], paste0("~/GalleriCEmodel/data-raw/NICE_modifier/surv_nrisk_cancerscombined", .x %>% substr(start = 1, stop = 5),".txt"), sep = "\t",
                                  row.names = FALSE))

        # Write surv times files for the digitise function
        combined_surv_times %>%
                names(.) %>%
                map(~ write.table(combined_surv_times[[.]], paste0("~/GalleriCEmodel/data-raw/NICE_modifier/surv_times_cancerscombined", .x %>% substr(start = 1, stop = 5),".txt"), sep = "\t",
                                  row.names = FALSE))

        l_files_nrisk <- intersect(list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", pattern = "surv_nrisk_cancerscombined"),
                                   list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", paste0(v_cancer, collapse="|")))

        l_file_surv_time <- intersect(list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", pattern = "surv_times_cancerscombined"),
                                      list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", paste0(v_cancer, collapse="|")))


        internal_f_digitise <- function(surv_string, nrisk_string, name_string){

                survHE::digitise(surv_inp = paste0("~/GalleriCEmodel/data-raw/NICE_modifier/", surv_string),
                                 nrisk_inp = paste0("~/GalleriCEmodel/data-raw/NICE_modifier/", nrisk_string),
                                 km_output = paste0("~/GalleriCEmodel/data-raw/NICE_modifier/", "surv_KMdata_", name_string ,".txt"),
                                 ipd_output = paste0("~/GalleriCEmodel/data-raw/NICE_modifier/", "surv_IPDdata_", name_string ,".txt"))
        }

        pmap(list(l_file_surv_time, l_files_nrisk, v_cancer),  internal_f_digitise)

        surv_IPDdata <- map_df(intersect(list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", pattern = "IPDdata_"),
                                         list.files(path = "~/GalleriCEmodel/data-raw/NICE_modifier/", paste0(v_cancer, collapse="|"))),
                               ~read.delim(paste0("~/GalleriCEmodel/data-raw/NICE_modifier/", .x)))

        mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = surv_IPDdata, add_knots = knotts)

        if(include_additive_hzds){

                model <- survextrap::survextrap(Surv(time, event) ~ 1, data=surv_IPDdata,
                                                mspline=mspline, backhaz = get_bckgrnd_hz,
                                                fit_method = survextrap_fit_method)
                print("Model with backhaz")


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

