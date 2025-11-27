#' Modal life expectancy using M-spline and calc survival
#'
#' Modal life expectancy using M-spline and calc survival
#'
#' @param survival Data frame input
#' @param include_additive_hzds True or False to include background hazard and use additive hazards framework
#' @param include_cure_fraction True or False to include cure fraction and use mixture-cure model
#' @param plotsurvhz True or False to output plots
#' @param survextrap_add_knotts_interval Knots arguement to feed through to survextrap function
#' @param survextrap_fit_method Fit method arguement to feed through to survextrap function
#' @param survextrap_niter number of iterations for MCMC to feed through to survextrap function
#'
#' @return Proabability of survival to age 60
#' @examples
#' f_prob_death_60
#' @export
f_prob_death_60 <- function(survival,
                            include_additive_hzds,
                            include_cure_fraction,
                            plotsurvhz,
                            survextrap_add_knotts_interval,
                            survextrap_fit_method,
                            survextrap_niter)
{

        get_age <- as.numeric(substr(survival$Age[1], start = 1, stop = 2)) + 3
        get_sex <- as.character(survival$Sex[1])


        if(include_additive_hzds){

                get_bckgrnd_hz <- df_gen_pop_hz %>%
                        filter(age >= get_age) %>%
                        filter(sex == get_sex) %>%
                        mutate(time = row_number() - 1) %>%
                        select(time, hazard)
        }

        if(include_cure_fraction){

                get_cure_shape1  <- as.numeric(survival$shape1[1])
                get_cure_shape2  <- as.numeric(survival$shape2[1])
        }

        if(!include_additive_hzds & include_cure_fraction) {

                stop(print("Must include additive hazards aswell as cure fraction"))
        }

        if(is.na(survextrap_add_knotts_interval)){

                knotts = NULL
        } else {
                knotts = seq.int(from = 10, to = 100 - get_age, by = survextrap_add_knotts_interval)
        }

        surv_times <- bind_rows(data.frame(stop = 0, CSS_HR_adjusted = 1), survival %>%
                                        select(stop, CSS_HR_adjusted)) %>%
                rename(tk = stop,
                       sk = CSS_HR_adjusted) %>%
                mutate(k = row_number()) %>%
                select(k, tk, sk)

        surv_nrisk <- survival %>%
                filter(start %in% c(0,1,2,3,4,4.75)) %>%
                rename(trisk = start,
                       nrisk = n_patients) %>%
                mutate(i = row_number()) %>%
                select(i, trisk, nrisk ) %>%
                mutate(lower = c(1,9,13,17,21,25),
                       upper = c(8,12,16,20,24,25)) %>%
                select(i, trisk, lower, upper, nrisk)

        write.table(surv_times, file = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_times.txt", sep = "\t",
                    row.names = FALSE)

        write.table(surv_nrisk, file = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_nrisk.txt", sep = "\t",
                    row.names = FALSE)

        survHE::digitise(surv_inp = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_times.txt" ,
                         nrisk_inp = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_nrisk.txt",
                         km_output = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_KMdata.txt",
                         ipd_output = "~/GalleriCEmodel/data-raw/NICE_modifier/surv_IPDdata.txt")

        surv_IPDdata <- read.delim("~/GalleriCEmodel/data-raw/NICE_modifier/surv_IPDdata.txt")

        mspline <- survextrap::mspline_spec(Surv(time, event) ~ 1, data = surv_IPDdata, add_knots = knotts)

        if(include_additive_hzds){

                if(include_cure_fraction){

                        model <- survextrap::survextrap(Surv(time, event) ~ 1,
                                                        data=surv_IPDdata, mspline=mspline, backhaz = get_bckgrnd_hz,
                                                        cure = TRUE, prior_cure =  p_beta(get_cure_shape1, get_cure_shape2),
                                                        fit_method = survextrap_fit_method)
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

                surv_60 <- survival( x = model, t = 60 - get_age, niter = survextrap_niter)
        } else {

                surv_60 <- survival( x = model, t = 60 - get_age, niter = survextrap_niter)
        }


        if(plotsurvhz){

                plot_surv <- survextrap::plot_survival(model, tmax = 100 - get_age, niter = survextrap_niter)
                plot_hz <- survextrap::plot_hazard(model, show_knots = TRUE, tmax = 100 - get_age, niter = survextrap_niter)

                return(list(surv_60, plot_surv, plot_hz, model))

        } else {

                return(surv_60)
        }


}

