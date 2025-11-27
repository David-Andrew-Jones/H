#______________________________________________________________________________#
#____ 03c_analysis_M_spline_sensitivities
options(mc.cores = 100)
#' Assess how changes to the model specification effects YLL estimates - apply to lung
#______________________________________________________________________________#
#'____ Results for each site by stage sex and age

#'____ Sens 5 year add knots
#'
l_split_TTE_data <- split(TTE_data_curefrac,
                          list(TTE_data_curefrac$Age_lower, TTE_data_curefrac$AJCC_stage,
                               TTE_data_curefrac$Sex, TTE_data_curefrac$SEER_Draw ))



temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Lung'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_lung_sens5y <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                          .f =  safely( ~f_life_expectancy(survival = .x,
                                                           include_additive_hzds = TRUE,
                                                           include_cure_fraction = TRUE,
                                                           plotsurvhz = FALSE,
                                                           survextrap_add_knotts_interval = 5,
                                                           survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_lung_sens5y, pluck, 1)), paste0(result_path_sens_knots5y, "LE_lung_sens5y"))
write_csv(f_LE_errors(l_res_lung_sens5y), paste0(result_path_sens_knots5y, "errors_lung_sens5y"))

rm(l_res_lung_sens5y)
gc()
Sys.sleep(60)

#'____ Sens 10 year add knots


l_res_lung_sens10y <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                           .f =  safely( ~f_life_expectancy(survival = .x,
                                                            include_additive_hzds = TRUE,
                                                            include_cure_fraction = TRUE,
                                                            plotsurvhz = FALSE,
                                                            survextrap_add_knotts_interval = 10,
                                                            survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_lung_sens10y, pluck, 1)), paste0(result_path_sens_knots10y, "LE_lung_sens10y"))
write_csv(f_LE_errors(l_res_lung_sens10y), paste0(result_path_sens_knots10y, "errors_lung_sens10y"))

rm(l_res_lung_sens10y)
gc()
Sys.sleep(60)

#'____ Sens no cure fraction

l_res_lung_sensnoCF <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                            .f =  safely( ~f_life_expectancy(survival = .x,
                                                             include_additive_hzds = TRUE,
                                                             include_cure_fraction = FALSE,
                                                             plotsurvhz = FALSE,
                                                             survextrap_add_knotts_interval = NA,
                                                             survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_lung_sensnoCF, pluck, 1)), paste0(result_path_sens_noCF, "LE_lung_sensnoCF"))
write_csv(f_LE_errors(l_res_lung_sensnoCF), paste0(result_path_sens_noCF, "errors_lung_sensnoCF"))

rm(l_res_lung_sensnoCF)
gc()
Sys.sleep(60)


