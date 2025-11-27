#______________________________________________________________________________#
#____ 03a_analysis_M_spline

#' Uses a Bayesian flexible parametric survival model, based on an M-spline
#' to extrapolate the SEER individual patient data 

#______________________________________________________________________________#
#' Run analysis
#' Have to run batch due to excessive computation time. Batch by cancer type 

#______________________________________________________________________________#
#'____ Results for each site by stage sex and age
l_split_TTE_data_bysite <- split(TTE_data_curefrac,
                          list(TTE_data_curefrac$Age_lower, TTE_data_curefrac$AJCC_stage,
                               TTE_data_curefrac$Sex, TTE_data_curefrac$SEER_Draw ))

l_res_allbysite<- map(.x = l_split_TTE_data_bysite,
                     .f =  possibly( ~f_life_expectancy(survival = .x,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = TRUE,
                                                        plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = "mcmc"),
                                     otherwise = NA))

write_csv(f_LE_tables(l_res_allbysite), paste0(results_path, "res_allbysite" ))
#______________________________________________________________________________#
#'____ All cancer - excluding prostate

l_split_TTE_data_allexcprostate <- TTE_data_curefrac %>% 
        filter(SEER_Draw != "Prostate") %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate <- map(.x = l_split_TTE_data_allexcprostate,
                            .f =  possibly( ~f_life_expectancy(survival = .x,
                                                               include_additive_hzds = TRUE,
                                                               include_cure_fraction = TRUE,
                                                               plotsurvhz = FALSE,
                                                               survextrap_add_knotts_interval = NA,
                                                               survextrap_fit_method = "mcmc"), 
                                            otherwise = NA))

write_csv(f_LE_tables(l_res_allexcprostate), paste0(results_path, "res_allexcprostate" ))

#______________________________________________________________________________#
#'____ All cancer
#'
l_split_TTE_data_all <- TTE_data_curefrac %>% 
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all <- map(.x = l_split_TTE_data_all,
                    .f =  possibly( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = TRUE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc"),
                                    otherwise = NA))

write_csv(f_LE_tables(l_res_all), paste0(results_path, "res_all" ))
