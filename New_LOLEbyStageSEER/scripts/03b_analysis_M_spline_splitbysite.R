#______________________________________________________________________________#
#____ 03a_analysis_M_spline_splitbysite

#' Alternative specification in which results are output in batch. 
#' This was written due to frequency of crashing when all run in function
#' 
options(mc.cores = 4)

#______________________________________________________________________________#
#______________________________________________________________________________#
#'____ All cancer

#' 40 Year


TTE_data_40 <- TTE_data %>% filter(Age_lower == 40) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_40 <-  map(.x = TTE_data_40,
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                      include_additive_hzds = TRUE,
                                                      include_cure_fraction = FALSE, include_Loo = FALSE,
                                                      plotsurvhz = FALSE,
                                                      survextrap_add_knotts_interval = NA,
                                                      survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_40, pluck, 1)), paste0(results_path, "LE_all_40"))
write_csv(f_LE_errors(l_res_all_40), paste0(results_path, "errors_all_40"))

rm(TTE_data_40, l_res_all_40)
gc()
Sys.sleep(60)

# 45 Year


TTE_data_45 <- TTE_data %>% filter(Age_lower == 45) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_45 <-  map(.x = TTE_data_45,
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                      include_additive_hzds = TRUE,
                                                      include_cure_fraction = FALSE, include_Loo = FALSE,
                                                      plotsurvhz = FALSE,
                                                      survextrap_add_knotts_interval = NA,
                                                      survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_45, pluck, 1)), paste0(results_path, "LE_all_45"))
write_csv(f_LE_errors(purrr::map(l_res_all_45, pluck, 2)), paste0(results_path, "errors_all_45"))

rm(TTE_data_45, l_res_all_45)
gc()
Sys.sleep(60)

#' 50 Year
 

TTE_data_50 <- TTE_data %>% filter(Age_lower == 50) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_50 <-  map(.x = TTE_data_50,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_50, pluck, 1)), paste0(results_path, "LE_all_50"))
write_csv(f_LE_errors(l_res_all_50), paste0(results_path, "errors_all_50"))

rm(TTE_data_50, l_res_all_50)
gc()
Sys.sleep(60)

# 55 Year
 

TTE_data_55 <- TTE_data %>% filter(Age_lower == 55) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_55 <-  map(.x = TTE_data_55,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_55, pluck, 1)), paste0(results_path, "LE_all_55"))
write_csv(f_LE_errors(purrr::map(l_res_all_55, pluck, 2)), paste0(results_path, "errors_all_55"))

rm(TTE_data_55, l_res_all_55)
gc()
Sys.sleep(60)

# 60 years
 

TTE_data_60 <- TTE_data %>% filter(Age_lower == 60) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_60 <-  map(.x = TTE_data_60,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_60, pluck, 1)), paste0(results_path, "LE_all_60"))
write_csv(f_LE_errors(l_res_all_60), paste0(results_path, "errors_all_60"))

rm(TTE_data_60, l_res_all_60)
gc()
Sys.sleep(60)

# 65 Year

 

TTE_data_65 <- TTE_data %>% filter(Age_lower == 65) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_65 <-  map(.x = TTE_data_65,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_65, pluck, 1)), paste0(results_path, "LE_all_65"))
write_csv(f_LE_errors(l_res_all_65), paste0(results_path, "errors_all_65"))

rm(TTE_data_65, l_res_all_65)
gc()
Sys.sleep(60)

# 70 years
 

TTE_data_70 <- TTE_data %>% filter(Age_lower == 70) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_70 <-  map(.x = TTE_data_70,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_70, pluck, 1)), paste0(results_path, "LE_all_70"))
write_csv(f_LE_errors(l_res_all_70), paste0(results_path, "errors_all_70"))

rm(TTE_data_70, l_res_all_70)
gc()
Sys.sleep(60)

# 75 Year
 

TTE_data_75 <- TTE_data %>% filter(Age_lower == 75) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_75 <-  map(.x = TTE_data_75,
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_75, pluck, 1)), paste0(results_path, "LE_all_75"))
write_csv(f_LE_errors(l_res_all_75), paste0(results_path, "errors_all_75"))

rm(TTE_data_75, l_res_all_75)
gc()
Sys.sleep(60)

# 80 years

TTE_data_80 <- TTE_data %>% filter(Age_lower == 80) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_80 <-  map(.x = TTE_data_80,
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                      include_additive_hzds = TRUE,
                                                      include_cure_fraction = FALSE, include_Loo = FALSE,
                                                      plotsurvhz = FALSE,
                                                      survextrap_add_knotts_interval = NA,
                                                      survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_80, pluck, 1)), paste0(results_path, "LE_all_80"))
write_csv(f_LE_errors(l_res_all_80), paste0(results_path, "errors_all_80"))

rm(TTE_data_80, l_res_all_80)
gc()
Sys.sleep(60)

# 85 Year

TTE_data_85 <- TTE_data %>% filter(Age_lower == 85) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.all.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_all_85 <-  map(.x = TTE_data_85,
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                      include_additive_hzds = TRUE,
                                                      include_cure_fraction = FALSE, include_Loo = FALSE,
                                                      plotsurvhz = FALSE,
                                                      survextrap_add_knotts_interval = NA,
                                                      survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_all_85, pluck, 1)), paste0(results_path, "LE_all_85"))
write_csv(f_LE_errors(l_res_all_85), paste0(results_path, "errors_all_85"))

rm(TTE_data_85, l_res_all_85)
gc()
Sys.sleep(60)

#'____ All cancer - excluding prostate - just Male

TTE_data_no_prostate <- TTE_data %>% filter(SEER_Draw != "Prostate") %>% filter(Sex == "Male") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) 

# 40 Year
TTE_data_40 <- TTE_data_no_prostate %>% filter(Age_lower == 40) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_40 <-  map(.x = TTE_data_40,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc",
                                                                 survextrap_niter = 1000
                                )))



write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_40, pluck, 1)), paste0(results_path, "LE_allexcprostate_40"))
write_csv(f_LE_errors(l_res_allexcprostate_40), paste0(results_path, "errors_allexcprostate_40"))

rm(TTE_data_40, l_res_allexcprostate_40)
gc()
Sys.sleep(60)

# 55 Year

TTE_data_45 <- TTE_data_no_prostate %>% filter(Age_lower == 45) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_45 <-  map(.x = TTE_data_45,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))



write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_45, pluck, 1)), paste0(results_path, "LE_allexcprostate_45"))
write_csv(f_LE_errors(l_res_allexcprostate_45), paste0(results_path, "errors_allexcprostate_45"))

rm(TTE_data_45, l_res_allexcprostate_45)
gc()
Sys.sleep(60)

# 50 Year
TTE_data_50 <- TTE_data_no_prostate %>% filter(Age_lower == 50) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_50 <-  map(.x = TTE_data_50,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc",
                                                                 survextrap_niter = 1000
                                )))



write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_50, pluck, 1)), paste0(results_path, "LE_allexcprostate_50"))
write_csv(f_LE_errors(l_res_allexcprostate_50), paste0(results_path, "errors_allexcprostate_50"))

rm(TTE_data_50, l_res_allexcprostate_50)
gc()
Sys.sleep(60)

# 55 Year

TTE_data_55 <- TTE_data_no_prostate %>% filter(Age_lower == 55) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_55 <-  map(.x = TTE_data_55,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))



write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_55, pluck, 1)), paste0(results_path, "LE_allexcprostate_55"))
write_csv(f_LE_errors(l_res_allexcprostate_55), paste0(results_path, "errors_allexcprostate_55"))

rm(TTE_data_55, l_res_allexcprostate_55)
gc()
Sys.sleep(60)

# 60 years


TTE_data_60 <- TTE_data_no_prostate %>% filter(Age_lower == 60) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_60 <-  map(.x = TTE_data_60,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_60, pluck, 1)), paste0(results_path, "LE_allexcprostate_60"))
write_csv(f_LE_errors(l_res_allexcprostate_60), paste0(results_path, "errors_allexcprostate_60"))

rm(TTE_data_60, l_res_allexcprostate_60)
gc()
Sys.sleep(60)

# 65 Year


TTE_data_65 <- TTE_data_no_prostate %>% filter(Age_lower == 65) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_65 <-  map(.x = TTE_data_65,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_65, pluck, 1)), paste0(results_path, "LE_allexcprostate_65"))
write_csv(f_LE_errors(l_res_allexcprostate_65), paste0(results_path, "errors_allexcprostate_65"))

rm(TTE_data_65, l_res_allexcprostate_65)
gc()
Sys.sleep(60)

# 70 years


TTE_data_70 <- TTE_data_no_prostate %>% filter(Age_lower == 70) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_70 <-  map(.x = TTE_data_70,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_70, pluck, 1)), paste0(results_path, "LE_allexcprostate_70"))
write_csv(f_LE_errors(l_res_allexcprostate_70), paste0(results_path, "errors_allexcprostate_70"))

rm(TTE_data_70, l_res_allexcprostate_70)
gc()
Sys.sleep(60)

# 75 Year


TTE_data_75 <- TTE_data_no_prostate %>% filter(Age_lower == 75) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_75 <-  map(.x = TTE_data_75,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_75, pluck, 1)), paste0(results_path, "LE_allexcprostate_75"))
write_csv(f_LE_errors(l_res_allexcprostate_75), paste0(results_path, "errors_allexcprostate_75"))

rm(TTE_data_75, l_res_allexcprostate_75)
gc()
Sys.sleep(60)

# 80 years


TTE_data_80 <- TTE_data_no_prostate %>% filter(Age_lower == 80) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_80 <-  map(.x = TTE_data_80,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_80, pluck, 1)), paste0(results_path, "LE_allexcprostate_80"))
write_csv(f_LE_errors(l_res_allexcprostate_80), paste0(results_path, "errors_allexcprostate_80"))

rm(TTE_data_80, l_res_allexcprostate_80)
gc()
Sys.sleep(60)

# 85 Year


TTE_data_85 <- TTE_data_no_prostate %>% filter(Age_lower == 85) %>%
        group_split(Sex, AJCC_stage, Age_lower) %>%
        set_names(., nm = map(.x = ., ~glue::glue("{first(.x$Sex)}.allexcprostate.{first(.x$AJCC_stage)}.{first(.x$Age_lower)}") ) ) ## Different way here as cancer name dropped out

l_res_allexcprostate_85 <-  map(.x = TTE_data_85,
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_allexcprostate_85, pluck, 1)), paste0(results_path, "LE_allexcprostate_85"))
write_csv(f_LE_errors(l_res_allexcprostate_85), paste0(results_path, "errors_allexcprostate_85"))

rm(TTE_data_85, l_res_allexcprostate_85)
gc()
Sys.sleep(60)



#______________________________________________________________________________#
#'____ 1. Breast
#'
l_split_TTE_data <- split(TTE_data,
                                 list(TTE_data$Age_lower, TTE_data$AJCC_stage,
                                      TTE_data$Sex, TTE_data$SEER_Draw ))

 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Breast'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Breast <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                  .f =  safely( ~f_life_expectancy(survival = .x,
                                                     include_additive_hzds = TRUE,
                                                     include_cure_fraction = FALSE, include_Loo = FALSE,
                                                     plotsurvhz = FALSE,
                                                     survextrap_add_knotts_interval = NA,
                                                     survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


write_csv(f_LE_tables(purrr::map(l_res_Breast, pluck, 1)), paste0(results_path, "LE_Breast"))
write_csv(f_LE_errors(l_res_Breast), paste0(results_path, "errors_Breast"))

rm(temp, l_res_Breast)
gc()
Sys.sleep(60)

#'____ 2. Cervix Uteri
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Cervix Uteri'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Cervix.Uteri <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = FALSE, include_Loo = FALSE,
                                                        plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Cervix.Uteri, pluck, 1)), paste0(results_path, "LE_Cervix.Uteri"))
write_csv(f_LE_errors(l_res_Cervix.Uteri), paste0(results_path, "errors_Cervix.Uteri"))

rm(temp, l_res_Cervix.Uteri)
gc()
Sys.sleep(60)

#'____ 3. Colon and Rectum
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Colon and Rectum'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Colon.and.Rectum <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))


        
write_csv(f_LE_tables(purrr::map(l_res_Colon.and.Rectum, pluck, 1)), paste0(results_path, "LE_Colon.and.Rectum"))
write_csv(f_LE_errors(l_res_Colon.and.Rectum), paste0(results_path, "errors_Colon.and.Rectum"))

rm(temp, l_res_Colon.and.Rectum)
gc()
Sys.sleep(60)

#'____ 4. Corpus and Uterus
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Corpus and Uterus'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Corpus.and.Uterus <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Corpus.and.Uterus, pluck, 1)), paste0(results_path, "LE_Corpus.and.Uterus"))
write_csv(f_LE_errors(l_res_Corpus.and.Uterus), paste0(results_path, "errors_Corpus.and.Uterus"))

rm(temp, l_res_Corpus.and.Uterus)
gc()
Sys.sleep(60)

#'____ 5. Esophagus
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Esophagus'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Esophagus <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                         .f =  safely( ~f_life_expectancy(survival = .x,
                                                            include_additive_hzds = TRUE,
                                                            include_cure_fraction = FALSE, include_Loo = FALSE,
                                                            plotsurvhz = FALSE,
                                                            survextrap_add_knotts_interval = NA,
                                                            survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Esophagus, pluck, 1)), paste0(results_path, "LE_Esophagus"))
write_csv(f_LE_errors(l_res_Esophagus), paste0(results_path, "errors_Esophagus"))

rm(temp, l_res_Esophagus)
gc()
Sys.sleep(60)

#'____ 6. Gallbladder
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Gallbladder'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Gallbladder <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                       .f =  safely( ~f_life_expectancy(survival = .x,
                                                          include_additive_hzds = TRUE,
                                                          include_cure_fraction = FALSE, include_Loo = FALSE,
                                                          plotsurvhz = FALSE,
                                                          survextrap_add_knotts_interval = NA,
                                                          survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Gallbladder, pluck, 1)), paste0(results_path, "LE_Gallbladder"))
write_csv(f_LE_errors(l_res_Gallbladder), paste0(results_path, "errors_Gallbladder"))

rm(temp, l_res_Gallbladder)
gc()
Sys.sleep(60)

#'____ 7. Hodgkin Lymphoma
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Hodgkin Lymphoma'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Hodgkin.Lymphoma <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                         .f =  safely( ~f_life_expectancy(survival = .x,
                                                            include_additive_hzds = TRUE,
                                                            include_cure_fraction = FALSE, include_Loo = FALSE,
                                                            plotsurvhz = FALSE,
                                                            survextrap_add_knotts_interval = NA,
                                                            survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Hodgkin.Lymphoma, pluck, 1)), paste0(results_path, "LE_Hodgkin.Lymphoma"))
write_csv(f_LE_errors(l_res_Hodgkin.Lymphoma), paste0(results_path, "errors_Hodgkin.Lymphoma"))

rm(temp, l_res_Hodgkin.Lymphoma)
gc()
Sys.sleep(60)

#'____ 8. Kidney and Renal Pelvis
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Kidney and Renal Pelvis'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Kidney.and.Renal.Pelvis<-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                .f =  safely( ~f_life_expectancy(survival = .x,
                                                   include_additive_hzds = TRUE,
                                                   include_cure_fraction = FALSE, include_Loo = FALSE,
                                                   plotsurvhz = FALSE,
                                                   survextrap_add_knotts_interval = NA,
                                                   survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Kidney.and.Renal.Pelvis, pluck, 1)), paste0(results_path, "LE_Kidney.and.Renal.Pelvis"))
write_csv(f_LE_errors(l_res_Kidney.and.Renal.Pelvis), paste0(results_path, "errors_Kidney.and.Renal.Pelvis"))

rm(temp, l_res_Kidney.and.Renal.Pelvis)
gc()
Sys.sleep(60)

#'____ 9. Larynx
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Larynx'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Larynx <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Larynx, pluck, 1)), paste0(results_path, "LE_Larynx"))
write_csv(f_LE_errors(l_res_Larynx), paste0(results_path, "errors_Larynx"))

rm(temp, l_res_Larynx)
gc()
Sys.sleep(60)

#'____ 10. Liver and Intrahepatic Bile Duct
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Liver and Intrahepatic Bile Duct'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Liver.and.Intrahepatic.Bile.Duct <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                           .f =  safely( ~f_life_expectancy(survival = .x,
                                                              include_additive_hzds = TRUE,
                                                              include_cure_fraction = FALSE, include_Loo = FALSE,
                                                              plotsurvhz = FALSE,
                                                              survextrap_add_knotts_interval = NA,
                                                              survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Liver.and.Intrahepatic.Bile.Duct, pluck, 1)), paste0(results_path, "LE_Liver.and.Intrahepatic.Bile.Duct"))
write_csv(f_LE_errors(l_res_Liver.and.Intrahepatic.Bile.Duct), paste0(results_path, "errors_Liver.and.Intrahepatic.Bile.Duct"))

rm(temp, l_res_Liver.and.Intrahepatic.Bile.Duct)
gc()
Sys.sleep(60)

#'____ 11a. Lung and Bronchus
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Lung and Bronchus'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Lung.and.Bronchus <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                  .f =  safely( ~f_life_expectancy(survival = .x,
                                                     include_additive_hzds = TRUE,
                                                     include_cure_fraction = FALSE, include_Loo = TRUE,
                                                     plotsurvhz = FALSE,
                                                     survextrap_add_knotts_interval = NA,
                                                     survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Lung.and.Bronchus, pluck, 1)), paste0(results_path, "LE_Lung.and.Bronchus"))
write_csv(f_LE_errors(l_res_Lung.and.Bronchus), paste0(results_path, "errors_Lung.and.Bronchus"))

rm(temp, l_res_Lung.and.Bronchus)
gc()
Sys.sleep(60)

#'____ 11b. Lung and Bronchus (cure)


temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Lung and Bronchus'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Lung.and.Bronchus <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                                .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = TRUE, include_Loo = TRUE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Lung.and.Bronchus, pluck, 1)), paste0(results_path_sens_CF, "LE_Lung.and.Bronchus"))
write_csv(f_LE_errors(l_res_Lung.and.Bronchus), paste0(results_path_sens_CF, "errors_Lung.and.Bronchus"))

rm(temp, l_res_Lung.and.Bronchus)
gc()
Sys.sleep(60)

#'____ 12. Melanoma of the Skin
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Melanoma of the Skin'))) %>%
mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Melanoma.of.the.Skin <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                         include_additive_hzds = TRUE,
                                                         include_cure_fraction = FALSE, include_Loo = FALSE,
                                                         plotsurvhz = FALSE,
                                                         survextrap_add_knotts_interval = NA,
                                                         survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Melanoma.of.the.Skin, pluck, 1)), paste0(results_path, "LE_Melanoma.of.the.Skin"))
write_csv(f_LE_errors(l_res_Melanoma.of.the.Skin), paste0(results_path, "errors_Melanoma.of.the.Skin"))

rm(temp, l_res_Melanoma.of.the.Skin)
gc()
Sys.sleep(60)

#'____ 13. Mesothelioma
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Mesothelioma'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Mesothelioma <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                         include_additive_hzds = TRUE,
                                                         include_cure_fraction = FALSE, include_Loo = FALSE,
                                                         plotsurvhz = FALSE,
                                                         survextrap_add_knotts_interval = NA,
                                                         survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Mesothelioma, pluck, 1)), paste0(results_path, "LE_Mesothelioma"))
write_csv(f_LE_errors(l_res_Mesothelioma), paste0(results_path, "errors_Mesothelioma"))

rm(temp, l_res_Mesothelioma)
gc()
Sys.sleep(60)

#'____ 14. Non-Hodgkin Lymphoma
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Non-Hodgkin Lymphoma'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Non.Hodgkin.Lymphoma <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                   .f =  safely( ~f_life_expectancy(survival = .x,
                                                      include_additive_hzds = TRUE,
                                                      include_cure_fraction = FALSE, include_Loo = FALSE,
                                                      plotsurvhz = FALSE,
                                                      survextrap_add_knotts_interval = NA,
                                                      survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Non.Hodgkin.Lymphoma, pluck, 1)), paste0(results_path, "LE_Non.Hodgkin.Lymphoma"))
write_csv(f_LE_errors(l_res_Non.Hodgkin.Lymphoma), paste0(results_path, "errors_Non.Hodgkin.Lymphoma"))

rm(temp, l_res_Non.Hodgkin.Lymphoma)
gc()
Sys.sleep(60)

#'____ 15. Oral Cavity and Pharynx
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Oral Cavity and Pharynx'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Oral.Cavity.and.Pharynx <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                         include_additive_hzds = TRUE,
                                                         include_cure_fraction = FALSE, include_Loo = FALSE,
                                                         plotsurvhz = FALSE,
                                                         survextrap_add_knotts_interval = NA,
                                                         survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Oral.Cavity.and.Pharynx, pluck, 1)), paste0(results_path, "LE_Oral.Cavity.and.Pharynx"))
write_csv(f_LE_errors(l_res_Oral.Cavity.and.Pharynx), paste0(results_path, "errors_Oral.Cavity.and.Pharynx"))

rm(temp, l_res_Oral.Cavity.and.Pharynx)
gc()
Sys.sleep(60)

#'____ 16. Other Biliary
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Other Biliary'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Other.Biliary <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                         include_additive_hzds = TRUE,
                                                         include_cure_fraction = FALSE, include_Loo = FALSE,
                                                         plotsurvhz = FALSE,
                                                         survextrap_add_knotts_interval = NA,
                                                         survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Other.Biliary, pluck, 1)), paste0(results_path, "LE_Other.Biliary"))
write_csv(f_LE_errors(l_res_Other.Biliary), paste0(results_path, "errors_Other.Biliary"))

rm(temp, l_res_Other.Biliary)
gc()
Sys.sleep(60)

#'____ 17. Other Non-Epithelial Skin

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Other Non-Epithelial Skin'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Other.Non.Epithelial.Skin <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = FALSE, include_Loo = FALSE,
                                                        plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Other.Non.Epithelial.Skin, pluck, 1)), paste0(results_path, "LE_Other.Non.Epithelial.Skin"))
write_csv(f_LE_errors(l_res_Other.Non.Epithelial.Skin), paste0(results_path, "errors_Other.Non.Epithelial.Skin"))

rm(temp, l_res_Other.Non.Epithelial.Skin)
gc()
Sys.sleep(60)

#'____ 18. Pancreas
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Pancreas'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Pancreas <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = FALSE, include_Loo = FALSE,
                                                        plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Pancreas, pluck, 1)), paste0(results_path, "LE_Pancreas"))
write_csv(f_LE_errors(l_res_Pancreas), paste0(results_path, "errors_Pancreas"))

rm(temp, l_res_Pancreas)
gc()
Sys.sleep(60)

#'____ 19. Prostate
 

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Prostate'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Prostate <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                     .f =  safely( ~f_life_expectancy(survival = .x,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = FALSE, include_Loo = FALSE,
                                                        plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Prostate, pluck, 1)), paste0(results_path, "LE_Prostate"))
write_csv(f_LE_errors(l_res_Prostate), paste0(results_path, "errors_Prostate"))

rm(temp, l_res_Prostate)
gc()
Sys.sleep(60)

#'____ 20. Small Intestine
 
temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Small Intestine'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Small.Intestine <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                              .f =  safely( ~f_life_expectancy(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Small.Intestine, pluck, 1)), paste0(results_path, "LE_Small.Intestine"))
write_csv(f_LE_errors(l_res_Small.Intestine), paste0(results_path, "errors_Small.Intestine"))

rm(temp, l_res_Small.Intestine)
gc()
Sys.sleep(60)

#'____ 21. Soft Tissue including Heart

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Soft Tissue including Heart'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Soft.Tissue.including.Heart <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                    .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Soft.Tissue.including.Heart, pluck, 1)), paste0(results_path, "LE_Soft.Tissue.including.Heart"))
write_csv(f_LE_errors(l_res_Soft.Tissue.including.Heart), paste0(results_path, "errors_Soft.Tissue.including.Heart"))

rm(temp, l_res_Soft.Tissue.including.Heart)
gc()
Sys.sleep(60)

#'____ 22. Stomach

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Stomach'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Stomach <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                                          .f =  safely( ~f_life_expectancy(survival = .x,
                                                                           include_additive_hzds = TRUE,
                                                                           include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                           plotsurvhz = FALSE,
                                                                           survextrap_add_knotts_interval = NA,
                                                                           survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Stomach, pluck, 1)), paste0(results_path, "LE_Stomach"))
write_csv(f_LE_errors(l_res_Stomach), paste0(results_path, "errors_Stomach"))

rm(temp, l_res_Stomach)
gc()
Sys.sleep(60)


#'____ 23. Thyroid

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Thyroid'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Thyroid <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Thyroid, pluck, 1)), paste0(results_path, "LE_Thyroid"))
write_csv(f_LE_errors(l_res_Thyroid), paste0(results_path, "errors_Thyroid"))

rm(temp, l_res_Thyroid)
gc()
Sys.sleep(60)

#'____ 24. Ureter and Other Urinary Organs

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Ureter and Other Urinary Organs'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Ureter.and.Other.Urinary.Organs <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                      .f =  safely( ~f_life_expectancy(survival = .x,
                                                       include_additive_hzds = TRUE,
                                                       include_cure_fraction = FALSE, include_Loo = FALSE,
                                                       plotsurvhz = FALSE,
                                                       survextrap_add_knotts_interval = NA,
                                                       survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Ureter.and.Other.Urinary.Organs, pluck, 1)), paste0(results_path, "LE_Ureter.and.Other.Urinary.Organs"))
write_csv(f_LE_errors(l_res_Ureter.and.Other.Urinary.Organs), paste0(results_path, "errors_Ureter.and.Other.Urinary.Organs"))

rm(temp, l_res_Ureter.and.Other.Urinary.Organs)
gc()
Sys.sleep(60)


#'____ 25. Urinary Bladder

temp <- bind_rows(keep(l_split_TTE_data, str_detect(names(l_split_TTE_data), 'Urinary Bladder'))) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Urinary.Bladder <-  map(.x = split(temp, list(temp$Sex, temp$SEER_Draw, temp$AJCC_stage, temp$Age_lower )),
                                              .f =  safely( ~f_life_expectancy(survival = .x,
                                                                               include_additive_hzds = TRUE,
                                                                               include_cure_fraction = FALSE, include_Loo = FALSE,
                                                                               plotsurvhz = FALSE,
                                                                               survextrap_add_knotts_interval = NA,
                                                                               survextrap_fit_method = "mcmc", survextrap_niter = 1000)))

write_csv(f_LE_tables(purrr::map(l_res_Urinary.Bladder, pluck, 1)), paste0(results_path, "LE_Urinary.Bladder"))
write_csv(f_LE_errors(l_res_Urinary.Bladder), paste0(results_path, "errors_Urinary.Bladder"))

rm(temp, l_res_Urinary.Bladder)
gc()
Sys.sleep(60)







