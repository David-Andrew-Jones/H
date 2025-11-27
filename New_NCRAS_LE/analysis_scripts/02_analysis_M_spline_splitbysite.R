#' _____________________________________________________________________________
#' _____ 02_analysis_M_spline_splitbysite

#' In this script the Bayesian flexible parametric survival models are run. One is run
#' for each 'presentation', which is a combination of cancer type, stage, age-band, sex,
#' whether there is a hazard of mortality, and cfDNA status, e.g. lung cancer, stage I, 50-55, female, HR of
#' death of 2, cfDNA-. (In the no HR scenario cfDNA positive and negative will be the same)

#' Due to the length of time the full Markov Chain Monte Carlo procedure takes,
#' which causes RAM issues and crashing, the below code runs cancer by cancer. It's
#' repetitive in nature so that the analysis can be picked up if there is a crash
#' Here is the example of Anus

# temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Anus") %>%
#         mutate(across(where(is.factor), as.character)) %>%
#         mutate(across(where(is.character), as.factor))
#
#
# l_res_anus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
#                   .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
#                                                                include_additive_hzds = TRUE,
#                                                                include_cure_fraction = specify_cure,
#                                                                include_Loo = FALSE, plotsurvhz = FALSE,
#                                                                survextrap_add_knotts_interval = NA,
#                                                                survextrap_fit_method = specify_method,
#                                                                survextrap_niter = specify_n_interation)))
#
# write_csv(f_LE_summary_table(l_res_anus), paste0(specify_res_save_path, specify_meth_name, "_LE_Anus"))
# write_csv(f_LE_errors(l_res_anus), paste0(specify_res_save_path, specify_meth_name, "_errors_Anus"))


#' First, a temporary dataframe (temp) is created for the cancer type.
#' The next chunk performs the analysis. The function 'split' is used to
#' split the cancer type specific dataframes into stage, age-band, sex, hazard of mortality, and
#' cfDNA status sub-dataframes, which are mapped to the f_life_exp function. Probably best time to look at function.
#' There is a 'safely' wrapper around the f_life_exp() function which picks up any errors, and
#' allows map to keep running. E.g, if an error was created for the lung cancer, stage I, 50-55, female, HR of
#' death of 2, cfDNA- presentation, the error would be logged and 'map' would move onto
#' the next presentation without stopping

#' The f_life_exp function primarily (and is specified here to)
#' returns the Restricted Mean Survival Time (RMST) estimate from the Bayesian
#' flexible parametric survival model fit and associated credible intervals.
#' If specified, it can also produce the survival and hazard plots.
#' l_res_anus is therefore a list of dataframes for each of the Anus cancer presentations

#' Subsequently, the functions f_LE_summary_table and f_LE_errors flatten these
#' lists into dataframes and save them with the following column headers.
#' Sex | NCRAS_Draw | Stage | Age | haz_ratio | cfDNA_status | variable | t | median_CrI | median | CrI_temp | 2.5% | 97.5%,CrI
#' Some memory management is then performed

#' _____________________________________________________________________________
#' _____ 1. Bayesian flexible parametric survival model parameters

#' First, specify the estimation method for the Bayesian flexible parametric survival model
specify_method <- "opt" # "mcmc" for full run
specify_n_interation <- 5 # number of iterations

#' Global option mc.cores - increases cores used. This flows into the "mcmc" method of the
#' survextrap() function, which then in turns flows into the rstan::samplying() function.
#' Parallisation occurs here, with each "thread" performed separately on a core. By default
#' there are 4 threads, and the default is used
options(mc.cores = 4)
#' To run the full "mcmc" analysis, a high powered adhoc was previously necessary rather than a personal laptop


#' For primary analysis - Path locations and run name
specify_res_save_path <- paste0(getwd(), "/results/primary_raw_tables/")
specify_meth_name <- "opt_nocure_relsurv"
specify_cure <- FALSE

#' For sensitivity analysis - Path locations and run name
#' Run this after primary with the above hashed out and the below unhashed
#' For this code review it is assumed the primary is a no-cure model and the sensitivity is
#' a mixture cure specification.
#' For the sensitivity analysis we may want to focus on only a few cancer types. e.g. Anal - colorectal
# specify_res_save_path <- paste0(getwd(), "/results/sensitivity_raw_tables/")
# specify_meth_name <- "opt_cure_relsurv"
# specify_cure <- TRUE

# ALSO NOTE, AGGREGATE ONLY METHOD CURRENTLY ONLY PROCDUCES RESULTS FOR NO HR OF MORTALITY

#' _____________________________________________________________________________
#' _____ 2. Go site by site
#' _____ Anus

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Anus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_anus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                  .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                            include_additive_hzds = TRUE,
                                            include_cure_fraction = specify_cure,
                                            include_Loo = FALSE, plotsurvhz = FALSE,
                                            survextrap_add_knotts_interval = NA,
                                            survextrap_fit_method = specify_method,
                                            survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_anus), paste0(specify_res_save_path, specify_meth_name, "_LE_Anus.csv"))
write_csv(f_LE_errors(l_res_anus), paste0(specify_res_save_path, specify_meth_name, "_errors_Anus.csv"))

rm(l_res_anus, temp)
gc()
Sys.sleep(60)

#' _____ Bladder

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Bladder") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Bladder <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                               include_additive_hzds = TRUE,
                                               include_cure_fraction = specify_cure,
                                               include_Loo = FALSE, plotsurvhz = FALSE,
                                               survextrap_add_knotts_interval = NA,
                                               survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Bladder), paste0(specify_res_save_path, specify_meth_name, "_LE_Bladder.csv"))
write_csv(f_LE_errors(l_res_Bladder), paste0(specify_res_save_path, specify_meth_name, "_errors_Bladder.csv"))

rm(l_res_Bladder, temp)
gc()
Sys.sleep(60)

#' _____ Breast

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Breast") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))


l_res_Breast <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                              include_additive_hzds = TRUE,
                                              include_cure_fraction = specify_cure,
                                              include_Loo = FALSE, plotsurvhz = FALSE,
                                              survextrap_add_knotts_interval = NA,
                                              survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Breast), paste0(specify_res_save_path, specify_meth_name, "_LE_Breast.csv"))
write_csv(f_LE_errors(l_res_Breast), paste0(specify_res_save_path, specify_meth_name, "_errors_Breast.csv"))

rm(l_res_Breast, temp)
gc()
Sys.sleep(60)

#' _____ Cervix

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Cervix") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Cervix <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                              include_additive_hzds = TRUE,
                                              include_cure_fraction = specify_cure,
                                              include_Loo = FALSE, plotsurvhz = FALSE,
                                              survextrap_add_knotts_interval = NA,
                                              survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Cervix), paste0(specify_res_save_path, specify_meth_name, "_LE_Cervix.csv"))
write_csv(f_LE_errors(l_res_Cervix), paste0(specify_res_save_path, specify_meth_name, "_errors_Cervix.csv"))

rm(l_res_Cervix, temp)
gc()
Sys.sleep(60)

#' _____ Colon/Rectum

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Colon/Rectum") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_ColonRectum <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                         .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                   include_additive_hzds = TRUE,
                                                   include_cure_fraction = specify_cure,
                                                   include_Loo = FALSE, plotsurvhz = FALSE,
                                                   survextrap_add_knotts_interval = NA,
                                                   survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_ColonRectum), paste0(specify_res_save_path, specify_meth_name, "_LE_ColonRectum.csv"))
write_csv(f_LE_errors(l_res_ColonRectum), paste0(specify_res_save_path, specify_meth_name, "_errors_ColonRectum.csv"))

rm(l_res_ColonRectum, temp)
gc()
Sys.sleep(60)

#' _____ Esophagus

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Esophagus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Esophagus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                       .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                 include_additive_hzds = TRUE,
                                                 include_cure_fraction = specify_cure,
                                                 include_Loo = FALSE, plotsurvhz = FALSE,
                                                 survextrap_add_knotts_interval = NA,
                                                 survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Esophagus), paste0(specify_res_save_path, specify_meth_name, "_LE_Esophagus.csv"))
write_csv(f_LE_errors(l_res_Esophagus), paste0(specify_res_save_path, specify_meth_name, "_errors_Esophagus.csv"))

rm(l_res_Esophagus, temp)
gc()
Sys.sleep(60)
#'____ Gallbladder

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Gallbladder") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Gallbladder <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                         .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                   include_additive_hzds = TRUE,
                                                   include_cure_fraction = specify_cure,
                                                   include_Loo = FALSE, plotsurvhz = FALSE,
                                                   survextrap_add_knotts_interval = NA,
                                                   survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Gallbladder), paste0(specify_res_save_path, specify_meth_name, "_LE_Gallbladder.csv"))
write_csv(f_LE_errors(l_res_Gallbladder), paste0(specify_res_save_path, specify_meth_name, "_errors_Gallbladder.csv"))

rm(l_res_Gallbladder, temp)
gc()
Sys.sleep(60)

#' _____ Head and Neck

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Head and Neck") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_HNC<- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                          include_additive_hzds = TRUE,
                                          include_cure_fraction = specify_cure,
                                          include_Loo = FALSE, plotsurvhz = FALSE,
                                          survextrap_add_knotts_interval = NA,
                                          survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_HNC), paste0(specify_res_save_path, specify_meth_name, "_LE_HNC.csv"))
write_csv(f_LE_errors(l_res_HNC), paste0(specify_res_save_path, specify_meth_name, "_errors_HNC.csv"))

rm(l_res_HNC, temp)
gc()
Sys.sleep(60)

#' _____ Kidney

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Kidney") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Kidney <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                              include_additive_hzds = TRUE,
                                              include_cure_fraction = specify_cure,
                                              include_Loo = FALSE, plotsurvhz = FALSE,
                                              survextrap_add_knotts_interval = NA,
                                              survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Kidney), paste0(specify_res_save_path, specify_meth_name, "_LE_Kidney.csv"))
write_csv(f_LE_errors(l_res_Kidney), paste0(specify_res_save_path, specify_meth_name, "_errors_Kidney.csv"))

rm(l_res_Kidney, temp)
gc()
Sys.sleep(60)

#' _____ Liver/Bile-duct

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Liver/Bile-duct") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_LiverBileduct <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                           .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                     include_additive_hzds = TRUE,
                                                     include_cure_fraction = specify_cure,
                                                     include_Loo = FALSE, plotsurvhz = FALSE,
                                                     survextrap_add_knotts_interval = NA,
                                                     survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_LiverBileduct), paste0(specify_res_save_path, specify_meth_name, "_LE_LiverBileduct.csv"))
write_csv(f_LE_errors(l_res_LiverBileduct), paste0(specify_res_save_path, specify_meth_name, "_errors_LiverBileduct.csv"))

rm(l_res_LiverBileduct, temp)
gc()
Sys.sleep(60)

#' _____ Lung

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Lung") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_lung <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                  .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                            include_additive_hzds = TRUE,
                                            include_cure_fraction = specify_cure,
                                            include_Loo = FALSE, plotsurvhz = FALSE,
                                            survextrap_add_knotts_interval = NA,
                                            survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_lung), paste0(specify_res_save_path, specify_meth_name, "_LE_Lung.csv"))
write_csv(f_LE_errors(l_res_lung), paste0(specify_res_save_path, specify_meth_name, "_errors_Lung.csv"))

rm(l_res_lung, temp)
gc()
Sys.sleep(60)

#' _____ Lymphoma

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Lymphoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Lymphoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                include_additive_hzds = TRUE,
                                                include_cure_fraction = specify_cure,
                                                include_Loo = FALSE, plotsurvhz = FALSE,
                                                survextrap_add_knotts_interval = NA,
                                                survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Lymphoma), paste0(specify_res_save_path, specify_meth_name, "_LE_Lymphoma.csv"))
write_csv(f_LE_errors(l_res_Lymphoma), paste0(specify_res_save_path, specify_meth_name, "_errors_Lymphoma.csv"))

rm(l_res_Lymphoma, temp)
gc()
Sys.sleep(60)

#' _____ Melanoma

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Melanoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Melanoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                include_additive_hzds = TRUE,
                                                include_cure_fraction = specify_cure,
                                                include_Loo = FALSE, plotsurvhz = FALSE,
                                                survextrap_add_knotts_interval = NA,
                                                survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Melanoma), paste0(specify_res_save_path, specify_meth_name, "_LE_Melanoma.csv"))
write_csv(f_LE_errors(l_res_Melanoma), paste0(specify_res_save_path, specify_meth_name, "_errors_Melanoma.csv"))

rm(l_res_Melanoma, temp)
gc()
Sys.sleep(60)

#' _____ Ovary

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Ovary") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Ovary <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                   .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                             include_additive_hzds = TRUE,
                                             include_cure_fraction = specify_cure,
                                             include_Loo = FALSE, plotsurvhz = FALSE,
                                             survextrap_add_knotts_interval = NA,
                                             survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Ovary), paste0(specify_res_save_path, specify_meth_name, "_LE_Ovary.csv"))
write_csv(f_LE_errors(l_res_Ovary), paste0(specify_res_save_path, specify_meth_name, "_errors_Ovary.csv"))

rm(l_res_Ovary, temp)
gc()
Sys.sleep(60)

#' _____ Pancreas

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Pancreas") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Pancreas <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                include_additive_hzds = TRUE,
                                                include_cure_fraction = specify_cure,
                                                include_Loo = FALSE, plotsurvhz = FALSE,
                                                survextrap_add_knotts_interval = NA,
                                                survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Pancreas), paste0(specify_res_save_path, specify_meth_name, "_LE_Pancreas.csv"))
write_csv(f_LE_errors(l_res_Pancreas), paste0(specify_res_save_path, specify_meth_name, "_errors_Pancreas.csv"))

rm(l_res_Pancreas, temp)
gc()
Sys.sleep(60)

#' _____ Prostate

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Prostate") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Prostate <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                include_additive_hzds = TRUE,
                                                include_cure_fraction = specify_cure,
                                                include_Loo = FALSE, plotsurvhz = FALSE,
                                                survextrap_add_knotts_interval = NA,
                                                survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Prostate), paste0(specify_res_save_path, specify_meth_name, "_LE_Prostate.csv"))
write_csv(f_LE_errors(l_res_Prostate), paste0(specify_res_save_path, specify_meth_name, "_errors_Prostate.csv"))

rm(l_res_Prostate, temp)
gc()
Sys.sleep(60)

#' _____ Sarcoma

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Sarcoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Sarcoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                               include_additive_hzds = TRUE,
                                               include_cure_fraction = specify_cure,
                                               include_Loo = FALSE, plotsurvhz = FALSE,
                                               survextrap_add_knotts_interval = NA,
                                               survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Sarcoma), paste0(specify_res_save_path, specify_meth_name, "_LE_Sarcoma.csv"))
write_csv(f_LE_errors(l_res_Sarcoma), paste0(specify_res_save_path, specify_meth_name, "_errors_Sarcoma.csv"))

rm(l_res_Sarcoma, temp)
gc()
Sys.sleep(60)

#' _____ Stomach

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Stomach") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Stomach <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                               include_additive_hzds = TRUE,
                                               include_cure_fraction = specify_cure,
                                               include_Loo = FALSE, plotsurvhz = FALSE,
                                               survextrap_add_knotts_interval = NA,
                                               survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Stomach), paste0(specify_res_save_path, specify_meth_name, "_LE_Stomach.csv"))
write_csv(f_LE_errors(l_res_Stomach), paste0(specify_res_save_path, specify_meth_name, "_errors_Stomach.csv"))

rm(l_res_Stomach, temp)
gc()
Sys.sleep(60)

#' _____ Thyroid

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Thyroid") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Thyroid <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                               include_additive_hzds = TRUE,
                                               include_cure_fraction = specify_cure,
                                               include_Loo = FALSE, plotsurvhz = FALSE,
                                               survextrap_add_knotts_interval = NA,
                                               survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Thyroid), paste0(specify_res_save_path, specify_meth_name, "_LE_Thyroid.csv"))
write_csv(f_LE_errors(l_res_Thyroid), paste0(specify_res_save_path, specify_meth_name, "_errors_Thyroid.csv"))

rm(l_res_Thyroid, temp)
gc()
Sys.sleep(60)

#' _____ Urothelial Tract

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Urothelial Tract") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Urothelial_Tract <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                              .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                                        include_additive_hzds = TRUE,
                                                        include_cure_fraction = specify_cure,
                                                        include_Loo = FALSE, plotsurvhz = FALSE,
                                                        survextrap_add_knotts_interval = NA,
                                                        survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Urothelial_Tract), paste0(specify_res_save_path, specify_meth_name, "_LE_Urothelial_Tract.csv"))
write_csv(f_LE_errors(l_res_Urothelial_Tract), paste0(specify_res_save_path, specify_meth_name, "_errors_Urothelial_Tract.csv"))

rm(l_res_Urothelial_Tract, temp)
gc()
Sys.sleep(60)

#' _____ Uterus

temp <- df_surv_byHR %>% filter(NCRAS_Draw == "Uterus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Uterus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp(survival = .x, aggregate_surv = TRUE,
                                              include_additive_hzds = TRUE,
                                              include_cure_fraction = specify_cure,
                                              include_Loo = FALSE, plotsurvhz = FALSE,
                                              survextrap_add_knotts_interval = NA,
                                              survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))

write_csv(f_LE_summary_table(l_res_Uterus), paste0(specify_res_save_path, specify_meth_name, "_LE_Uterus.csv"))
write_csv(f_LE_errors(l_res_Uterus), paste0(specify_res_save_path, specify_meth_name, "_errors_Uterus.csv"))

rm(l_res_Uterus, temp)
gc()
Sys.sleep(60)






#' OlD before move to only using aggregate survival data (although this now in question)
#' Do not need for CE
#' #' _____________________________________________________________________________
#' #' _____ 3. Combined categories - All sites, all sites excluding prostate, 12-prespecified
#' #'
#' #' Note these will be far slower to run. Instead of the f_life_exp function, the
#' #' f_life_exp_multiple_cancers function is instead used. The differences between the functions is that
#' #' the f_life_exp_multiple_cancers function performs the time to event data reconstruction for the cancer types
#' #' specified, and then combines it before fitting the Bayesian FPSM
#'
#' #' _____ All sites
#' temp <- df_surv_byHR
#'
#' l_res_all <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
#'                  .f =  safely( ~f_life_exp_multiple_cancers(survival = .x, aggregate_surv = TRUE,
#'                                                             include_additive_hzds = TRUE,
#'                                                             include_cure_fraction = specify_cure,
#'                                                             include_Loo = FALSE, plotsurvhz = FALSE,
#'                                                             survextrap_add_knotts_interval = NA,
#'                                                             survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))
#'
#' write_csv(f_LE_summary_table(l_res_all, single = FALSE), paste0(specify_res_save_path, specify_meth_name, "_LE_all.csv"))
#' write_csv(f_LE_errors(l_res_all), paste0(specify_res_save_path, specify_meth_name, "_errors_all.csv"))
#'
#' rm(l_res_all, temp)
#' gc()
#' Sys.sleep(60)
#'
#' #' _____ All sites exc prostate
#' temp <- df_surv_byHR %>%
#'         filter(NCRAS_Draw != "Prostate") %>%
#'         mutate(across(where(is.factor), as.character)) %>%
#'         mutate(across(where(is.character), as.factor))
#'
#' l_res_allexcprostate <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
#'                             .f =  safely( ~f_life_exp_multiple_cancers(survival = .x, aggregate_surv = TRUE,
#'                                                                        include_additive_hzds = TRUE,
#'                                                                        include_cure_fraction = specify_cure,
#'                                                                        include_Loo = FALSE, plotsurvhz = FALSE,
#'                                                                        survextrap_add_knotts_interval = NA,
#'                                                                        survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))
#'
#' write_csv(f_LE_summary_table(l_res_allexcprostate, single = FALSE), paste0(specify_res_save_path, specify_meth_name, "_LE_allexcprostate.csv"))
#' write_csv(f_LE_errors(l_res_allexcprostate), paste0(specify_res_save_path, specify_meth_name, "_errors_allexcprostate.csv"))
#'
#' rm(l_res_allexcprostate, temp)
#' gc()
#' Sys.sleep(60)
#'
#' #' _____ 12-prespecified (ex Plasma cell)
#'
#' temp <- df_surv_byHR %>%
#'         filter(NCRAS_Draw %in% c("Anus", "Bladder", "Colon/Rectum", "Esophagus", "Head and Neck", "Liver/Bile-duct", "Lung", "Lymphoma", "Ovary", "Pancreas", "Stomach")) %>%
#'         mutate(across(where(is.factor), as.character)) %>%
#'         mutate(across(where(is.character), as.factor))
#'
#' l_res_12deadly <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
#'                       .f =  safely( ~f_life_exp_multiple_cancers(survival = .x, aggregate_surv = TRUE,
#'                                                                  include_additive_hzds = TRUE,
#'                                                                  include_cure_fraction = specify_cure,
#'                                                                  include_Loo = FALSE, plotsurvhz = FALSE,
#'                                                                  survextrap_add_knotts_interval = NA,
#'                                                                  survextrap_fit_method = specify_method, survextrap_niter = specify_n_interation)))
#'
#' write_csv(f_LE_summary_table(l_res_12deadly, single = FALSE), paste0(specify_res_save_path, specify_meth_name, "_LE_12deadly.csv"))
#' write_csv(f_LE_errors(l_res_12deadly), paste0(specify_res_save_path, specify_meth_name, "_errors_12deadly.csv"))
#'
#' rm(l_res_12deadly, temp)
#' gc()
#' Sys.sleep(60)
#'
#'





