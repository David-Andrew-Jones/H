#______________________________________________________________________________#
#____ 02_analysis_M_spline_splitbysite

#' Specification in which results are output in batch.
#' This was written due to frequency of crashing when all run in function - RAM issue

rm(df_surv_byHR, surv,surv_full, df_surv_formatted)
#______________________________________________________________________________#
#'____ Results for each site by stage sex and age
#'

#______________________________________________________________________________#
#'____ Anus
#'

options(mc.cores = 100)

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Anus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))


l_res_anus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                  .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                               include_additive_hzds = TRUE,
                                                               include_cure_fraction = FALSE,
                                                               plotsurvhz = FALSE,
                                                               survextrap_add_knotts_interval = NA,
                                                               survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_anus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Anus"))
write_csv(f_LE_errors(l_res_anus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Anus"))

rm(l_res_anus, temp)
gc()
Sys.sleep(60)
#'____ Bladder

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Bladder") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Bladder <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                  include_additive_hzds = TRUE,
                                                                  include_cure_fraction = FALSE,
                                                                  plotsurvhz = FALSE,
                                                                  survextrap_add_knotts_interval = NA,
                                                                  survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Bladder), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Bladder"))
write_csv(f_LE_errors(l_res_Bladder), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Bladder"))

rm(l_res_Bladder, temp)
gc()
Sys.sleep(60)

#'____ Breast

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Breast") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))


l_res_Breast <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Breast), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Breast"))
write_csv(f_LE_errors(l_res_Breast), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Breast"))

rm(l_res_Breast, temp)
gc()
Sys.sleep(60)

#'____ Cervix

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Cervix") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Cervix <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Cervix), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Cervix"))
write_csv(f_LE_errors(l_res_Cervix), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Cervix"))

rm(l_res_Cervix, temp)
gc()
Sys.sleep(60)

#'____ Colon/Rectum
#'
temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Colon/Rectum") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_ColonRectum <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                         .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                      include_additive_hzds = TRUE,
                                                                      include_cure_fraction = FALSE,
                                                                      plotsurvhz = FALSE,
                                                                      survextrap_add_knotts_interval = NA,
                                                                      survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_ColonRectum), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_ColonRectum"))
write_csv(f_LE_errors(l_res_ColonRectum), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_ColonRectum"))

rm(l_res_ColonRectum, temp)
gc()
Sys.sleep(60)
#'____ Esophagus

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Esophagus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Esophagus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                       .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                    include_additive_hzds = TRUE,
                                                                    include_cure_fraction = FALSE,
                                                                    plotsurvhz = FALSE,
                                                                    survextrap_add_knotts_interval = NA,
                                                                    survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Esophagus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Esophagus"))
write_csv(f_LE_errors(l_res_Esophagus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Esophagus"))

rm(l_res_Esophagus, temp)
gc()
Sys.sleep(60)
#'____ Gallbladder

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Gallbladder") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Gallbladder <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                         .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                      include_additive_hzds = TRUE,
                                                                      include_cure_fraction = FALSE,
                                                                      plotsurvhz = FALSE,
                                                                      survextrap_add_knotts_interval = NA,
                                                                      survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Gallbladder), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Gallbladder"))
write_csv(f_LE_errors(l_res_Gallbladder), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Gallbladder"))

rm(l_res_Gallbladder, temp)
gc()
Sys.sleep(60)
#'____ Head and Neck

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Head and Neck") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_HNC<- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                             include_additive_hzds = TRUE,
                                                             include_cure_fraction = FALSE,
                                                             plotsurvhz = FALSE,
                                                             survextrap_add_knotts_interval = NA,
                                                             survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_HNC), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_HNC"))
write_csv(f_LE_errors(l_res_HNC), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_HNC"))

rm(l_res_HNC, temp)
gc()
Sys.sleep(60)
#'____ Kidney

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Kidney") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Kidney <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Kidney), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Kidney"))
write_csv(f_LE_errors(l_res_Kidney), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Kidney"))

rm(l_res_Kidney, temp)
gc()
Sys.sleep(60)
#'____ Liver/Bile-duct

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Liver/Bile-duct") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_LiverBileduct <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                           .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                        include_additive_hzds = TRUE,
                                                                        include_cure_fraction = FALSE,
                                                                        plotsurvhz = FALSE,
                                                                        survextrap_add_knotts_interval = NA,
                                                                        survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_LiverBileduct), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_LiverBileduct"))
write_csv(f_LE_errors(l_res_LiverBileduct), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_LiverBileduct"))

rm(l_res_LiverBileduct, temp)
gc()
Sys.sleep(60)
#'____ Lung

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Lung") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        filter(haz_ratio ==1) %>%
        filter(cfDNA_status == "negative")

l_res_lung <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                  .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                               include_additive_hzds = TRUE,
                                                               include_cure_fraction = FALSE,
                                                               plotsurvhz = FALSE,
                                                               survextrap_add_knotts_interval = NA,
                                                               survextrap_fit_method = "opt", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_lung), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Lung"))
write_csv(f_LE_errors(l_res_lung), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Lung"))

rm(l_res_lung, temp)
gc()
Sys.sleep(60)

#'____ Lymphoma

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Lymphoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Lymphoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                   include_additive_hzds = TRUE,
                                                                   include_cure_fraction = FALSE,
                                                                   plotsurvhz = FALSE,
                                                                   survextrap_add_knotts_interval = NA,
                                                                   survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Lymphoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Lymphoma"))
write_csv(f_LE_errors(l_res_Lymphoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Lymphoma"))


rm(l_res_Lymphoma, temp)
gc()
Sys.sleep(60)
#'____ Melanoma

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Melanoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Melanoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                   include_additive_hzds = TRUE,
                                                                   include_cure_fraction = FALSE,
                                                                   plotsurvhz = FALSE,
                                                                   survextrap_add_knotts_interval = NA,
                                                                   survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Melanoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Melanoma"))
write_csv(f_LE_errors(l_res_Melanoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Melanoma"))

rm(l_res_Melanoma, temp)
gc()
Sys.sleep(60)
#'____ Ovary

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Ovary") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Ovary <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                   .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                include_additive_hzds = TRUE,
                                                                include_cure_fraction = FALSE,
                                                                plotsurvhz = FALSE,
                                                                survextrap_add_knotts_interval = NA,
                                                                survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Ovary), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Ovary"))
write_csv(f_LE_errors(l_res_Ovary), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Ovary"))

rm(l_res_Ovary, temp)
gc()
Sys.sleep(60)
#'____ Pancreas

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Pancreas") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Pancreas <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                   include_additive_hzds = TRUE,
                                                                   include_cure_fraction = FALSE,
                                                                   plotsurvhz = FALSE,
                                                                   survextrap_add_knotts_interval = NA,
                                                                   survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Pancreas), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Pancreas"))
write_csv(f_LE_errors(l_res_Pancreas), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Pancreas"))

rm(l_res_Pancreas, temp)
gc()
Sys.sleep(60)
#'____ Prostate

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Prostate") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Prostate <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                      .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                   include_additive_hzds = TRUE,
                                                                   include_cure_fraction = FALSE,
                                                                   plotsurvhz = FALSE,
                                                                   survextrap_add_knotts_interval = NA,
                                                                   survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Prostate), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Prostate"))
write_csv(f_LE_errors(l_res_Prostate), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Prostate"))

rm(l_res_Prostate, temp)
gc()
Sys.sleep(60)
#'____ Sarcoma

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Sarcoma") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Sarcoma <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                  include_additive_hzds = TRUE,
                                                                  include_cure_fraction = FALSE,
                                                                  plotsurvhz = FALSE,
                                                                  survextrap_add_knotts_interval = NA,
                                                                  survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Sarcoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Sarcoma"))
write_csv(f_LE_errors(l_res_Sarcoma), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Sarcoma"))

rm(l_res_Sarcoma, temp)
gc()
Sys.sleep(60)
#'____ Stomach

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Stomach") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Stomach <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                  include_additive_hzds = TRUE,
                                                                  include_cure_fraction = FALSE,
                                                                  plotsurvhz = FALSE,
                                                                  survextrap_add_knotts_interval = NA,
                                                                  survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Stomach), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Stomach"))
write_csv(f_LE_errors(l_res_Stomach), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Stomach"))

rm(l_res_Stomach, temp)
gc()
Sys.sleep(60)
#'____ Thyroid

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Thyroid") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Thyroid <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                     .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                  include_additive_hzds = TRUE,
                                                                  include_cure_fraction = FALSE,
                                                                  plotsurvhz = FALSE,
                                                                  survextrap_add_knotts_interval = NA,
                                                                  survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Thyroid), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Thyroid"))
write_csv(f_LE_errors(l_res_Thyroid), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Thyroid"))

rm(l_res_Thyroid, temp)
gc()
Sys.sleep(60)
#'____ Urothelial Tract

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Urothelial Tract") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Urothelial_Tract <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                              .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                           include_additive_hzds = TRUE,
                                                                           include_cure_fraction = FALSE,
                                                                           plotsurvhz = FALSE,
                                                                           survextrap_add_knotts_interval = NA,
                                                                           survextrap_fit_method = "mcmc", survextrap_niter = 500)))


write_csv(f_LE_summary_table(l_res_Urothelial_Tract), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Urothelial_Tract"))
write_csv(f_LE_errors(l_res_Urothelial_Tract), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Urothelial_Tract"))

rm(l_res_Urothelial_Tract, temp)
gc()
Sys.sleep(60)
#'____ Uterus

temp <- df_surv_byHR_CF %>% filter(NCRAS_Draw == "Uterus") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_Uterus <- map(.x = split(temp, list(temp$Sex, temp$NCRAS_Draw, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp_from_5year_netsurv(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 include_cure_fraction = FALSE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_Uterus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Uterus"))
write_csv(f_LE_errors(l_res_Uterus), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Uterus"))

rm(l_res_Uterus, temp)
gc()
Sys.sleep(60)


#______________________________________________________________________________#
#'____ All sites, all sites excluding prostate, 12-prespecified

#' All sites

temp <- df_surv_byHR_CF

l_res_all <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                    .f =  safely( ~f_life_exp_from_5year_net_surv_multiple_cancers(survival = .x,
                                                                 include_additive_hzds = TRUE,
                                                                 plotsurvhz = FALSE,
                                                                 survextrap_add_knotts_interval = NA,
                                                                 survextrap_fit_method = "opt", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_all), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_All"))
write_csv(f_LE_errors(l_res_all), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_All"))

rm(l_res_all, temp)
gc()
Sys.sleep(60)


#' All sites exc prostate

temp <- df_surv_byHR_CF %>%
        filter(NCRAS_Draw != "Prostate") %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_allexcprostate <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                 .f =  safely( ~f_life_exp_from_5year_net_surv_multiple_cancers(survival = .x,
                                                              include_additive_hzds = TRUE,
                                                              plotsurvhz = FALSE,
                                                              survextrap_add_knotts_interval = NA,
                                                              survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_allexcprostate), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_Allexcprostate"))
write_csv(f_LE_errors(l_res_allexcprostate), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_Allexcprostate"))

rm(l_res_allexcprostate, temp)
gc()
Sys.sleep(60)

#' 12-prespecified

temp <- df_surv_byHR_CF %>%
        filter(NCRAS_Draw %in% c("Anus", "Bladder", "Colon/Rectum", "Esophagus", "Head and Neck", "Liver/Bile-duct", "Lung", "Lymphoma", "Ovary", "Pancreas", "Stomach")) %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(across(where(is.character), as.factor))

l_res_12deadly <- map(.x = split(temp, list(temp$Sex, temp$Stage, temp$Age, temp$haz_ratio, temp$cfDNA_status)),
                            .f =  safely( ~f_life_exp_from_5year_net_surv_multiple_cancers(survival = .x,
                                                                         include_additive_hzds = TRUE,
                                                                         plotsurvhz = FALSE,
                                                                         survextrap_add_knotts_interval = NA,
                                                                         survextrap_fit_method = "mcmc", survextrap_niter = 500)))

write_csv(f_LE_summary_table(l_res_12deadly), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_LE_12deadly"))
write_csv(f_LE_errors(l_res_12deadly), paste0("~/GalleriCEmodel/analyses/NICE_modifier/Life_expectancy/results/", "nocure_MCMC_errors_12deadly"))

rm(l_res_12deadly, temp)
gc()
Sys.sleep(60)







