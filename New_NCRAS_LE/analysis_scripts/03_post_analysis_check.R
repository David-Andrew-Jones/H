#' _____________________________________________________________________________
#' _____ 03_post_analysis_check

#' This script performs checks on the model runs
#' An all combinations data frame is created marking all the patient presentations
#' (based on the OS 10 year request) the model should be run on. Dataframes are then created
#' to see the patient presentations where an estimate was not generated (due to an error)

#' _____________________________________________________________________________
#' _____ 1. Post 02 scrip check - check the model run outputs (results and any errors)

#' Repeat raw results locations and name in case running at a different time to 02 script
#' Path names of primary analysis outcomes
res_save_path_primary <- paste0(getwd(), "/results/primary_raw_tables/")
res_meth_name_primary <- "opt_nocure_relsurv"

#' Path names of sensitivity analysis outcomes
res_save_path_sens <- paste0(getwd(), "/results/sensitivity_raw_tables/")
res_meth_name_sens <- "opt_cure_relsurv"

#' Path to save
results_path <- paste0(getwd(), "/results/results_tables/")

#' Create template dataframe with all potential combinations
#' Not the net survival data is only 50-79
df_all_combos <- expand.grid(Sex = c("Male","Female"),
                             NCRAS_Draw = c("all", "allexcprostate", "12deadly", "Anus", "Bladder", "Breast", "Cervix", "Colon/Rectum", "Esophagus", "Gallbladder",
                                               "Head and Neck", "Kidney", "Liver/Bile-duct", "Lung", "Lymphoma", "Melanoma", "Ovary",
                                               "Pancreas", "Prostate", "Sarcoma", "Stomach", "Thyroid", "Urothelial Tract", "Uterus"),
                             Stage = c("I", "II", "III", "IV"),
                             Age = c("40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+"),
                             haz_ratio = c(1, 2),
                             cfDNA_status = c("negative", "positive")) %>%
        filter(!(haz_ratio == 1 & cfDNA_status == "positive"))


#' Check to see which patient presentations produces errors and save
df_all_combos_primary <- df_all_combos %>%
        left_join(list.files(path = res_save_path_primary, pattern = paste0( res_meth_name_primary, "_LE")) %>%
                          map_df(~read_csv(paste0(res_save_path_primary, .))) %>%
                          mutate(modelled = "Yes"),
                  by = c("Sex", "NCRAS_Draw", "Stage", "Age", "haz_ratio", "cfDNA_status")) %>%
        # Remove the cancer type-sex incompatible rows
        filter(!(Sex == "Male" & NCRAS_Draw == "Cervix")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Breast")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Ovary")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Uterus")) %>%
        filter(!(Sex == "Female" & NCRAS_Draw == "Prostate")) %>%
        rename(`Cancer type` = "NCRAS_Draw",
               `cfDNA status` = "cfDNA_status",
               HR = haz_ratio) %>%
        select(Sex, `Cancer type`, Stage, Age, HR, `cfDNA status`, modelled) %>%
        pivot_wider(names_from = "Age", values_from = "modelled") %>%
        arrange(Sex, `Cancer type`, Stage) %>%
        # Create a column sum row to see errors by age
        mutate(across(`40-44`:`85+`, ~ case_when(. == "Yes" ~ 1, .default = 0))) %>%
        bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

write.csv(df_all_combos_primary, paste0(results_path, "model_convergence_primary.csv"))


#' Do same for the sensitivity, but we may decide not to run it for all the cancer types
df_all_combos_sens <- df_all_combos %>%
        left_join(list.files(path = res_save_path_sens, pattern = paste0( res_meth_name_sens, "_LE")) %>%
                          map_df(~read_csv(paste0(res_save_path_sens, .))) %>%
                          mutate(modelled = "Yes"),
                  by = c("Sex", "NCRAS_Draw", "Stage", "Age", "haz_ratio", "cfDNA_status")) %>%
        # Remove the cancer type-sex incompatible rows
        filter(!(Sex == "Male" & NCRAS_Draw == "Cervix")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Breast")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Ovary")) %>%
        filter(!(Sex == "Male" & NCRAS_Draw == "Uterus")) %>%
        filter(!(Sex == "Female" & NCRAS_Draw == "Prostate")) %>%
        rename(`Cancer type` = "NCRAS_Draw",
               `cfDNA status` = "cfDNA_status",
               HR = haz_ratio) %>%
        select(Sex, `Cancer type`, Stage, Age, HR, `cfDNA status`, modelled) %>%
        pivot_wider(names_from = "Age", values_from = "modelled") %>%
        arrange(Sex, `Cancer type`, Stage) %>%
        # Create a column sum row to see errors by age
        mutate(across(`40-44`:`85+`, ~ case_when(. == "Yes" ~ 1, .default = 0))) %>%
        bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

write.csv(df_all_combos_sens, paste0(results_path, "model_convergence_sens.csv"))



