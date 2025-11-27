#' _____________________________________________________________________________
#' _____ Use the cost-effectiveness model with preliminary England data
#' _____________________________________________________________________________
rm(list = ls())
#' _____ Data
#' Put all data in Global environment. These can then be changed depending on the model run
data_dir <- "~/GalleriCEmodel/data-raw/England_pilot_planning/"

#'data_dir <-"~/GalleriCEmodel/data-raw/US_data_082023/"
#' Put all data in Global environment. These can then be changed depending on the model run
setwd(data_dir)

# Put data in global environment
l_data_file <- list.files(path = path.expand(data_dir), pattern="csv") %>%
        map(~ as.data.frame(read_csv(.x, col_names = FALSE, show_col_types = FALSE)))

names(l_data_file) <- tools::file_path_sans_ext(
        list.files(path = path.expand(data_dir), pattern="csv")
)

list2env(l_data_file, env = .GlobalEnv)

check_USdata  <- f_run_model(run_desc = "test",
                             export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                             return_M_trace = TRUE,
                             # From iSetting in original
                             model_start_age = 50,
                             time_horizon = 100,
                             mced_screen_min_age = 55,
                             mced_screen_max_age = 79,
                             perc_female = 0.53333333333,
                             disc_health = 0.03,
                             disc_cost = 0.03,
                             # From iMisc in original
                             mced_cost = 949)
#' _____________________________________________________________________________


list_outcomes_2rounds <- vector("list", length = 30)
names(list_outcomes_2rounds) <- seq(from = 50, to = 79, by = 1)

temp <- f_run_model(run_desc = "test",
                    export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                    return_M_trace = TRUE,
                    # From iSetting in original
                    model_start_age = 50,
                    mced_screen_min_age = 50,
                    mced_screen_max_age = 79)

as.data.frame(temp$res_number_outcomes_timehorizon) %>%
        rownames_to_column(var = "Stage")

for(i in 1:29){

        # Refresh global variables after each run
        l_data_file <- list.files(path = path.expand(data_dir), pattern="csv") %>%
                map(~ as.data.frame(read_csv(.x, col_names = FALSE, show_col_types = FALSE)))

        names(l_data_file) <- tools::file_path_sans_ext(
                list.files(path = path.expand(data_dir), pattern="csv")
        )

        list2env(l_data_file, env = .GlobalEnv)

        temp <- f_run_model(run_desc = "test",
                            export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                            return_M_trace = TRUE,
                            # From iSetting in original
                            model_start_age = 50,
                            mced_screen_min_age = 49+i,
                            mced_screen_max_age = 80,
                            time_horizon = 50+i)

        list_outcomes_2rounds[[i]] <- as.data.frame(temp$res_number_outcomes_timehorizon) %>%
                rownames_to_column(var = "Stage")


}

# Divide each one by 100,000 to give result per 1 patient
# Scale each age's result by their proportion in the age group
# Times by number of patients

outcomes_2rounds <- bind_rows(list_outcomes_2rounds) %>%
        cbind(rep(c(0.0381, 0.0396, 0.0410, 0.0399, 0.0408, 0.0406, 0.0408, 0.0406, 0.0407, 0.0401, 0.0391, 0.0381,
                    0.0366, 0.0350, 0.0340, 0.0329, 0.0315, 0.0301, 0.0288, 0.0288, 0.0281, 0.0271, 0.0270, 0.0272,
                    0.0276, 0.0288, 0.0308, 0.0231, 0.0220, 0.0213), each=4)) %>%
        group_by(Stage) %>%
        summarise(across(`MCED+SoC`:`SoC`), \(x) sum(x)) %>%
        mutate(perc_stageshift = ((`MCED+SoC`- `SoC`) / `SoC`) * 100)




#' _____________________________________________________________________________
#' 1. Varying start age and end age of screening
#' _____
#' 1.1 Start Age 50
#' 1.1.1 End Age 64
results_Start50_End64 <- f_run_model(run_desc = "results_Start50_End64",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 50,
                                     mced_screen_min_age = 50,
                                     mced_screen_max_age = 64,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 1.1.2 End Age 69
results_Start50_End69 <- f_run_model(run_desc = "results_Start50_End69",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 50,
                                     mced_screen_min_age = 50,
                                     mced_screen_max_age = 69,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 1.1.3 End Age 74
results_Start50_End74 <- f_run_model(run_desc = "results_Start50_End74",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 50,
                                     mced_screen_min_age = 50,
                                     mced_screen_max_age = 74,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 1.1.4 End Age 79
results_Start50_End79 <- f_run_model(run_desc = "results_Start50_End79",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 50,
                                     mced_screen_min_age = 50,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)

#' _____
#' 1.2 Start Age 55
#' 1.2.1 End Age 64
results_Start55_End64 <- f_run_model(run_desc = "results_Start55_End64",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 55,
                                     mced_screen_min_age = 55,
                                     mced_screen_max_age = 64,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 1.2.2 End Age 69
results_Start55_End69 <- f_run_model(run_desc = "results_Start55_End69",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 55,
                                     mced_screen_min_age = 55,
                                     mced_screen_max_age = 69,
                                     # From iMisc in original
                                     mced_cost = 949)
#' 1.2.3 End Age 74
results_Start55_End74 <- f_run_model(run_desc = "results_Start55_End74",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 55,
                                     mced_screen_min_age = 55,
                                     mced_screen_max_age = 74,
                                     # From iMisc in original
                                     mced_cost = 949)
#' 1.2.4 End Age 79
results_Start55_End79 <- f_run_model(run_desc = "results_Start55_End79",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 55,
                                     mced_screen_min_age = 55,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)
#' _____
#' 1.3 Start Age 60
#' 1.3.1 End Age 69
results_Start60_End69 <- f_run_model(run_desc = "results_Start60_End69",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 60,
                                     mced_screen_min_age = 60,
                                     mced_screen_max_age = 69,
                                     # From iMisc in original
                                     mced_cost = 949)
#' 1.3.2 End Age 74
results_Start60_End74 <- f_run_model(run_desc = "results_Start60_End74",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 60,
                                     mced_screen_min_age = 60,
                                     mced_screen_max_age = 74,
                                     # From iMisc in original
                                     mced_cost = 949)
#' 1.3.3 End Age 79
results_Start60_End79 <- f_run_model(run_desc = "results_Start60_End79",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 60,
                                     mced_screen_min_age = 60,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)
#' _____
#' 1.4 Start Age 65
#' 1.4.1 End Age 74
results_Start65_End74 <- f_run_model(run_desc = "results_Start65_End74",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 65,
                                     mced_screen_min_age = 65,
                                     mced_screen_max_age = 74,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 1.4.2 End Age 79
results_Start65_End79 <- f_run_model(run_desc = "results_Start65_End79",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     data_dir = "~/GalleriCEmodel/data-raw/US_data_082023/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 65,
                                     mced_screen_min_age = 65,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)

#' _____
#' 1.4 Start Age 70

#' 1.4.1 End Age 79
results_Start70_End79 <- f_run_model(run_desc = "results_Start65_End79",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 70,
                                     mced_screen_min_age = 70,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)


#' _____________________________________________________________________________
#' 2. Increased hazard
#' 2.1 Hazard ratio between cfDNA- and cfDNA+ of 1.5

results_Start50_End79_cfDNAhazard1.5 <- f_run_model(run_desc = "results_Start50_End79_cfDNAhazard1.5",
                                     export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                     return_M_trace = TRUE,
                                     # From iSetting in original
                                     model_start_age = 70,
                                     mced_screen_min_age = 70,
                                     mced_screen_max_age = 79,
                                     # From iMisc in original
                                     mced_cost = 949)

#' 2.2 Hazard ratio between cfDNA- and cfDNA+ of 2
results_Start50_End79_cfDNAhazard2 <- f_run_model(run_desc = "results_Start50_End79_cfDNAhazard2",
                                                    export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                                    return_M_trace = TRUE,
                                                    # From iSetting in original
                                                    model_start_age = 50,
                                                    mced_screen_min_age = 50,
                                                    mced_screen_max_age = 79,
                                                    # From iMisc in original
                                                    mced_cost = 949)
#' _____________________________________________________________________________
#' 3. Compliance: 100%, 90%, 80% to annual screening
#' 3.1 Compliance 100%
results_Start50_End79_compliance100 <- f_run_model(run_desc = "results_Start50_End79_compliance100",
                                                    export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                                    return_M_trace = TRUE,
                                                    # From iSetting in original
                                                    model_start_age = 70,
                                                    mced_screen_min_age = 70,
                                                    mced_screen_max_age = 79,
                                                    annual_compliance = 1, # Annual compliance
                                                   # From iMisc in original
                                                    mced_cost = 949)

#' 3.1 Compliance 90%
results_Start50_End79_compliance90 <- f_run_model(run_desc = "results_Start50_End79_compliance90",
                                                  export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                                  return_M_trace = TRUE,
                                                  # From iSetting in original
                                                  model_start_age = 50,
                                                  mced_screen_min_age = 50,
                                                  mced_screen_max_age = 79,
                                                  annual_compliance = 0.9, # Annual compliance
                                                  # From iMisc in original
                                                  mced_cost = 949)

#' 3.1 Compliance 80%
results_Start50_End79_compliance80 <- f_run_model(run_desc = "results_Start50_End79_compliance80",
                                                  export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                                                  return_M_trace = TRUE,
                                                  # From iSetting in original
                                                  model_start_age = 50,
                                                  mced_screen_min_age = 50,
                                                  mced_screen_max_age = 79,
                                                  annual_compliance = 0.8, # Annual compliance
                                                  # From iMisc in original
                                                  mced_cost = 949)










#' _____________________________________________________________________________
#' OLD - Generate English incidence in correct format


# # 19 cancer named as in the inc data for later ordering - note that in the US version there are two
# # breast sites (HR+-) at positions 5 and 12. position 5 will be used for breast cancer
# CE_model_cancer_order <- c("Lung","Colon/Rectum","Pancreas", "Liver/Bile duct", "Breast", "Oesophagus", "Head and Neck", "Stomach", "Ovary", "Kidney",
#                            "Prostate","Lymphoma","Anus", "Uterus", "Bladder", "Cervix","Urothelial Tract","Other")
#
# df_incidence_50to79<- readxl::read_xlsx("~/uk_modelled_mortality_08Feb24/data/grail_incidence_formatted_v2.xlsx") %>%
#         # Note age at incidence does not go beyond 75-79 so will have to use that figure for later ages
#         filter(agegroup == "50-54" | agegroup == "55-59" | agegroup == "60-64" | agegroup == "65-69" | agegroup == "70-74"| agegroup == "75-79") %>%
#         filter(deprivationqunitile == "All deprivations") %>%
#         filter(subtype == "N/A") %>%
#         filter(stageatdiagnosis != "All stages" & stageatdiagnosis != "Unstageable/unknown/missing") %>%
#         filter((cancersite %in% c("Lung","Colon/Rectum","Pancreas","Liver/Bile duct","Breast","Oesophagus", "Head and Neck","Stomach",
#                                   "Kidney","Lymphoma","Anus","Bladder","Urothelial Tract","Other") & sex == "Persons") |
#                        (grepl(c("Ovary|Uterus|Cervix"), cancersite) & sex == "Females") |
#                        (grepl(c("Prostate"), cancersite) & sex == "Males")) %>%
#         mutate(cancersite = factor(cancersite, levels = CE_model_cancer_order)) %>%
#         select(cancersite, agegroup, stageatdiagnosis, count, population) %>%
#         group_by(agegroup) %>%
#         # Needed for the correct sex-specific denominator
#         mutate(population = max(population)) %>%
#         ungroup() %>%
#         mutate(rateper100000population = (100000/population) * count) %>%
#         select(cancersite, stageatdiagnosis, agegroup, rateper100000population) %>%
#         pivot_wider(names_from = agegroup, values_from = rateper100000population) %>%
#         arrange(cancersite) %>%
#         mutate("0-4" = 0,
#                "5-9" = 0,
#                "10-14" = 0,
#                "15-19" = 0,
#                "20-24" = 0,
#                "25-29" = 0,
#                "30-34" = 0,
#                "35-39" = 0,
#                "40-44" = 0,
#                "45-49" = 0, .before = `50-54`) %>%
#         mutate("80-84" = `75-79`,
#                "85-89" = `75-79`,
#                "90-94" = `75-79`,
#                "95-100" = `75-79`, .after = `75-79`) %>%
#         select(-cancersite, -stageatdiagnosis) %>%
#         ## add row of 0s for breast HR-
#         add_row(.before = 45) %>%
#         add_row(.before = 45) %>%
#         add_row(.before = 45) %>%
#         add_row(.before = 45) %>%
#         mutate(across(everything(), \(x) replace_na(x, 0)))
#
# write_csv(df_incidence_50to79, file = "~/GalleriCEmodel/data-raw/England_pilot_planning/incidence.csv", col_names = FALSE)
# write_csv(df_incidence_50to79, file = "~/GalleriCEmodel/data-raw/England_pilot_planning/incidence_Intercept.csv", col_names = FALSE)
