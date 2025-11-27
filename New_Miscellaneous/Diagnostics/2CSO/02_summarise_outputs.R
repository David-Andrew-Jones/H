#______________________________________________________________________________#
################################################################################
#___ Basic model of diagnostic use in the MCBTP

################################################################################
#______________________________________________________________________________#
#' Scripts
source("~/Miscellaneous/Diagnostics/01_diag_impact_model.R")

#______________________________________________________________________________#
#' Data
df_diag_use_England2022 <- read_csv("~/Miscellaneous/Diagnostics/NHS_England_month_diagnostic_activity2023.csv") %>%
        mutate(annual_23 = rowSums(select(., contains("23"))))

#______________________________________________________________________________#
#____ Summarise results for the scenarios by imaging type

df_all_scenarios_imaging <- list(df_scenario_1_imaging, df_scenario_2_imaging, df_scenario_3_imaging, df_scenario_4_imaging, df_scenario_9_imaging,
                          df_scenario_6_imaging, df_scenario_7_imaging, df_scenario_8_imaging, df_scenario_9_imaging, df_scenario_10_imaging) %>%
        reduce(left_join, by = "imaging_type")

res_sum_byimagingtype <- df_all_scenarios_imaging %>%
        mutate(total_sum_byimagingtype = rowSums(select(., scenario_1_total_use_byimagine_type:scenario_10_total_use_byimagine_type), na.rm=TRUE)) %>%
        select(imaging_type, total_sum_byimagingtype) %>%
        mutate(total_imaging = sum(total_sum_byimagingtype))

#____ Compare to annual English usage

result_annual_increase <- gt(res_sum_byimagingtype %>%
        mutate(imaging_type = case_when(imaging_type == "CT" ~ "Computed Tomography",
                                        imaging_type == "MRI" ~ "Magnetic Resonance Imaging",
                                        .default = imaging_type)) %>%
        left_join(df_diag_use_England2022 %>% select(imaging_type, annual_23), by = c("imaging_type")) %>%
        mutate(combined_total = total_sum_byimagingtype + annual_23,
               percent_increase = ((combined_total - annual_23) / annual_23) * 100) %>%
        select(imaging_type, total_sum_byimagingtype, annual_23, percent_increase) %>%
        mutate(across(where(is.numeric), \(x) round(x , 1))) %>%
        rename(`Imaging modality` = imaging_type,
               `Total usage in England 2023` = annual_23,
               `Estimated use associated with an MCED screening prevalent round` = total_sum_byimagingtype,
               `Percentage increase in annual usage` = percent_increase)) %>%
        tab_options(column_labels.background.color = "gray",
                    column_labels.font.weight = "bold") %>%
        gtsave("results_fullprogramme.docx")

#______________________________________________________________________________#
