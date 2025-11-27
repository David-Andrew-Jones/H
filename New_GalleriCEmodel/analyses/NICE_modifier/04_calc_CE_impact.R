#' _____________________________________________________________________________

#' Calculate Absolute and Proportional QALY shortfall

#' _____________________________________________________________________________


res_no_modifier <- f_run_model(no_NICE_modifier = TRUE,
                               run_desc = "No_modifier",
                               export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                               data_dir = "~/GalleriCEmodel/data-raw/England_pilot_planning/",
                               return_M_trace = TRUE,
                               # From iSetting in original
                               model_start_age = 50,
                               soc_screen_min_age = 50,
                               soc_screen_max_age = 85,
                               mced_screen_min_age = 50,
                               mced_screen_max_age = 79,
                               time_horizon = 100,
                               perc_female = 0.53)

l_all_results <- list(`Number shifted: ` = as_tibble(res_no_modifier$time_shift_markov_trace_TS),
                      `From stage with no modifier: ` = as_tibble(res_no_modifier$time_shift_markov_trace_TS_nomodifier),
                      `From stage with 1.2 modifier: ` = as_tibble(res_no_modifier$time_shift_markov_trace_TS_1.2modifier),
                      `From stage with 1.7 modifier: ` = as_tibble(res_no_modifier$time_shift_markov_trace_TS_1.7modifier)) %>%
        map(., function(x){
                x %>% add_column(`Age at diagnosis` = 50:100, .before = 1) %>%
                        select(-contains("Cancer12"), -contains("Cancer19")) %>%
                rename_all(function(x) gsub("Cancer10", "Kidney", x))%>%
                        rename_all(function(x) gsub("Cancer11", "Prostate", x))%>%
                        rename_all(function(x) gsub("Cancer13", "Lymphoma", x))%>%
                        rename_all(function(x) gsub("Cancer14", "Anal", x))%>%
                        rename_all(function(x) gsub("Cancer15", "Uterine", x))%>%
                        rename_all(function(x) gsub("Cancer16", "Bladder", x))%>%
                        rename_all(function(x) gsub("Cancer17", "Cervical", x))%>%
                        rename_all(function(x) gsub("Cancer18", "Urothelial tract", x))%>%
                        rename_all(function(x) gsub("Cancer1", "Lung", x)) %>%
                        rename_all(function(x) gsub("Cancer2", "Colorectal", x)) %>%
                        rename_all(function(x) gsub("Cancer3", "Pancreatic", x))%>%
                        rename_all(function(x) gsub("Cancer4", "Liver/Bile-duct", x))%>%
                        rename_all(function(x) gsub("Cancer5", "Breast", x))%>%
                        rename_all(function(x) gsub("Cancer6", "Oesophageal", x))%>%
                        rename_all(function(x) gsub("Cancer7", "Head and neck", x))%>%
                        rename_all(function(x) gsub("Cancer8", "Stomach", x))%>%
                        rename_all(function(x) gsub("Cancer9", "Ovarian", x))%>%
                        rename_all(function(x) gsub("Stage1", ": I", x)) %>%
                        rename_all(function(x) gsub("Stage2", ": II", x)) %>%
                        rename_all(function(x) gsub("Stage3", ": III", x)) %>%
                        rename_all(function(x) gsub("Stage4", ": IV", x))

        }) %>%
        bind_rows(.id = "id")%>%
        filter(`Age at diagnosis` < 80) %>%
        select(-No_Cancer, -Deaths) %>%
        pivot_wider(
                names_from = id,
                names_glue = "{id}{.value}",
                values_from = `Lung: I`:`Urothelial tract: IV`
        )

#' Adjust out the cases in which the modifier still applies

df_adjustment <- as.data.frame(df_aggregate_absoluteprop_sf[1:30, 3:78]) %>%
        mutate(across(everything(), ~ case_when(. > 1 ~ 0,
                                                .default = 1))) %>%
        select(-X47,-X48,-X49,-X50, -`X73`, -`X74`, -`X75`, -`X76`)

res_number_no_longer1.2 <- l_all_results %>%
        select(starts_with("From stage with 1.2 modifier")) * df_adjustment

res_number_no_longer1.7 <- l_all_results %>%
        select(starts_with("From stage with 1.7 modifier")) * df_adjustment

 res_nshifted_bycancer <- l_all_results %>%
         select(`Age at diagnosis`, starts_with("Number shifted:")) %>%
         pivot_longer(`Number shifted: Lung: I`:`Number shifted: Urothelial tract: IV`, names_to = "names", values_to = "value") %>%
         separate_wider_delim(names, ":", names = c("temp", "Cancer type", "Stage")) %>%
         select(-temp) %>%
         group_by(`Cancer type`) %>%
         summarise(`Total shifted` = round(sum(value), 1))


res_nshifted_from1.2_bycancer <- res_number_no_longer1.2 %>%
        pivot_longer(`From stage with 1.2 modifier: Lung: I`:`From stage with 1.2 modifier: Urothelial tract: IV`, names_to = "names", values_to = "value") %>%
        separate_wider_delim(names, ":", names = c("temp", "Cancer type", "Stage")) %>%
        select(-temp) %>%
        group_by(`Cancer type`) %>%
        summarise(`Total shifted from 1.2` = round(sum(value), 1))


res_nshifted_from1.7_bycancer <- res_number_no_longer1.7 %>%
        pivot_longer(`From stage with 1.7 modifier: Lung: I`:`From stage with 1.7 modifier: Urothelial tract: IV`, names_to = "names", values_to = "value") %>%
        separate_wider_delim(names, ":", names = c("temp", "Cancer type", "Stage")) %>%
        select(-temp) %>%
        group_by(`Cancer type`) %>%
        summarise(`Total shifted from 1.7` = round(sum(value), 1))

res_nshifted_combined <- res_nshifted_bycancer %>%
        left_join(res_nshifted_from1.2_bycancer, by = c("Cancer type")) %>%
        left_join(res_nshifted_from1.7_bycancer, by = c("Cancer type")) %>%
        add_row(`Cancer type` = " Total", summarise(., across(where(is.numeric), sum))) %>%
        mutate(`% shifted from 1.2` = round((`Total shifted from 1.2`/`Total shifted`) * 100 , 1)) %>%
        mutate(`% shifted from 1.7` = round((`Total shifted from 1.7`/`Total shifted`) * 100 , 1)) %>%
        mutate(`Cancer type` = str_sub(`Cancer type`, 2, -1)) %>%
        filter(`Cancer type` != "Urothelial tract" )

res_soc_markov_trace <- as_tibble(res_no_modifier$soc_markov_trace[1:30, 3:78])%>%
        select(-contains("Cancer12"), -contains("Cancer19")) %>%
        rename_all(function(x) gsub("Cancer10", "Kidney", x))%>%
        rename_all(function(x) gsub("Cancer11", "Prostate", x))%>%
        rename_all(function(x) gsub("Cancer13", "Lymphoma", x))%>%
        rename_all(function(x) gsub("Cancer14", "Anal", x))%>%
        rename_all(function(x) gsub("Cancer15", "Uterine", x))%>%
        rename_all(function(x) gsub("Cancer16", "Bladder", x))%>%
        rename_all(function(x) gsub("Cancer17", "Cervical", x))%>%
        rename_all(function(x) gsub("Cancer18", "Urothelial tract", x))%>%
        rename_all(function(x) gsub("Cancer1", "Lung", x)) %>%
        rename_all(function(x) gsub("Cancer2", "Colorectal", x)) %>%
        rename_all(function(x) gsub("Cancer3", "Pancreatic", x))%>%
        rename_all(function(x) gsub("Cancer4", "Liver/Bile-duct", x))%>%
        rename_all(function(x) gsub("Cancer5", "Breast", x))%>%
        rename_all(function(x) gsub("Cancer6", "Oesophageal", x))%>%
        rename_all(function(x) gsub("Cancer7", "Head and neck", x))%>%
        rename_all(function(x) gsub("Cancer8", "Stomach", x))%>%
        rename_all(function(x) gsub("Cancer9", "Ovarian", x))%>%
        rename_all(function(x) gsub("Stage1", ": I", x)) %>%
        rename_all(function(x) gsub("Stage2", ": II", x)) %>%
        rename_all(function(x) gsub("Stage3", ": III", x)) %>%
        rename_all(function(x) gsub("Stage4", ": IV", x))


df_adjustment_SoC <- as.data.frame(df_aggregate_absoluteprop_sf[1:30, 3:78]) %>%
        mutate(across(everything(), ~ case_when(. == 1 ~ 0,
                                                .default = 1))) %>%
        select(-X47,-X48,-X49,-X50, -`X73`, -`X74`, -`X75`, -`X76`)

res_n_soc_bytypestage <- res_soc_markov_trace * df_adjustment_SoC

res_n_soc_modifier <-  res_n_soc_bytypestage %>%
        pivot_longer(`Lung: I`:`Urothelial tract: IV`, names_to = "names", values_to = "value") %>%
        separate_wider_delim(names, ":", names = c( "Cancer type", "Stage")) %>%
        group_by(`Cancer type`) %>%
        summarise(`Number in modifier state under SoC` = round(sum(value), 1)) %>%
        add_row(`Cancer type` = " Total", summarise(., across(where(is.numeric), sum))) %>%




library(readxl, quietly = TRUE)
library(gt, quietly = TRUE)
library(openxlsx, quietly = TRUE)

excel_output <- createWorkbook()

addWorksheet(excel_output, sheetName = "1.Mod by c,s,a")
writeData(excel_output, sheet = "1.Mod by c,s,a", df_aggregate_absoluteprop_sf %>%
                  setNames(colnames(as_tibble(res_no_modifier$time_shift_markov_trace_TS))) %>%
                  select(-No_Cancer, -Deaths, -contains("Cancer12"), -contains("Cancer19")))

addWorksheet(excel_output, sheetName = "2.Shifted by c,s,a")
writeData(excel_output, sheet = "2.Shifted by c,s,a", l_all_results)

addWorksheet(excel_output, sheetName = "3.Shifted from 1.2 by c,s,a")
writeData(excel_output, sheet = "3.Shifted from 1.2 by c,s,a", res_nshifted_from1.2_bycancer)

addWorksheet(excel_output, sheetName = "4.Shifted from 1.7 by c,s,a")
writeData(excel_output, sheet = "4.Shifted from 1.7 by c,s,a", res_nshifted_from1.7_bycancer)

addWorksheet(excel_output, sheetName = "5.Shifted % from mod")
writeData(excel_output, sheet = "5.Shifted % from mod", res_nshifted_combined)


saveWorkbook(excel_output, paste0("~/GalleriCEmodel/analyses/NICE_modifier/","NICE_SoD_modifier_ISPOR2025.xlsx"), overwrite = TRUE)















res_modifier <- f_run_model(no_NICE_modifier = FALSE,
                            run_desc = "Modifier",
                            export_path = "~/GalleriCEmodel/analyses/NICE_modifier/export/",
                            data_dir = "~/GalleriCEmodel/data-raw/England_pilot_planning/",
                            return_M_trace = TRUE,
                            # From iSetting in original
                            model_start_age = 50,
                            soc_screen_min_age = 50,
                            soc_screen_max_age = 85,
                            mced_screen_min_age = 50,
                            mced_screen_max_age = 79,
                            time_horizon = 100,
                            perc_female = 0.53)

no_NICE_modifier_QALY_gained <- res_no_modifier$res_MCED_total_QALYs - res_no_modifier$res_soc_total_QALYs
NICE_modifier_QALY_gained <- res_modifier$res_MCED_total_QALYs - res_modifier$res_soc_total_QALYs
pecentage_gain = ((NICE_modifier_QALY_gained-no_NICE_modifier_QALY_gained)/no_NICE_modifier_QALY_gained) *100



