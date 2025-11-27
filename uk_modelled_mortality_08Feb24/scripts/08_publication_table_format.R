#present results in a table format for publication

# Main sequential progression results -------------------------------------
# Stage shift in the open cohort scenario (Table 1 ) ----------------------

w_SS_agg_main = w_SS_agg %>%
  filter(model_type=="main") %>%
  select(-model_type, -perc_skip) %>%
  mutate_if(is.numeric, round)  %>%
  mutate(scenario = case_when(scenario=="old_2" ~ "Slow", 
                              scenario=="old_3" ~ "Medium", 
                              scenario=="old_4" ~ "Fast", 
                              TRUE~scenario)) 
  
w_mort_agg_main = w_mort_agg %>%
  filter(model_type=="main") %>%
  filter(local_haz_ratio == 3) %>% #not displaying mort results, so not required 
  select(-model_type, -perc_skip, -local_haz_ratio) %>%
  mutate_if(is.numeric, round) %>%
  mutate(scenario = case_when(scenario=="old_2" ~ "Slow", 
                              scenario=="old_3" ~ "Medium", 
                              scenario=="old_4" ~ "Fast", 
                              TRUE~scenario))

w_mort_agg_main_HR = w_mort_agg %>% 
  filter(model_type=="main") %>%
  select(-model_type, -perc_skip) %>%
  mutate_if(is.numeric, round) %>%
  mutate(scenario = case_when(scenario=="old_2" ~ "Slow", 
                              scenario=="old_3" ~ "Medium", 
                              scenario=="old_4" ~ "Fast", 
                              TRUE~scenario))

#Table 1 - Open cohort stage shift prevalent and incident 
tab_1_data <- w_SS_agg_main %>%
  cbind(select(w_mort_agg_main, -scenario)) %>%
  mutate(all_care_detected_prev = screen_detected_prev + SoC_detected_prev) %>%
  mutate(all_care_detected = screen_detected + SoC_detected, scenario = row_number()) %>% 
  mutate(p_all_care = 100,
         p_SoC_detected = round(SoC_detected/all_care_detected *100,0),
         p_screen_detected = round(screen_detected/all_care_detected *100,0),
         p_reduction_late = round(shifted_early/(late_stage + shifted_early) *100,0),
         p_SoC_detected_prev = round(SoC_detected_prev/all_care_detected_prev *100,0),
         p_screen_detected_prev = round(screen_detected_prev/all_care_detected_prev *100,0),
         p_reduction_late_prev = round(shifted_early_prev/(late_stage_prev + shifted_early_prev) *100,0),
         p_initial_late = round(initial_late/all_care_detected *100,0),
         p_initial_late_prev = round(initial_late_prev/all_care_detected_prev *100,0),
         p_late_stage = round(late_stage/all_care_detected *100,0),
         p_late_stage_prev = round(late_stage_prev/all_care_detected_prev *100,0),
  ) %>% 
  transmute(all_care_detected = paste0(as.character(all_care_detected), " (",as.character(p_all_care),")"),
            SoC_detected = paste0(as.character(SoC_detected), " (",as.character(p_SoC_detected),")"),
            screen_detected = paste0(as.character(screen_detected), " (",as.character(p_screen_detected),")"),
            initial_late = paste0(as.character(initial_late),  " (",as.character(p_initial_late),")"), #late stage diagnosis without MCED screening
            late_stage = paste0(as.character(late_stage), " (",as.character(p_late_stage),")" ), #late stage diagnosis with MCED screening
            reduction_late = paste0(as.character(shifted_early), " (",as.character(p_reduction_late),")"),
            all_care_detected_prev = paste0(as.character(all_care_detected_prev), " (",as.character(p_all_care),")"),
            SoC_detected_prev = paste0(as.character(SoC_detected_prev), " (",as.character(p_SoC_detected_prev),")"),
            screen_detected_prev = paste0(as.character(screen_detected_prev), " (",as.character(p_screen_detected_prev),")"),
            initial_late_prev = paste0(as.character(initial_late_prev),  " (",as.character(p_initial_late_prev),")"),
            late_stage_prev = paste0(as.character(late_stage_prev), " (",as.character(p_late_stage_prev),")"  ), #late stage diagnosis with MCED screening
            reduction_late_prev = paste0(as.character(shifted_early_prev), " (",as.character(p_reduction_late_prev),")")
  )

#create df for null model to tab_1_data
null_model = data.frame (all_care_detected = tab_1_data$all_care_detected[1],
                        SoC_detected = tab_1_data$all_care_detected[1],
                        screen_detected = c("NA"),
                        initial_late = tab_1_data$initial_late[1],
                        late_stage = c("NA"),
                        reduction_late = c("NA"),
                        all_care_detected_prev = c("NA"),
                        SoC_detected_prev = c("NA"),
                        screen_detected_prev = c("NA"),
                        initial_late_prev = c("NA"),
                        late_stage_prev = c("NA"),
                        reduction_late_prev = c("NA"))

#append null model to main results
tab_1_data_nm = rbind(null_model, tab_1_data)

#reshape
tab_1_res <- tab_1_data_nm %>%
  mutate(scenario = row_number()) %>%
  pivot_longer(cols = 1:12,names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = c("value")) %>%  
  mutate(statistic = case_when(
    statistic == "all_care_detected_prev" | statistic == "all_care_detected" ~ "Found usual care and MCED (%)",
    statistic == "SoC_detected_prev" | statistic == "SoC_detected" ~ "Found usual care (%)",
    statistic == "screen_detected_prev" | statistic == "screen_detected" ~ "MCED detected (%)",
    statistic == "initial_late_prev" | statistic == "initial_late" ~ "Late stage diagnosis with usual care",
    statistic == "late_stage_prev" | statistic == "late_stage" ~ "Late stage diagnosis with MCED",
    statistic == "reduction_late_prev" | statistic == "reduction_late" ~ "Reduction in late stage diagnosis with MCED (%)"))

#write out excel file
write.xlsx(tab_1_res, sprintf("generated_data/%s_tab_1_res.xlsx", date_code))

#Use gt to display
tab_1_res %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "Late Stage Reduction in Open Cohort") %>%
  cols_label('1' = "No screening",
             '2' = "Slow",
             '3' = "Medium",
             '4' = "Fast") %>%
  tab_row_group(label = "Incidence Round", rows = 1:6,) %>%
  tab_row_group(label = "Prevalence Round", rows = 7:12) %>%
  row_group_order(c("Prevalence Round", "Incidence Round")) %>%
  gtsave(filename = sprintf("reports/%s_table_1.html",date_code))

# Mortality reduction in the open cohort scenario (Table 2) ---------------
tab_2_data <- w_mort_agg_main_HR %>%
  mutate(total_mort = round(mortality_no_screening + mortality_screening),
         total_mort_prev = round(mortality_no_screening_prev + mortality_screening_prev),
         mortality_difference_prev = round(mortality_no_screening_prev - mortality_screening_prev),
         p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0),
         p_mortality_difference_prev = round(mortality_difference_prev/mortality_no_screening_prev*100,0),
         p_mortality_screening = round(mortality_screening/total_mort  *100,0),
         p_mortality_no_screening = round(mortality_no_screening/total_mort  *100,0),
         p_mortality_screening_prev = round(mortality_screening_prev/total_mort_prev  *100,0),
         p_mortality_no_screening_prev = round(mortality_no_screening_prev/total_mort_prev  *100,0),
  ) %>%
  transmute(local_haz_ratio = paste0("HR = ",as.character(local_haz_ratio)),
            scenario = scenario,
            mortality_no_screening_prev = as.character(mortality_no_screening_prev), 
            mortality_screening_prev = as.character(mortality_screening_prev), 
            reduction_mortality_prev = paste0(as.character(mortality_difference_prev), " (",as.character(p_mortality_difference_prev),")"),
            mortality_no_screening = as.character(mortality_no_screening), 
            mortality_screening = as.character(mortality_screening), 
            reduction_mortality = paste0(as.character(mortality_difference), " (",as.character(p_mortality_difference),")"))

#reshape
tab_2_res <- tab_2_data %>%
  pivot_longer(cols = 3:8,names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = local_haz_ratio) %>%
  pivot_wider(names_from = scenario, values_from = c("HR = 1", "HR = 2", "HR = 3")) %>%
  mutate(statistic = case_when(
    statistic == "mortality_no_screening_prev" | statistic == "mortality_no_screening" ~ "Cancer Mortality Rate - usual care",
    statistic == "mortality_screening_prev" | statistic == "mortality_screening" ~ "Cancer  Mortality Rate - usual care and MCED",
    statistic == "reduction_mortality_prev" | statistic == "reduction_mortality" ~ "Cancer Mortality Rate reduction with MCED, (%)"))

#insert additional column for null model
tab_2_res_nm = tab_2_res %>%
  add_column(`No screening` = c("NA","NA","NA",tab_2_res$`HR = 1_Slow`[4],"NA","NA"))

#reorder column names for visualisation
tab_2_res_nm = tab_2_res_nm[, c(1,11,2,3,4,5,6,7,8,9,10)]

#write out excel file
write.xlsx(tab_2_res, sprintf("generated_data/%s_tab_2_res_nm.xlsx", date_code))

#Use gt to display
tab_2_res_nm %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "Mortality in Open Cohort") %>%
  cols_label(`No screening` = "No screening",
             `HR = 1_Slow` = "Slow",
             `HR = 1_Medium` = "Medium",
             `HR = 1_Fast` = "Fast",
             `HR = 2_Slow` = "Slow",
             `HR = 2_Medium` = "Medium",
             `HR = 2_Fast` = "Fast",
             `HR = 3_Slow` = "Slow",
             `HR = 3_Medium` = "Medium",
             `HR = 3_Fast` = "Fast") %>%
  tab_spanner(label = "Hazard Ratio = 1",columns = c(3:5)) %>%
  tab_spanner(label = "Hazard Ratio = 2",columns = c(6:8)) %>%
  tab_spanner(label = "Hazard Ratio = 3",columns = c(9:11)) %>%
  tab_row_group(label = "Prevalence Round", rows = 1:3,) %>%
  tab_row_group(label = "Incidence Round", rows = 4:6) %>%
  row_group_order(c("Prevalence Round", "Incidence Round"))%>%
  gtsave(filename = sprintf("reports/%s_table_2.html",date_code))

# SS + MR by cancer type (Table 3C) ---------------------------------------

#filter down to relevant dfs
w_mort_by_cancer_main = w_mort_by_cancer %>%
  filter(model_type=="main") %>% #only presenting main model, HR=3 results, to avoid table getting too unwieldy. 
  filter(local_haz_ratio==3) %>%
  filter(scenario=="old_3") %>%
  filter(NCRAS_Draw!="[OTHER]") %>% #not presented
  mutate_if(is.numeric, round) %>%
  select(-model_type, -perc_skip, -local_haz_ratio, -scenario)

w_SS_by_cancer_main = w_SS_by_cancer %>%
  filter(model_type=="main") %>%
  filter(scenario=="old_3") %>%
  filter(NCRAS_Draw!="[OTHER]") %>%
  mutate_if(is.numeric, round) %>%
  select(-model_type, -perc_skip, -scenario)

tab_3_data <- w_SS_by_cancer_main %>%
  left_join(w_mort_by_cancer_main, by=c("NCRAS_Draw")) %>%
  mutate(NCRAS_Draw = case_when(NCRAS_Draw == "Esophagus" ~ "Oesophagus", #Get corrected order, e.g. esophagus -> oesophagus
                                TRUE~NCRAS_Draw)) %>%
  select(-ends_with("_prev")) %>%
  arrange(desc(mortality_difference)) %>%
  mutate(cur_IR = screen_detected + SoC_detected, 
         p_late_reduction = round(shifted_early/initial_late *100,0),
         p_mort_reduction = round(mortality_difference/mortality_no_screening  *100,0),
  ) %>% 
  mutate(shifted_early = paste0(as.character(shifted_early), " (",as.character(p_late_reduction),")"),
         mortality_difference = paste0(as.character(mortality_difference), " (",as.character(p_mort_reduction),")"),
  ) %>%
  select(NCRAS_Draw, cur_IR, initial_late, late_stage, shifted_early, mortality_no_screening, mortality_screening, mortality_difference) 

#write out excel file
write.xlsx(tab_3_data, sprintf("generated_data/%s_tab_3_data.xlsx", date_code))

#Use gt to display
tab_3_data %>%
  gt(rowname_col = "NCRAS_Draw") %>%
  tab_header(title = "Stage Shift and Mortality Reduction by Cancer Type") %>%
  cols_label('cur_IR' = "Current Incidence Rate",
             'initial_late' = "Diagnosed late usual care", 
             'late_stage' = "Diagnosed late usual care and MCED",
             'shifted_early' = "Late stage reduction with MCED, (%)",
             'mortality_no_screening' = "Cancer mortality rate usual care", 
             'mortality_screening' = "Cancer mortality rate usual care and MCED", 
             'mortality_difference' = "Cancer mortality reduction with MCED, (%)") %>%
  gtsave(filename = sprintf("reports/%s_table_3.html",date_code))


# SS + MR in national screening programme (Table 4) -----------------------

#basic performance and stage shift part of table. 
#filter to the relevant dfs
np_SURV_res_main = np_SURV_res %>%
  filter(model_type=="main") %>%
  select(-model_type, -perc_skip) %>%
  mutate_if(is.numeric, round) %>%
  mutate(scenario = case_when(scenario=="old_2" ~ "Slow", 
                              scenario=="old_3" ~ "Medium", 
                              scenario=="old_4" ~ "Fast", 
                              TRUE~scenario))

np_SS_res_main = np_SS_res %>%
  filter(model_type=="main") %>%
  select(-model_type, -perc_skip) %>%
  mutate_if(is.numeric, round) %>%
  mutate(scenario = case_when(scenario=="old_2" ~ "Slow", 
                              scenario=="old_3" ~ "Medium", 
                              scenario=="old_4" ~ "Fast", 
                              TRUE~scenario))

tab_4_bp_SS = np_SURV_res_main %>% #basic performance and stage shift
  filter(local_haz_ratio == 3) %>% #HR does not affect these results
  mutate(p_screen_detected = round(screen_detected/(screen_detected + SoC_detected) *100,0),
         p_SoC_detected = round(SoC_detected/(screen_detected + SoC_detected) *100,0),
         "Found usual care, (%)" = paste0(as.character(SoC_detected), " (",as.character(p_SoC_detected),")"),
         "Found MCED, (%)" = paste0(as.character(screen_detected), " (",as.character(p_screen_detected),")")) %>%
  left_join(np_SS_res_main, by="scenario") %>%  
  mutate(p_early_stage = round(early_stage/(late_stage + early_stage) *100,0),
         p_late_stage = round(late_stage/(late_stage + early_stage) *100,0),
         p_shifted_early = round(shifted_early/(shifted_early + late_stage)*100,0),
         p_shifted_any = round(shifted_any/(early_stage + late_stage)*100,0)) %>%
  mutate(early_stage = paste0(as.character(early_stage), " (",as.character(p_early_stage),")"), #not reported in table
         "Diagnosed late, Usual care and MCED, (%)" = paste0(as.character(late_stage), " (",as.character(p_late_stage),")"), #late_stage
         "Reduction in late diagnosis with MCED, (%)" = paste0(as.character(shifted_early), " (",as.character(p_shifted_early),")"), #shifted_early
         shifted_any = paste0(as.character(shifted_any), " (",as.character(p_shifted_any),")")) %>% #not reported in table
  select(scenario, "Found usual care, (%)", "Found MCED, (%)", "Diagnosed late, Usual care and MCED, (%)", "Reduction in late diagnosis with MCED, (%)") 

tab_4_bp_SS <- tab_4_bp_SS %>%
  pivot_longer(cols = c("Found usual care, (%)", "Found MCED, (%)", "Diagnosed late, Usual care and MCED, (%)", "Reduction in late diagnosis with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = c("value"))

#Use gt to display
tab_4_bp_SS %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "Late Stage Reduction") %>%
  gtsave(filename = sprintf("reports/%s_table_4_bp_SS.html",date_code))

#mort part of table
tab_4_mort = np_SURV_res_main %>%
  mutate(p_mortality_screening = round(mortality_screening/(screen_detected + SoC_detected) *100,0),
         p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>%
  mutate(p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>% #% of no screening mortality
  mutate("Cancer Mortality, Usual care, (%)" = paste0(as.character(mortality_screening)," (",as.character(p_mortality_screening),")"),
         "Reduction cancer mortality with MCED, (%)" = paste0(as.character(mortality_difference)," (",as.character(p_mortality_difference),")")) %>%
  select(scenario, local_haz_ratio, "Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)")

#reshape
tab_4_mort <- tab_4_mort %>%
  pivot_longer(cols = c("Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = c("value"))

#write out excel file
write.xlsx(tab_4_mort, sprintf("generated_data/%s_tab_4_mort.xlsx", date_code))

#Use gt to display
tab_4_mort %>%
  gt(rowname_col = "statistic", groupname_col = "local_haz_ratio")%>%
  tab_header(title = "Cancer Mortality in 5 years") %>%
  tab_spanner(label = "Dwell Time",columns = c(1,2,3)) %>%
  tab_stubhead(label = "Hazard Ratio") %>%
  gtsave(filename = sprintf("reports/%s_table_4_mort.html",date_code))

# Supplementary: non seq results ------------------------------------------
# Nonseq, NSP, PH, med dwell,  HR=3, (Table S5) ----------------------------

#filter to relevant df
np_SS_PH = np_SS_res %>%
  filter(model_type=="PH") %>%
  filter(scenario == "old_3") 

np_SURV_PH = np_SURV_res %>%
  filter(model_type=="PH") %>%
  filter(scenario == "old_3") %>%
  filter(local_haz_ratio == "3") 

tab_s5_PH <- np_SURV_PH %>% #basic performance and stage shift
  mutate(p_screen_detected = round(screen_detected/(screen_detected + SoC_detected) *100,0),
         p_SoC_detected = round(SoC_detected/(screen_detected + SoC_detected) *100,0),
         "Found usual care, (%)" = paste0(as.character(round(SoC_detected)), " (",as.character(p_SoC_detected),")"),
         "Found MCED, (%)" = paste0(as.character(round(screen_detected)), " (",as.character(p_screen_detected),")")) %>%
  left_join(np_SS_PH, by=c("perc_skip")) %>%  
  mutate(p_late_stage = round(late_stage/(late_stage + early_stage) *100,0),
         p_shifted_early = round(shifted_early/(shifted_early + late_stage)*100,0)) %>%
  mutate("Diagnosed late, Usual care and MCED, (%)" = paste0(as.character(round(late_stage)), " (",as.character(p_late_stage),")"),
         "Reduction in late diagnosis with MCED, (%)" = paste0(as.character(round(shifted_early)), " (",as.character(p_shifted_early),")")) %>% 
  select(perc_skip, "Found usual care, (%)", "Found MCED, (%)", "Diagnosed late, Usual care and MCED, (%)", "Reduction in late diagnosis with MCED, (%)") 

#reshape
tab_s5_PH <- tab_s5_PH %>%
  pivot_longer(cols = c("Found usual care, (%)", "Found MCED, (%)","Diagnosed late, Usual care and MCED, (%)","Reduction in late diagnosis with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = perc_skip, values_from = c("value"))

#write out excel file
write.xlsx(tab_s5_PH, sprintf("generated_data/%s_tab_s5_PH.xlsx", date_code))

#Use gt to display
tab_s5_PH %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "Short Stage I only: Basic Performance and Late Stage Reduction") %>%
  tab_stubhead(label = "Proportion with non-sequential progression") %>%
  gtsave(filename = sprintf("reports/%s_table_s5_PH.html",date_code))

#mort part of table
tab_s5_mort = np_SURV_PH %>%
  select(-local_haz_ratio, -scenario) %>%
  mutate(p_mortality_screening = round(mortality_screening/(screen_detected + SoC_detected) *100,0),
         p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>%
  mutate(p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>% #% of no screening mortality
  mutate("Cancer Mortality, Usual care, (%)" = paste0(as.character(round(mortality_screening))," (",as.character(p_mortality_screening),")"),
         "Reduction cancer mortality with MCED, (%)" = paste0(as.character(round(mortality_difference))," (",as.character(p_mortality_difference),")")) %>%
  select(perc_skip, "Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)")

#reshape
tab_s5_mort <- tab_s5_mort %>%
  pivot_longer(cols = c("Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = perc_skip, values_from = c("value"))

#write out excel file
write.xlsx(tab_s5_mort, sprintf("generated_data/%s_tab_s5_mort.xlsx", date_code))

#Use gt to display
tab_s5_mort %>%
  gt(rowname_col = "statistic") %>%
  tab_header(title = "Short Stage I only: Cancer Mortality in 5 years") %>%
  tab_spanner(label = "Proportion with non-sequential progression",columns = c(1,2,3)) %>%
  gtsave(filename = sprintf("reports/%s_table_s5_mort.html", date_code))

# Nonseq,  NSP, NPH, med dwell, HR=3 (Table S6) ---------------------------

#filter to relevant df
np_SS_NPH = np_SS_res %>%
  filter(model_type=="NPH") %>%
  filter(scenario == "old_3") 

np_SURV_NPH = np_SURV_res %>%
  filter(model_type=="NPH") %>%
  filter(scenario == "old_3") %>%
  filter(local_haz_ratio == "3") 

tab_s6_NPH <- np_SURV_NPH %>% #basic performance and stage shift
  mutate(p_screen_detected = round(screen_detected/(screen_detected + SoC_detected) *100,0),
         p_SoC_detected = round(SoC_detected/(screen_detected + SoC_detected) *100,0),
         "Found usual care, (%)" = paste0(as.character(round(SoC_detected)), " (",as.character(p_SoC_detected),")"),
         "Found MCED, (%)" = paste0(as.character(round(screen_detected)), " (",as.character(p_screen_detected),")")) %>%
  left_join(np_SS_NPH, by=c("perc_skip")) %>%  
  mutate(p_late_stage = round(late_stage/(late_stage + early_stage) *100,0),
         p_shifted_early = round(shifted_early/(shifted_early + late_stage)*100,0)) %>%
  mutate("Diagnosed late, Usual care and MCED, (%)" = paste0(as.character(round(late_stage)), " (",as.character(p_late_stage),")"),
         "Reduction in late diagnosis with MCED, (%)" = paste0(as.character(round(shifted_early)), " (",as.character(p_shifted_early),")")) %>% 
  select(perc_skip, "Found usual care, (%)", "Found MCED, (%)", "Diagnosed late, Usual care and MCED, (%)", "Reduction in late diagnosis with MCED, (%)") 

#reshape
tab_s6_NPH <- tab_s6_NPH %>%
  pivot_longer(cols = c("Found usual care, (%)", "Found MCED, (%)","Diagnosed late, Usual care and MCED, (%)","Reduction in late diagnosis with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = perc_skip, values_from = c("value"))

#write out excel file
write.xlsx(tab_s6_NPH, sprintf("generated_data/%s_tab_s6_NPH.xlsx", date_code))

#Use gt to display
tab_s6_NPH %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "No stage prior to Stage IV: Basic Performance and Late Stage Reduction") %>%
  tab_stubhead(label = "Proportion with non-sequential progression") %>%
  gtsave(filename = sprintf("reports/%s_table_s6_NPH.html",date_code))

#mort part of table
tab_s6_mort = np_SURV_NPH %>%
  select(-local_haz_ratio, -scenario) %>%
  mutate(p_mortality_screening = round(mortality_screening/(screen_detected + SoC_detected) *100,0),
         p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>%
  mutate(p_mortality_difference = round(mortality_difference/mortality_no_screening*100,0)) %>% #% of no screening mortality
  mutate("Cancer Mortality, Usual care, (%)" = paste0(as.character(round(mortality_screening))," (",as.character(p_mortality_screening),")"),
         "Reduction cancer mortality with MCED, (%)" = paste0(as.character(round(mortality_difference))," (",as.character(p_mortality_difference),")")) %>%
  select(perc_skip, "Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)")

#reshape
tab_s6_mort <- tab_s6_mort %>%
  pivot_longer(cols = c("Cancer Mortality, Usual care, (%)", "Reduction cancer mortality with MCED, (%)"),names_to = "statistic", values_to = "value") %>%
  pivot_wider(names_from = perc_skip, values_from = c("value"))

#write out excel file
write.xlsx(tab_s6_mort, sprintf("generated_data/%s_tab_s6_mort.xlsx", date_code))

#Use gt to display
tab_s6_mort %>%
  gt(rowname_col = "statistic")%>%
  tab_header(title = "No stage prior to Stage IV: Cancer Mortality in 5 years") %>%
  tab_spanner(label = "Proportion with non-sequential progression",columns = c(1,2,3)) %>%
  gtsave(filename = sprintf("reports/%s_table_s6_mort.html", date_code))