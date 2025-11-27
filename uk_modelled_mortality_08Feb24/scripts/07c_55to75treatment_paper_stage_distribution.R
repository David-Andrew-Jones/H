#Stage distributions after introduction of screening
#For treatment impact manuscript

#All analysis will use base case assumption of fast (aggfast/old_4) dwell times


################################################################################
#____ Scenario 1 - 100% uptake

#Long-run steady state scenario, screening from age 50 to age 79 annual
#Base case 100% uptake

#We want to extract absolute and proportional change in incidence by stage for each cancer type following the singe age cohort scenario

# Stage distribtion -----------------------------------------------------
# Get incidence by stage when screening is applied and when it is not applied
#prequel is stage diagnosed with MCED screening
#clinical is stage diagnosed without MCED screening
#repeated for prevalence round, no suffix = incidence round (1 year interval)
stage_dist_by_cancer <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>%
  summarize(
    end_stage_1 = sum(end_stage_1),#incidence rounds
    end_stage_2 = sum(end_stage_2),
    end_stage_3 = sum(end_stage_3),
    end_stage_4 = sum(end_stage_4),
    end_stage_1_prev = sum(end_stage_1_prev),#prevalence rounds
    end_stage_2_prev = sum(end_stage_2_prev),
    end_stage_3_prev = sum(end_stage_3_prev),
    end_stage_4_prev = sum(end_stage_4_prev),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer <- stage_dist_by_cancer %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer <- weighted_stage_dist_by_cancer %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
    ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 55 + sum of all the incidence rounds from 56

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no55 = weighted_stage_dist_by_cancer %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 55 ~ 0, 
                                    TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 55 ~ 0, 
                                            TRUE ~ weighted_end_stage_4))
  


# SS_GROUP_BY_CANCER = c("NCRAS_Draw","scenario", "perc_skip","model_type") 

np_SD_res <-  weighted_sd_no55 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base <- np_SD_res %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_50reducedsens_55to79.xlsx", date_code))

################################################################################
#____ Scenario 2 - 90% uptake

#With less than 100% coverage and/or uptake - assuming randomly distributed wrt risk

#Proportion that recieve screening
uptake_prop <- 0.9

stage_dist_by_cancer_uptake <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>% #ADJUSTED FOR UPTAKE
  summarize(
    end_stage_1 = (sum(end_stage_1) * uptake_prop) + (sum(start_stage_1) *(1-uptake_prop)),#incidence rounds
    end_stage_2 = (sum(end_stage_2) * uptake_prop) + (sum(start_stage_2) *(1-uptake_prop)),
    end_stage_3 = (sum(end_stage_3) * uptake_prop) + (sum(start_stage_3) *(1-uptake_prop)),
    end_stage_4 = (sum(end_stage_4) * uptake_prop) + (sum(start_stage_4) *(1-uptake_prop)),
    end_stage_1_prev = (sum(end_stage_1_prev) * uptake_prop) + (sum(start_stage_1_prev) * (1-uptake_prop)),#prevalence rounds
    end_stage_2_prev = (sum(end_stage_2_prev) * uptake_prop) + (sum(start_stage_2_prev) * (1-uptake_prop)),
    end_stage_3_prev = (sum(end_stage_3_prev) * uptake_prop) + (sum(start_stage_3_prev) * (1-uptake_prop)),
    end_stage_4_prev = (sum(end_stage_4_prev) * uptake_prop) + (sum(start_stage_4_prev) * (1-uptake_prop)),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer_90<- stage_dist_by_cancer_uptake %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer_90 <- weighted_stage_dist_by_cancer_90 %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
  ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no50_90 = weighted_stage_dist_by_cancer_90 %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_4))


np_SD_res_90 <-  weighted_sd_no50_90 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base_90 <- np_SD_res_90 %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base_90, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_90.xlsx", date_code))

################################################################################
#____ Scenario 3 - 80% uptake

#With less than 100% coverage and/or uptake - assuming randomly distributed wrt risk

#Proportion that recieve screening
uptake_prop <- 0.8

stage_dist_by_cancer_uptake <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>% #ADJUSTED FOR UPTAKE
  summarize(
    end_stage_1 = (sum(end_stage_1) * uptake_prop) + (sum(start_stage_1) *(1-uptake_prop)),#incidence rounds
    end_stage_2 = (sum(end_stage_2) * uptake_prop) + (sum(start_stage_2) *(1-uptake_prop)),
    end_stage_3 = (sum(end_stage_3) * uptake_prop) + (sum(start_stage_3) *(1-uptake_prop)),
    end_stage_4 = (sum(end_stage_4) * uptake_prop) + (sum(start_stage_4) *(1-uptake_prop)),
    end_stage_1_prev = (sum(end_stage_1_prev) * uptake_prop) + (sum(start_stage_1_prev) * (1-uptake_prop)),#prevalence rounds
    end_stage_2_prev = (sum(end_stage_2_prev) * uptake_prop) + (sum(start_stage_2_prev) * (1-uptake_prop)),
    end_stage_3_prev = (sum(end_stage_3_prev) * uptake_prop) + (sum(start_stage_3_prev) * (1-uptake_prop)),
    end_stage_4_prev = (sum(end_stage_4_prev) * uptake_prop) + (sum(start_stage_4_prev) * (1-uptake_prop)),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer_80<- stage_dist_by_cancer_uptake %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer_80 <- weighted_stage_dist_by_cancer_80 %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
  ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no50_80 = weighted_stage_dist_by_cancer_80 %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_4))


np_SD_res_80 <-  weighted_sd_no50_80 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base_80 <- np_SD_res_80 %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base_80, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_80.xlsx", date_code))

################################################################################
#____ Scenario 4 - 70% uptake

#With less than 100% coverage and/or uptake - assuming randomly distributed wrt risk

#Proportion that recieve screening
uptake_prop <- 0.7

stage_dist_by_cancer_uptake <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>% #ADJUSTED FOR UPTAKE
  summarize(
    end_stage_1 = (sum(end_stage_1) * uptake_prop) + (sum(start_stage_1) *(1-uptake_prop)),#incidence rounds
    end_stage_2 = (sum(end_stage_2) * uptake_prop) + (sum(start_stage_2) *(1-uptake_prop)),
    end_stage_3 = (sum(end_stage_3) * uptake_prop) + (sum(start_stage_3) *(1-uptake_prop)),
    end_stage_4 = (sum(end_stage_4) * uptake_prop) + (sum(start_stage_4) *(1-uptake_prop)),
    end_stage_1_prev = (sum(end_stage_1_prev) * uptake_prop) + (sum(start_stage_1_prev) * (1-uptake_prop)),#prevalence rounds
    end_stage_2_prev = (sum(end_stage_2_prev) * uptake_prop) + (sum(start_stage_2_prev) * (1-uptake_prop)),
    end_stage_3_prev = (sum(end_stage_3_prev) * uptake_prop) + (sum(start_stage_3_prev) * (1-uptake_prop)),
    end_stage_4_prev = (sum(end_stage_4_prev) * uptake_prop) + (sum(start_stage_4_prev) * (1-uptake_prop)),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer_70<- stage_dist_by_cancer_uptake %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer_70 <- weighted_stage_dist_by_cancer_70 %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
  ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no50_70 = weighted_stage_dist_by_cancer_70 %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_4))


np_SD_res_70 <-  weighted_sd_no50_70 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base_70 <- np_SD_res_70 %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base_70, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_70.xlsx", date_code))
################################################################################
#____ Scenario 5 - 60% uptake

#With less than 100% coverage and/or uptake - assuming randomly distributed wrt risk

#Proportion that recieve screening
uptake_prop <- 0.6

stage_dist_by_cancer_uptake <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>% #ADJUSTED FOR UPTAKE
  summarize(
    end_stage_1 = (sum(end_stage_1) * uptake_prop) + (sum(start_stage_1) *(1-uptake_prop)),#incidence rounds
    end_stage_2 = (sum(end_stage_2) * uptake_prop) + (sum(start_stage_2) *(1-uptake_prop)),
    end_stage_3 = (sum(end_stage_3) * uptake_prop) + (sum(start_stage_3) *(1-uptake_prop)),
    end_stage_4 = (sum(end_stage_4) * uptake_prop) + (sum(start_stage_4) *(1-uptake_prop)),
    end_stage_1_prev = (sum(end_stage_1_prev) * uptake_prop) + (sum(start_stage_1_prev) * (1-uptake_prop)),#prevalence rounds
    end_stage_2_prev = (sum(end_stage_2_prev) * uptake_prop) + (sum(start_stage_2_prev) * (1-uptake_prop)),
    end_stage_3_prev = (sum(end_stage_3_prev) * uptake_prop) + (sum(start_stage_3_prev) * (1-uptake_prop)),
    end_stage_4_prev = (sum(end_stage_4_prev) * uptake_prop) + (sum(start_stage_4_prev) * (1-uptake_prop)),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer_60<- stage_dist_by_cancer_uptake %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer_60 <- weighted_stage_dist_by_cancer_60 %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
  ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no50_60 = weighted_stage_dist_by_cancer_60 %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_4))


np_SD_res_60 <-  weighted_sd_no50_60 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base_60 <- np_SD_res_60 %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base_60, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_60.xlsx", date_code))
################################################################################
#____ Scenario 6 - 50% uptake

#With less than 100% coverage and/or uptake - assuming randomly distributed wrt risk

#Proportion that recieve screening
uptake_prop <- 0.5

stage_dist_by_cancer_uptake <- interception_stage_shift_ALL_w %>%
  filter(clinical < 5 & prequel < 5) %>% #Remove those not staged
  mutate(
    end_stage_1 = ifelse(prequel == 1, 1, 0) * caught,#incidence rounds
    end_stage_2 = ifelse(prequel == 2, 1, 0) * caught,
    end_stage_3 = ifelse(prequel == 3, 1, 0) * caught,
    end_stage_4 = ifelse(prequel == 4, 1, 0) * caught,
    end_stage_1_prev = ifelse(prequel == 1, 1, 0) * prevalence_caught,#prevalence rounds
    end_stage_2_prev = ifelse(prequel == 2, 1, 0) * prevalence_caught,
    end_stage_3_prev = ifelse(prequel == 3, 1, 0) * prevalence_caught,
    end_stage_4_prev = ifelse(prequel == 4, 1, 0) * prevalence_caught,
    start_stage_1 = ifelse(clinical == 1, 1, 0) * caught,#incidence rounds
    start_stage_2 = ifelse(clinical == 2, 1, 0) * caught,
    start_stage_3 = ifelse(clinical == 3, 1, 0) * caught,
    start_stage_4 = ifelse(clinical == 4, 1, 0) * caught,
    start_stage_1_prev = ifelse(clinical == 1, 1, 0) * prevalence_caught,#prevalence rounds
    start_stage_2_prev = ifelse(clinical == 2, 1, 0) * prevalence_caught,
    start_stage_3_prev = ifelse(clinical == 3, 1, 0) * prevalence_caught,
    start_stage_4_prev = ifelse(clinical == 4, 1, 0) * prevalence_caught
  ) %>%
  group_by_at(SS_GROUP_AGE_BY_CANCER) %>% #ADJUSTED FOR UPTAKE
  summarize(
    end_stage_1 = (sum(end_stage_1) * uptake_prop) + (sum(start_stage_1) *(1-uptake_prop)),#incidence rounds
    end_stage_2 = (sum(end_stage_2) * uptake_prop) + (sum(start_stage_2) *(1-uptake_prop)),
    end_stage_3 = (sum(end_stage_3) * uptake_prop) + (sum(start_stage_3) *(1-uptake_prop)),
    end_stage_4 = (sum(end_stage_4) * uptake_prop) + (sum(start_stage_4) *(1-uptake_prop)),
    end_stage_1_prev = (sum(end_stage_1_prev) * uptake_prop) + (sum(start_stage_1_prev) * (1-uptake_prop)),#prevalence rounds
    end_stage_2_prev = (sum(end_stage_2_prev) * uptake_prop) + (sum(start_stage_2_prev) * (1-uptake_prop)),
    end_stage_3_prev = (sum(end_stage_3_prev) * uptake_prop) + (sum(start_stage_3_prev) * (1-uptake_prop)),
    end_stage_4_prev = (sum(end_stage_4_prev) * uptake_prop) + (sum(start_stage_4_prev) * (1-uptake_prop)),
    start_stage_1 = sum(start_stage_1),#incidence rounds
    start_stage_2 = sum(start_stage_2),
    start_stage_3 = sum(start_stage_3),
    start_stage_4 = sum(start_stage_4),
    start_stage_1_prev = sum(start_stage_1_prev),#prevalence rounds
    start_stage_2_prev = sum(start_stage_2_prev),
    start_stage_3_prev = sum(start_stage_3_prev),
    start_stage_4_prev = sum(start_stage_4_prev)
  ) %>% #proportional change for each stage
  ungroup() %>%
  mutate(
    stage_1_change = end_stage_1/start_stage_1,
    stage_2_change = end_stage_2/start_stage_2,
    stage_3_change = end_stage_3/start_stage_3,
    stage_4_change = end_stage_4/start_stage_4,
    stage_1_change_prev = end_stage_1_prev/start_stage_1_prev,
    stage_2_change_prev = end_stage_2_prev/start_stage_2_prev,
    stage_3_change_prev = end_stage_3_prev/start_stage_3_prev,
    stage_4_change_prev = end_stage_4_prev/start_stage_4_prev
  )


# Apply weights to stage distribution results ------------------------------------
#Weights as in MM, account for attrition in steady state programme with invitations starting at 50
#join weights to stage shift, by cancer
weighted_stage_dist_by_cancer_50<- stage_dist_by_cancer_uptake %>%
  left_join(weight_screened_by_scenario, by = c("tAge","scenario","perc_skip","model_type"))  %>%
  mutate(
    weighted_end_stage_1 = end_stage_1 * weight,
    weighted_end_stage_2 = end_stage_2 * weight,
    weighted_end_stage_3 = end_stage_3 * weight,
    weighted_end_stage_4 = end_stage_4 * weight,
    weighted_end_stage_1_prev = end_stage_1_prev * weight,
    weighted_end_stage_2_prev = end_stage_2_prev * weight,
    weighted_end_stage_3_prev = end_stage_3_prev * weight,
    weighted_end_stage_4_prev = end_stage_4_prev * weight,
    weighted_start_stage_1 = start_stage_1 * weight,
    weighted_start_stage_2 = start_stage_2 * weight,
    weighted_start_stage_3 = start_stage_3 * weight,
    weighted_start_stage_4 = start_stage_4 * weight,
    weighted_start_stage_1_prev = start_stage_1_prev * weight,
    weighted_start_stage_2_prev = start_stage_2_prev * weight,
    weighted_start_stage_3_prev = start_stage_3_prev * weight,
    weighted_start_stage_4_prev = start_stage_4_prev * weight,
  ) 

#aggregate across ages - age-weighted
w_SD_by_cancer_50 <- weighted_stage_dist_by_cancer_50 %>%
  group_by_at(SS_GROUP_BY_CANCER) %>%
  summarize(
    weighted_start_stage_1 = sum(weighted_start_stage_1)/sum(weight),
    weighted_start_stage_2 = sum(weighted_start_stage_2)/sum(weight),
    weighted_start_stage_3 = sum(weighted_start_stage_3)/sum(weight),
    weighted_start_stage_4 = sum(weighted_start_stage_4)/sum(weight),
    weighted_end_stage_1 = sum(weighted_end_stage_1)/sum(weight),
    weighted_end_stage_2 = sum(weighted_end_stage_2)/sum(weight),
    weighted_end_stage_3 = sum(weighted_end_stage_3)/sum(weight),
    weighted_end_stage_4 = sum(weighted_end_stage_4)/sum(weight),
    weighted_start_stage_1_prev = sum(weighted_start_stage_1_prev)/sum(weight),
    weighted_start_stage_2_prev = sum(weighted_start_stage_2_prev)/sum(weight),
    weighted_start_stage_3_prev = sum(weighted_start_stage_3_prev)/sum(weight),
    weighted_start_stage_4_prev = sum(weighted_start_stage_4_prev)/sum(weight),
    weighted_end_stage_1_prev = sum(weighted_end_stage_1_prev)/sum(weight),
    weighted_end_stage_2_prev = sum(weighted_end_stage_2_prev)/sum(weight),
    weighted_end_stage_3_prev = sum(weighted_end_stage_3_prev)/sum(weight),
    weighted_end_stage_4_prev = sum(weighted_end_stage_4_prev)/sum(weight)
  ) %>%
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
    weighted_stage_1_change_prev = weighted_end_stage_1_prev/weighted_start_stage_1_prev,
    weighted_stage_2_change_prev = weighted_end_stage_2_prev/weighted_start_stage_2_prev,
    weighted_stage_3_change_prev = weighted_end_stage_3_prev/weighted_start_stage_3_prev,
    weighted_stage_4_change_prev = weighted_end_stage_4_prev/weighted_start_stage_4_prev
  )


#birth cohort results - stage distribution
#prevalence round at age 50 + sum of all the incidence rounds from 51

#remove the incidence round at age 50, so as not to double count it
weighted_sd_no50_50 = weighted_stage_dist_by_cancer_50 %>%
  mutate(weighted_start_stage_1 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_1)) %>%
  mutate(weighted_start_stage_2 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_2)) %>%
  mutate(weighted_start_stage_3 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_3)) %>%
  mutate(weighted_start_stage_4 = case_when(tAge == 50 ~ 0, 
                                            TRUE ~ weighted_start_stage_4)) %>%
  mutate(weighted_end_stage_1 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_1)) %>%
  mutate(weighted_end_stage_2 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_2)) %>%
  mutate(weighted_end_stage_3 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_3)) %>%
  mutate(weighted_end_stage_4 = case_when(tAge == 50 ~ 0, 
                                          TRUE ~ weighted_end_stage_4))


np_SD_res_50 <-  weighted_sd_no50_50 %>% 
  arrange(tAge) %>%
  group_by_at(SS_GROUP_BY_CANCER) %>% 
  summarize(weighted_start_stage_1_SA = first(weighted_start_stage_1_prev), #to take the prevalence screen at age 50
            weighted_start_stage_2_SA = first(weighted_start_stage_2_prev),
            weighted_start_stage_3_SA = first(weighted_start_stage_3_prev),
            weighted_start_stage_4_SA = first(weighted_start_stage_4_prev),
            weighted_end_stage_1_SA = first(weighted_end_stage_1_prev),
            weighted_end_stage_2_SA = first(weighted_end_stage_2_prev),
            weighted_end_stage_3_SA = first(weighted_end_stage_3_prev),
            weighted_end_stage_4_SA = first(weighted_end_stage_4_prev),
            weighted_start_stage_1 = sum(weighted_start_stage_1), #to sum all incident rounds of screening age 51-79
            weighted_start_stage_2 = sum(weighted_start_stage_2),
            weighted_start_stage_3 = sum(weighted_start_stage_3),
            weighted_start_stage_4 = sum(weighted_start_stage_4),
            weighted_end_stage_1 = sum(weighted_end_stage_1), 
            weighted_end_stage_2 = sum(weighted_end_stage_2),
            weighted_end_stage_3 = sum(weighted_end_stage_3),
            weighted_end_stage_4 = sum(weighted_end_stage_4)) %>%
  mutate(weighted_start_stage_1= weighted_start_stage_1 + weighted_start_stage_1_SA, 
         weighted_start_stage_2= weighted_start_stage_2 + weighted_start_stage_2_SA,
         weighted_start_stage_3= weighted_start_stage_3 + weighted_start_stage_3_SA, 
         weighted_start_stage_4= weighted_start_stage_4 + weighted_start_stage_4_SA,
         weighted_end_stage_1= weighted_end_stage_1 + weighted_end_stage_1_SA, 
         weighted_end_stage_2= weighted_end_stage_2 + weighted_end_stage_2_SA,
         weighted_end_stage_3= weighted_end_stage_3 + weighted_end_stage_3_SA, 
         weighted_end_stage_4= weighted_end_stage_4 + weighted_end_stage_4_SA) %>% 
  ungroup()%>% #proportional change for each stage (weighted)
  mutate(
    weighted_stage_1_change = weighted_end_stage_1/weighted_start_stage_1,
    weighted_stage_2_change = weighted_end_stage_2/weighted_start_stage_2,
    weighted_stage_3_change = weighted_end_stage_3/weighted_start_stage_3,
    weighted_stage_4_change = weighted_end_stage_4/weighted_start_stage_4,
  )

#Filter to base case only
np_SD_res_base_50 <- np_SD_res_50 %>%
  filter(scenario == "old_4",
         perc_skip == 0,
         model_type == "main")
#Output result
#write out excel file
write.xlsx(np_SD_res_base_50, sprintf("~/Miscellaneous/Treatments/interception_model_results/%s_np_SD_res_50.xlsx", date_code))

