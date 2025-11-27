#______________________________________________________________________________#
################################################################################
#___ Basic model of diagnostic use in the MCBTP

################################################################################

f_diag_scenarios <- function(data_CSO_diagnostics = df_CSO_diagnostic_assumptions,
                      data_CSO_probs = df_CSO_proportions,
                      intercep_model_rate_denom = 1000000,
                      participation_rate = participation,
                      GalleriPPV = 0.431,
                      FP_2CSO_likelihood = 1.8,
                      prob_TP1_CSO = 0.65,
                      prob_1stcorrect_1_CSO = 0.925,
                      prob_1stcorrect_2_CSO = 0.73,
                      prob_2ndcorrect_2_CSO = 0.33,
                      prob_incidental = 0.25,
                      total_elig = total_eligible){
  
  
  # Derived from arguments
  prob_TP2_CSO = 1 - prob_TP1_CSO
  
  prob_FP2_CSO = prob_TP2_CSO * FP_2CSO_likelihood
  prob_FP1_CSO = 1 - prob_FP2_CSO
  
  #__________________________________________________________________#
  # Define path probabilities for the 7 true positive scenarios.
  # And calculate numbers and path probabilities for the 2 false positive scenarios
  
  df_scenario_probs <- data_CSO_probs %>%
    # Scale number diagnosed by predicted number of participants and add on input probs and diagnostic use of Cancer TypetoCSO
    mutate(total_TP_num_diagnosed_bytype = ((total_elig * participation_rate)/intercep_model_rate_denom) * rate_1M_cancertype_mapped_to_CSO) %>%
    mutate(total_FP_num_bytype = (total_TP_num_diagnosed_bytype/GalleriPPV) * (1-GalleriPPV)) %>%
    left_join(data_CSO_diagnostics, by = c("cancer_class_mapped" = "CSO_type")) %>%
    mutate(prob_TP1_CSO = prob_TP1_CSO,
           prob_TP2_CSO = prob_TP2_CSO,
           prob_1stcorrect_1_CSO = prob_1stcorrect_1_CSO,
           prob_1stcorrect_2_CSO = prob_1stcorrect_2_CSO,
           prob_2ndcorrect_2_CSO = prob_2ndcorrect_2_CSO,
           prob_incidental = prob_incidental) %>% 
    # Calculate path probabilities for true positive scenarios
    mutate(prob_scenario_1_1CSO_correct = prob_TP1_CSO * prob_1stcorrect_1_CSO,
           prob_scenario_2_1CSO_incidental = prob_TP1_CSO * ((1 - prob_1stcorrect_1_CSO) * prob_incidental),
           prob_scenario_3_1CSO_wrong = prob_TP1_CSO * (1 - prob_1stcorrect_1_CSO - ((1 - prob_1stcorrect_1_CSO) * prob_incidental)),
           prob_scenario_4_2CSO_1correct = prob_TP2_CSO * prob_1stcorrect_2_CSO,
           prob_scenario_5_2CSO_1incidental = prob_TP2_CSO * prob_incidental,
           prob_scenario_6_2CSO_2correct = prob_TP2_CSO * (1 - prob_1stcorrect_2_CSO -  prob_incidental) * prob_2ndcorrect_2_CSO,
           prob_scenario_7_2CSO_2incidental = prob_TP2_CSO * (1 - prob_1stcorrect_2_CSO -  prob_incidental) * prob_incidental,
           prob_scenario_8_2CSO_2wrong = prob_TP2_CSO * (1 - prob_1stcorrect_2_CSO -  prob_incidental) * (1 - prob_2ndcorrect_2_CSO -prob_incidental )) %>% 
    # check sums to 1:
    # mutate(check = rowSums(select(., prob_scenario_1_1CSO_correct:prob_scenario_8_2CSO_2wrong)))
    # Calculate path probabilities for false positive scenarios
    mutate(prob_scenario_9_1CSO = prob_FP1_CSO,
           prob_scenario_10_2CSO = prob_FP2_CSO) %>%
    mutate(across(where(is.character), as.factor))
  
  #__________________________________________________________________#
  # For each each scenario great a data frame of number of patient by cancer type 
  # for future cross checking. 
  # And calculate imaging usage based on path probabilities, distribution of CSOs and 
  # consequent
  
  #____ # Scenario 1 - True positive, 1 CSO, correct call - CSO type specific diagnostic and staging imaging
  df_scenario_1_patients <- df_scenario_probs %>%
    select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_1_1CSO_correct, diagnostic_imaging_1, diagnostic_imaging_2, staging_imaging_1 ) %>%
    mutate(patients_byclass_scenario1 = total_TP_num_diagnosed_bytype * prob_scenario_1_1CSO_correct)
  
  df_scenario_1_imaging <- df_scenario_1_patients %>%
    pivot_longer(!c(cancer_class_mapped:prob_scenario_1_1CSO_correct, patients_byclass_scenario1), names_to = "imaging_reason", values_to = "imaging_type") %>%
    group_by(imaging_type) %>%
    summarise(scenario_1_total_use_byimagine_type = sum(patients_byclass_scenario1))%>%
    ungroup() %>%
    filter(!is.na(imaging_type))
  
  #____ # Scenario 2 - True positive, 1 CSO, incorrect call but found incidentally - Diagnostic imaging from other CSOs, and CSO type specific staging imaging
  
  #' The problem here: for a true positive results where there is 1 CSO returned and that CSO is wrong
  #' the CSO called must be different to the cancer type that has been mapped to the CSO label.
  #' Therefore potentially different diagnostics are performed depending on this actual CSO call.
  #' As we don't have good information on the distribution of CSO calls, conditional on there being cancer
  #' and that the first call being incorrect, some sort of assumption will have to be made.
  #' As a starting point, it will be assumed that the distribution of CSO calls will be equal over the
  #' CSOs excluding the one that maps to the cancer type the patient truely has.
  
  #' Function: For each cancer type mapped to CSO, calculate the average diagnostic resource use when there is 1 CSO
  #' and that CSO is incorrect, assuming distribution of wrong CSO calls follows the distribution of TP cancer types reweighted to account for 
  #' the true cancer type not being included
  
  f_diag_use_by_cancertype_1CSO1_incorrect <- function(x){
    
    out <- df_scenario_probs %>%
      rename(possible_CSOs = cancer_class_mapped) %>%
      filter(possible_CSOs != x) %>%
      select(possible_CSOs, diagnostic_imaging_1, diagnostic_imaging_2, prop_TP_cancertype_mapped_to_CSO) %>%
      mutate(total_old_prop = sum(prop_TP_cancertype_mapped_to_CSO),
             new_prop = prop_TP_cancertype_mapped_to_CSO * (1/total_old_prop)) %>%
      # check new props = 1
      # mutate(total_new_prop = sum(new_prop))
      select(possible_CSOs, new_prop, diagnostic_imaging_1,  diagnostic_imaging_2, new_prop) %>%
      pivot_longer(!c(possible_CSOs:new_prop), names_to = "imaging_reason", values_to = "imaging_type") %>%
      group_by(imaging_type, .drop = TRUE) %>%
      filter(!is.na(imaging_type)) %>%
      summarise(perpatient_use_byimage_type_otherCSOs = sum(new_prop)) %>%
      # should be slightly over 1 for all but ovary which should be 1
      # mutate(check = sum(perpatient_use_byimage_type_otherCSOs))
      mutate(cancer_class_mapped = x, .before = everything())
    
    return(out)
  }
  
  
  df_scenario_2_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    # check how many total imaging per patient and use for bottom check ~ 1.10
    #group_by(cancer_class_mapped) %>% summarise(total = sum(perpatient_use_byimage_type_otherCSOs))
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_2_1CSO_incidental),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario2 = total_TP_num_diagnosed_bytype * prob_scenario_2_1CSO_incidental) 
  
  df_scenario_2_imaging <- df_scenario_2_patients %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, staging_imaging_1_quanity), na.rm=TRUE)) %>%
    mutate(total_use_bycancertype_byimage = perpatient_use_byimage_type_total * patients_byclass_scenario2) %>%
    group_by(imaging_type) %>%
    summarise(scenario_2_total_use_byimagine_type = sum(total_use_bycancertype_byimage)) %>%    
    ungroup()
  
  # Check - number of images total should be just slightly over the number of patients in this branch
  # df_scenario_2_imaging %>% summarise(total_imaging_scen2 = sum(scenario_2_total_use_byimagine_type))
  # df_scenario_2_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen2 = sum(patients_byclass_scenario2))
  
  #____ Scenario 3 - True positive, 1 CSO, incorrect call requires further investigation - Diagnostic imaging from other CSOs, CTCAP, and CSO type specific staging imaging
  
  # Calculate the diagnostic use initially for first CSO call 
  df_scenario_3_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_3_1CSO_wrong),
              by = c( "cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario3 = total_TP_num_diagnosed_bytype * prob_scenario_3_1CSO_wrong) 
  
  df_scenario_3_imaging <- df_scenario_3_patients %>%
    # Add on the CTCAP 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, CSO_incorrect_imaging ) %>%
                mutate(CSO_incorrect_imaging_quanity = 1),
              by = c( "cancer_class_mapped", "imaging_type" = "CSO_incorrect_imaging" )) %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, CSO_incorrect_imaging_quanity, staging_imaging_1_quanity), na.rm=TRUE),
           total_use_bycancertype_byimage = perpatient_use_byimage_type_total * patients_byclass_scenario3) %>%
    group_by(imaging_type) %>%
    summarise(scenario_3_total_use_byimagine_type = sum(total_use_bycancertype_byimage)) %>%
    ungroup()
  
  # Check - number of images total should be ~ 2.10 higher than number of patients
  # df_scenario_3_imaging %>% summarise(total_imaging_scen3 = sum(scenario_3_total_use_byimagine_type))
  # df_scenario_3_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen3 = sum(patients_byclass_scenario3))
  
  
  #____ # Scenario 4 - True positive, 2 CSOs, 1st CSO correct call - CSO type specific diagnostic and staging imaging
  
  df_scenario_4_patients <- df_scenario_probs %>%
    select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_4_2CSO_1correct, diagnostic_imaging_1, diagnostic_imaging_2, staging_imaging_1 ) %>%
    mutate(patients_byclass_scenario4 = total_TP_num_diagnosed_bytype * prob_scenario_4_2CSO_1correct)
  
  df_scenario_4_imaging <- df_scenario_4_patients %>%
    pivot_longer(!c(cancer_class_mapped:prob_scenario_4_2CSO_1correct, patients_byclass_scenario4), names_to = "imaging_reason", values_to = "imaging_type") %>%
    group_by(imaging_type) %>%
    summarise(scenario_4_total_use_byimagine_type = sum(patients_byclass_scenario4))%>%
    ungroup() %>%
    filter(!is.na(imaging_type))
  
  # Check - number of images total should be slightly higher than number of patients
  # df_scenario_4_imaging %>% summarise(total_imaging_scen4 = sum(scenario_4_total_use_byimagine_type))
  # df_scenario_4_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen4 = sum(patients_byclass_scenario4))
  
  
  #____ # Scenario 5 - True positive, 2 CSOs, 1st CSO incorrect call but incidentally found - Diagnostic imaging from other CSOs, and CSO type specific staging imaging
  
  df_scenario_5_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_5_2CSO_1incidental),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario5 = total_TP_num_diagnosed_bytype * prob_scenario_5_2CSO_1incidental) 
  
  
  df_scenario_5_imaging <- df_scenario_5_patients %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, staging_imaging_1_quanity), na.rm=TRUE),
           total_use_bycancertype = perpatient_use_byimage_type_total * patients_byclass_scenario5) %>%
    group_by(imaging_type) %>%
    summarise(scenario_5_total_use_byimagine_type = sum(total_use_bycancertype)) %>%    
    ungroup()
  
  # Check - number of images total should be slightly higher than number of patients ~ 1.10
  # df_scenario_5_imaging %>% summarise(total_imaging_scen5 = sum(scenario_5_total_use_byimagine_type))
  # df_scenario_5_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen4 = sum(patients_byclass_scenario5))
  
  #____ # Scenario 6 - True positive, 2 CSOs, 1st CSO incorrec, found at CSO 2
  
  # First get proportional use of CSO 1 diagnostics given correct cancer type (which is the correct CSO2)
  df_scenario_6_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_6_2CSO_2correct),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario6 = total_TP_num_diagnosed_bytype * prob_scenario_6_2CSO_2correct) 
  
  
  # Add on missing diagnostic factors for Colon and Rectum Bladder and Urothelial Tract and Anus. This is because there's only 1 CSO which uses this type of diagnostic 
  df_scenario_6_patients_add_missing_imaging_factors <- df_scenario_6_patients %>%
    filter(cancer_class_mapped %in% c("Colon and Rectum", "Bladder and Urothelial Tract", "Anus")) %>% 
    group_by(cancer_class_mapped) %>%
    slice(1) %>%
    mutate(perpatient_use_byimage_type_otherCSOs = 0) %>%
    ungroup() %>%
    mutate(imaging_type = case_when(cancer_class_mapped == "Anus" ~ "Flexi Sigmoidoscopy",
                                    cancer_class_mapped == "Bladder and Urothelial Tract" ~ "Cystoscopy",
                                    cancer_class_mapped == "Colon and Rectum" ~ "Colonoscopy"))
  
  
  df_scenario_6_imaging <- df_scenario_6_patients %>%
    # Add on missing diagnostic factors for Colon and Rectum Bladder and Urothelial Tract and Anus. This is because there's only 1 CSO which uses this type of diagnostic 
    bind_rows(.,
              df_scenario_6_patients %>%
                filter(cancer_class_mapped %in% c("Colon and Rectum", "Bladder and Urothelial Tract", "Anus")) %>% 
                group_by(cancer_class_mapped) %>%
                slice(1) %>%
                mutate(perpatient_use_byimage_type_otherCSOs = 0) %>%
                ungroup() %>%
                mutate(imaging_type = case_when(cancer_class_mapped == "Anus" ~ "Flexi Sigmoidoscopy",
                                                cancer_class_mapped == "Bladder and Urothelial Tract" ~ "Cystoscopy",
                                                cancer_class_mapped == "Colon and Rectum" ~ "Colonoscopy"))) %>%
    # Add on CSO type specific imaging (CSO 2 investigation which is correct)
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, diagnostic_imaging_1 ) %>%
                mutate(diagnostic_imaging_1_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "diagnostic_imaging_1" )) %>%
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, diagnostic_imaging_2 ) %>%
                mutate(diagnostic_imaging_2_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "diagnostic_imaging_2" )) %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c("cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, diagnostic_imaging_1_quanity , diagnostic_imaging_2_quanity, staging_imaging_1_quanity), na.rm=TRUE),
           total_use_bycancertype = perpatient_use_byimage_type_total * patients_byclass_scenario6) %>%
    # check 
    #group_by(cancer_class_mapped) %>%
    #summarise(check = sum(perpatient_use_byimage_type_total)) %>%    
    group_by(imaging_type) %>%
    summarise(scenario_6_total_use_byimagine_type = sum(total_use_bycancertype)) %>%    
    ungroup()
  
  # Check - number of images total should be over double number of patients
  df_scenario_6_imaging %>% summarise(total_imaging_scen6 = sum(scenario_6_total_use_byimagine_type))
  df_scenario_6_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen6 = sum(patients_byclass_scenario6))
  
  
  #____ # Scenario 7 - True positive, 2 CSOs, 1st and 2nd CSO incorrect found incidentally
  
  #' The problem here: Similar to the problem when there was 1 CSO call. 
  #' for a true positive results where there are 2 CSOs returned and both are wrong, they must both
  #' be different to the cancer type that has been mapped to the CSO label.
  #' Therefore potentially different diagnostics are performed depending on these actual 2 CSO call.
  #' As we don't have good information on the distribution of CSO calls, conditional on there being cancer
  #' and that the 1st and 2nd call being incorrect, some sort of assumption will have to be made.
  
  #' As a starting point, it will be assumed that the distribution of the 1st CSO calls will be equal over the
  #' CSOs excluding the one that maps to the cancer type the patient truely has. 
  #' The distribution of the 2nd CSO calls will then be equal over the CSOs excluding 
  #' the one that maps to the cancer type the patient truely has AND the previous CSO
  
  #' Function: 
  
  #' For each cancer type mapped to CSO, calculate the average diagnostic resource use when there is 1 CSO
  #' and that CSO is incorrect, assuming uniform probability of have one of the other CSO calls
  
  f_diag_use_by_cancertype_2CSOboth_incorrect <- function(x){
    
    out <- df_scenario_probs %>%
      # Create matrix of all possible 2 CSO combinations given actual cancer and that the CSOs can not be the same
      rename(first_CSO = cancer_class_mapped) %>%
      select(first_CSO) %>%
      mutate(second_CSO = first_CSO)  %>% 
      expand(first_CSO, second_CSO) %>%
      filter(first_CSO != x & second_CSO != x )%>%
      filter(first_CSO != second_CSO) %>%
      # Join on diagnostic use for first and second CSOs
      left_join(df_scenario_probs %>% 
                  select(cancer_class_mapped, prop_TP_cancertype_mapped_to_CSO,  diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
                  rename(prop_TP_cancertype_mapped_to_CSO_CSO1 = prop_TP_cancertype_mapped_to_CSO,
                         diagnostic_imaging_1_CSO1 = diagnostic_imaging_1,
                         diagnostic_imaging_2_CSO1= diagnostic_imaging_2),
                by = c("first_CSO" = "cancer_class_mapped")) %>%
      # reweight probs for CSO 1
      left_join(df_scenario_probs %>%
                  filter(cancer_class_mapped != x) %>%
                  select(cancer_class_mapped, diagnostic_imaging_1, diagnostic_imaging_2, prop_TP_cancertype_mapped_to_CSO) %>%
                  mutate(total_old_prop = sum(prop_TP_cancertype_mapped_to_CSO),
                         new_prop_CSO1 = prop_TP_cancertype_mapped_to_CSO * (1/total_old_prop)) %>%
                  select(cancer_class_mapped , new_prop_CSO1),
                by = c("first_CSO" = "cancer_class_mapped")) %>%
      # Join on diagnostic use for second CSOs
      left_join(df_scenario_probs %>% 
                  select(cancer_class_mapped, prop_TP_cancertype_mapped_to_CSO, diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
                  rename(prop_TP_cancertype_mapped_to_CSO_CSO2 = prop_TP_cancertype_mapped_to_CSO,
                         diagnostic_imaging_1_CSO2 = diagnostic_imaging_1,
                         diagnostic_imaging_2_CSO2 = diagnostic_imaging_2),
                by = c("second_CSO" = "cancer_class_mapped")) %>%
      # Create reqeighted proportions for CSO2
      group_by(first_CSO) %>%
      mutate(total_old_prop_CSO2 = sum(prop_TP_cancertype_mapped_to_CSO_CSO2),
             new_prop_CSO2 = prop_TP_cancertype_mapped_to_CSO_CSO2 * (1/total_old_prop_CSO2)) %>%
      ungroup() %>%
      mutate(prop_2CSOs_combined = new_prop_CSO1 * new_prop_CSO2) %>%
      # check this adds to 1
      #summarise(check = sum(prop_2CSOs_combined))
      select(first_CSO, second_CSO, prop_2CSOs_combined, diagnostic_imaging_1_CSO1, diagnostic_imaging_2_CSO1, diagnostic_imaging_1_CSO2, diagnostic_imaging_2_CSO2) %>%
      pivot_longer(!c(first_CSO:prop_2CSOs_combined), names_to = "imaging_reason", values_to = "imaging_type") %>%
      group_by(imaging_type, .drop = FALSE) %>%
      filter(!is.na(imaging_type)) %>%
      summarise(perpatient_use_byimage_type_otherCSOs = sum(prop_2CSOs_combined)) %>%
      # check should equal about 2
      #summarise(check = sum(perpatient_use_byimage_type_otherCSOs))
      mutate(cancer_class_mapped = x, .before = everything())
    
    return(out)
    
  }
  
  df_scenario_7_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_2CSOboth_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_7_2CSO_2incidental),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario7 = total_TP_num_diagnosed_bytype * prob_scenario_7_2CSO_2incidental) %>%
    filter(!is.na(imaging_type))
  
  df_scenario_7_imaging <- df_scenario_7_patients %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c( "cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, staging_imaging_1_quanity), na.rm=TRUE),
           total_use_bycancertype = perpatient_use_byimage_type_total * patients_byclass_scenario7) %>%
    group_by(imaging_type) %>%
    summarise(scenario_7_total_use_byimagine_type = sum(total_use_bycancertype)) %>%
    ungroup()
  
  # Check - number of images total should be over double number of patients
  #df_scenario_7_imaging %>% summarise(total_imaging_scen7 = sum(scenario_7_total_use_byimagine_type))
  #df_scenario_7_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen7 = sum(patients_byclass_scenario7))
  
  
  #____ # Scenario 8 - True positive, 2 CSOs, 1st and 2nd CSO incorrect found with further investiagation
  
  df_scenario_8_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_2CSOboth_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_8_2CSO_2wrong),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario8 = total_TP_num_diagnosed_bytype * prob_scenario_8_2CSO_2wrong) %>%
    filter(!is.na(imaging_type))
  
  df_scenario_8_imaging <- df_scenario_8_patients %>%
    # Add on the CTCAP 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, CSO_incorrect_imaging ) %>%
                mutate(CSO_incorrect_imaging_quanity = 1),
              by = c( "cancer_class_mapped", "imaging_type" = "CSO_incorrect_imaging" )) %>%
    # Add on the staging imaging used for the cancer type 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, staging_imaging_1 ) %>%
                mutate(staging_imaging_1_quanity = 1),
              by = c( "cancer_class_mapped", "imaging_type" = "staging_imaging_1" )) %>%
    mutate(perpatient_use_byimage_type_total = rowSums(select(., perpatient_use_byimage_type_otherCSOs, CSO_incorrect_imaging_quanity,staging_imaging_1_quanity), na.rm=TRUE),
           total_use_bycancertype = perpatient_use_byimage_type_total * patients_byclass_scenario8) %>%
    group_by(imaging_type) %>%
    summarise(scenario_8_total_use_byimagine_type = sum(total_use_bycancertype)) %>%
    ungroup()
  
  # Check - number of images total should be over three times number of patients
  #df_scenario_8_imaging %>% summarise(total_imaging_scen8 = sum(scenario_8_total_use_byimagine_type))
  #df_scenario_8_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen8 = sum(patients_byclass_scenario8))
  
  #__________________________________________________________________#
  
  #____ # Scenario 9 - False positive, 1 CSOs -  Diagnostic imaging from all proportional to cancer type CSo incidence and CTCAP
  # using total_FP_num_bytype here
  
  df_scenario_9_patients <- df_scenario_probs %>%
    select(cancer_class_mapped, total_FP_num_bytype, prob_scenario_9_1CSO, diagnostic_imaging_1, diagnostic_imaging_2, CSO_incorrect_imaging ) %>%
    mutate(patients_byclass_scenario9 = total_FP_num_bytype * prob_scenario_9_1CSO)
  
  df_scenario_9_imaging <- df_scenario_9_patients %>%
    pivot_longer(!c(cancer_class_mapped:prob_scenario_9_1CSO, patients_byclass_scenario9), names_to = "imaging_reason", values_to = "imaging_type") %>%
    group_by(imaging_type) %>%
    summarise(scenario_9_total_use_byimagine_type = sum(patients_byclass_scenario9)) %>%
    ungroup() %>%
    filter(!is.na(imaging_type))
  
  # Check - number of images total should be over two times number of patients
  #df_scenario_9_imaging %>% summarise(total_imaging_scen9 = sum(scenario_9_total_use_byimagine_type))
  #df_scenario_9_patients %>% group_by(cancer_class_mapped) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen9 = sum(patients_byclass_scenario9))
  
  
  #____ # Scenario 10 - False positive, 2 CSOs -  Diagnostic imaging 1st and 2nd CSO proportional to cancer type CSO incidence and CTCAP
  
  #' The problem here: how to assign probability distribution of the 2 CSOs. 
  #' Will assume the distribution of CSO 1 is equal to cancer type incidence.
  #' and the distribution of CSO 2, conditional on CSO 1, is equal to the cancer type incidence,
  #' adjusted for the removal of the 1st CSO
  
  df_scenario_10_patients <- df_scenario_probs %>%
    rename(first_CSO = cancer_class_mapped) %>%
    select(first_CSO) %>%
    mutate(second_CSO = first_CSO)  %>% 
    expand(first_CSO, second_CSO) %>%
    filter(first_CSO != second_CSO) %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_FP_num_bytype, prob_scenario_10_2CSO),
              by = c( "first_CSO" = "cancer_class_mapped")) %>%
    mutate(n_by1stCSO = total_FP_num_bytype * prob_scenario_10_2CSO) %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, prop_TP_cancertype_mapped_to_CSO),
              by = c( "second_CSO" = "cancer_class_mapped")) %>%
    # re-weight 2nd CSO proportions as there are now only 20 CSOs possible
    group_by(first_CSO) %>%
    mutate(total_prop_2ndCSO = sum(prop_TP_cancertype_mapped_to_CSO)) %>%
    ungroup() %>%
    mutate(prob_2ndCSO_adjusted = prop_TP_cancertype_mapped_to_CSO / total_prop_2ndCSO ) %>%
    # check, group_by(first_CSO) %>% mutate(check = sum(prob_2ndCSO_adjusted)) # should = 1
    # calculate number with this 2 CSO combination
    group_by(first_CSO) %>%
    mutate(n_FP_2CSO_combination = prob_2ndCSO_adjusted * n_by1stCSO) %>%
    # check, mutate(check = sum(n_FP_2CSO_combination)) %>% # should be equal to n by 1st cso
    ungroup() 
  
  df_scenario_10_imaging <- df_scenario_10_patients %>%
    # add on diagnostic use for 1st CSO 
    left_join(df_scenario_probs %>% 
                select(cancer_class_mapped, diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
                rename(diagnostic_imaging_1_CSO1 = diagnostic_imaging_1, diagnostic_imaging_2_CSO1= diagnostic_imaging_2),
              by = c("first_CSO" = "cancer_class_mapped")) %>%
    # add on diagnostic use for 2nd CSO and CTCAP
    left_join(df_scenario_probs %>% 
                select(cancer_class_mapped, diagnostic_imaging_1, diagnostic_imaging_2, CSO_incorrect_imaging ) %>%
                rename(diagnostic_imaging_1_CSO2 = diagnostic_imaging_1, diagnostic_imaging_2_CSO2= diagnostic_imaging_2),
              by = c("second_CSO" = "cancer_class_mapped")) %>%
    pivot_longer(!c(first_CSO:n_FP_2CSO_combination), names_to = "imaging_reason", values_to = "imaging_type") %>%
    group_by(imaging_type, .drop = FALSE) %>%
    summarise(scenario_10_total_use_byimagine_type = sum(n_FP_2CSO_combination)) %>%
    ungroup() %>%
    filter(!is.na(imaging_type))
  
  # Check - number of images total should be over three times number of patients
  #df_scenario_10_imaging %>% summarise(total_imaging_scen10 = sum(scenario_10_total_use_byimagine_type))
  #df_scenario_10_patients %>% group_by(first_CSO) %>% slice(1) %>% ungroup() %>% summarise(total_patient_scen10 = sum(n_by1stCSO))
  
  return(list(df_scenario_1_patients, df_scenario_1_imaging,
              df_scenario_2_patients, df_scenario_2_imaging,
              df_scenario_3_patients, df_scenario_3_imaging,
              df_scenario_4_patients, df_scenario_4_imaging,
              df_scenario_5_patients, df_scenario_5_imaging,
              df_scenario_6_patients, df_scenario_6_imaging,
              df_scenario_7_patients, df_scenario_7_imaging,
              df_scenario_8_patients, df_scenario_8_imaging,
              df_scenario_9_patients, df_scenario_9_imaging,
              df_scenario_10_patients, df_scenario_10_imaging))
  
}
