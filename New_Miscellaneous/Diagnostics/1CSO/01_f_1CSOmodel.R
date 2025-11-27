#______________________________________________________________________________#
################################################################################
#___ Basic model of diagnostic use in the MCBTP

################################################################################

f_diag_scenarios <- function(data_CSO_diagnostics_TP = df_CSO_diagnostic_assumptions_TP,
                             data_CSO_diagnostics_FP = df_CSO_diagnostic_assumptions_FP,
                      data_CSO_probs = df_CSO_proportions,
                      intercep_model_rate_denom = 1000000,
                      participation_rate = participation,
                      GalleriPPV = 0.431,
                      prob_CSO_correct = 0.934,
                      prob_incidental = 0.25,
                      total_elig = total_eligible){
  

  #__________________________________________________________________#
  # Define path probabilities for the 3 true positive scenarios.
  # And calculate numbers and path probabilities for the false positive scenarios
  
  df_scenario_probs <- data_CSO_probs %>%
    # Scale number diagnosed by predicted number of participants and add on input probs and diagnostic use of Cancer TypetoCSO
    mutate(total_TP_num_diagnosed_bytype = ((total_elig * participation_rate)/intercep_model_rate_denom) * rate_1M_cancertype_mapped_to_CSO) %>%
    mutate(total_FP_num_bytype = (total_TP_num_diagnosed_bytype/GalleriPPV) * (1-GalleriPPV)) %>%
    left_join(data_CSO_diagnostics_TP, by = c("cancer_class_mapped" = "CSO_type")) %>%
    select(-`2WW_pathway`) %>%
    left_join(data_CSO_diagnostics_FP, by = c("cancer_class_mapped" = "CSO_type")) %>%
    # Calculate path probabilities for true positive scenarios
    mutate(prob_scenario_1_CSO_correct = prob_CSO_correct,
           prob_scenario_2_CSO_incidental = (1- prob_CSO_correct) * prob_incidental,
           prob_scenario_3_CSO_wrong =  (1 - prob_CSO_correct - ((1- prob_CSO_correct) * prob_incidental)),
           prob_scenario_4_FP = 1) %>%
    mutate(across(where(is.character), as.factor))
  
  #__________________________________________________________________#
  # For each each scenario create a data frame of number of patient by cancer type for future cross checking. 
  # And calculate imaging usage based on path probabilities, distribution of CSOs and consequent
  
  #____ # Scenario 1 - True positive, 1 CSO, correct call - CSO type specific diagnostic and staging imaging
  df_scenario_1_patients <- df_scenario_probs %>%
    select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_1_CSO_correct, diagnostic_imaging_1.x, diagnostic_imaging_2.x, staging_imaging_1.x ) %>%
    mutate(patients_byclass_scenario1 = total_TP_num_diagnosed_bytype * prob_scenario_1_CSO_correct)
  
  df_scenario_1_imaging <- df_scenario_1_patients %>%
    pivot_longer(!c(cancer_class_mapped:prob_scenario_1_CSO_correct, patients_byclass_scenario1), names_to = "imaging_reason", values_to = "imaging_type") %>%
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
      select(possible_CSOs, diagnostic_imaging_1.x, diagnostic_imaging_2.x, staging_imaging_1.x, prop_TP_cancertype_mapped_to_CSO) %>%
      mutate(total_old_prop = sum(prop_TP_cancertype_mapped_to_CSO),
             new_prop = prop_TP_cancertype_mapped_to_CSO * (1/total_old_prop)) %>%
      # check new props = 1
      # mutate(total_new_prop = sum(new_prop))
      select(possible_CSOs, new_prop, diagnostic_imaging_1.x,  diagnostic_imaging_2.x, staging_imaging_1.x ,new_prop) %>%
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
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_2_CSO_incidental),
              by = c("cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario2 = total_TP_num_diagnosed_bytype * prob_scenario_2_CSO_incidental) 
  
  df_scenario_2_imaging <- df_scenario_2_patients %>%
    mutate(total_use_bycancertype_byimage = perpatient_use_byimage_type_otherCSOs * patients_byclass_scenario2) %>%
    group_by(imaging_type) %>%
    summarise(scenario_2_total_use_byimagine_type = sum(total_use_bycancertype_byimage)) %>%    
    ungroup()
  
  #____ Scenario 3 - True positive, 1 CSO, incorrect call requires further investigation - Diagnostic imaging from other CSOs, CTCAP
  
  # Calculate the diagnostic use initially for first CSO call 
  df_scenario_3_patients <- map(levels(df_scenario_probs$cancer_class_mapped), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
    setNames(levels(df_scenario_probs$cancer_class_mapped)) %>%
    bind_rows() %>%
    left_join(df_scenario_probs %>% select(cancer_class_mapped, total_TP_num_diagnosed_bytype, prob_scenario_3_CSO_wrong),
              by = c( "cancer_class_mapped")) %>%
    mutate(patients_byclass_scenario3 = total_TP_num_diagnosed_bytype * prob_scenario_3_CSO_wrong) 
  
  df_scenario_3_imaging <- df_scenario_3_patients %>%
    # Add on the CTCAP 
    left_join(df_scenario_probs %>%
                select(cancer_class_mapped, CSO_incorrect_imaging.x ) %>%
                mutate(CSO_incorrect_imaging_quanity = 1),
              by = c( "cancer_class_mapped", "imaging_type" = "CSO_incorrect_imaging.x" )) %>%
    mutate(total_use_bycancertype_byimage = perpatient_use_byimage_type_otherCSOs * patients_byclass_scenario3) %>%
    group_by(imaging_type) %>%
    summarise(scenario_3_total_use_byimagine_type = sum(total_use_bycancertype_byimage)) %>%
    ungroup()
  
  #____ # Scenario 4 - False positive, 1 CSOs -  Diagnostic imaging from all proportional to cancer type CSo incidence and CTCAP
  # using total_FP_num_bytype here
  
  df_scenario_4_patients <- df_scenario_probs %>%
    select(cancer_class_mapped, total_FP_num_bytype, prob_scenario_4_FP, diagnostic_imaging_1.y, diagnostic_imaging_2.y, staging_imaging_1.y ,CSO_incorrect_imaging.y ) %>%
    mutate(patients_byclass_scenario4 = total_FP_num_bytype * prob_scenario_4_FP)
  
  df_scenario_4_imaging <- df_scenario_4_patients %>%
    pivot_longer(!c(cancer_class_mapped:prob_scenario_4_FP, patients_byclass_scenario4), names_to = "imaging_reason", values_to = "imaging_type") %>%
    group_by(imaging_type) %>%
    summarise(scenario_4_total_use_byimagine_type = sum(patients_byclass_scenario4)) %>%
    ungroup() %>%
    filter(!is.na(imaging_type))
  
  return(list(df_scenario_1_patients, df_scenario_1_imaging,
              df_scenario_2_patients, df_scenario_2_imaging,
              df_scenario_3_patients, df_scenario_3_imaging,
              df_scenario_4_patients, df_scenario_4_imaging))
  
}
