#______________________________________________________________________________#
################################################################################
#___ Basic model of diagnostic use in the MCBTP

################################################################################
#______________________________________________________________________________#
#' Libraries
library(tidyverse)
library(networkD3)

#______________________________________________________________________________#
#' Data
df_CA_pop_byage <- read_csv("~/Miscellaneous/Diagnostics/data_cancer_alliance_pop_byage.csv")

df_CSO_diagnostic_assumptions <- read_csv("~/Miscellaneous/Diagnostics/CSO_diagnostics_assumptions.csv")

df_MM_cancer_classes_detected <- read_csv("~/Miscellaneous/Diagnostics/MM_cancer_detected.csv")

df_CSO_proportions <- df_MM_cancer_classes_detected %>%
        mutate(mapped_CSO = case_when(cancer_class == "Anus" ~ "Anus", 
                                      cancer_class == "Bladder" ~ "Bladder and Urothelial Tract", 
                                      cancer_class == "Breast" ~ "Breast",
                                      cancer_class == "Cervix" ~ "Cervix",
                                      cancer_class == "Colon/Rectum" ~ "Colon and Rectum",
                                      cancer_class == "Esophagus" ~ "Stomach and Esophagus",
                                      cancer_class == "Gallbladder" ~ "Pancreas and Gallbladder",
                                      cancer_class == "Head and Neck" ~ "Head and Neck",
                                      cancer_class == "Kidney" ~ "Kidney",
                                      cancer_class == "Liver/Bile-duct" ~ "Liver and Bile Duct",
                                      cancer_class == "Lung" ~ "Lung",
                                      cancer_class == "Lymphoid Leukemia" ~ "Lymphoid Lineage",
                                      cancer_class == "Lymphoma" ~ "Lymphoid Lineage",
                                      cancer_class == "Melanoma" ~ "Melanocytic Lineage",
                                      cancer_class == "Myeloid Neoplasm" ~ "Myeloid Lineage",
                                      cancer_class == "Ovary" ~ "Ovary",
                                      cancer_class == "Pancreas" ~ "Pancreas and Gallbladder",
                                      cancer_class == "Plasma Cell Neoplasm" ~ "Plasma Cell Neoplasm",
                                      cancer_class == "Prostate" ~ "Prostate",
                                      cancer_class == "Sarcoma" ~ "Bone and Soft Tissue",
                                      cancer_class == "Stomach" ~ "Stomach and Esophagus",
                                      cancer_class == "Thyroid" ~ "Thyroid Gland",
                                      cancer_class == "Urothelial Tract" ~ "Bladder and Urothelial Tract",
                                      cancer_class == "Uterus" ~ "Uterus",
                                      .default = cancer_class),
               mapped_CSO = fct(mapped_CSO)) %>%
        group_by(mapped_CSO) %>%
        summarise(rate_1M_cancertype_mapped_to_CSO = sum(rate_1M))  %>%
        mutate(prop_TP_cancertype_mapped_to_CSO= rate_1M_cancertype_mapped_to_CSO / sum(rate_1M_cancertype_mapped_to_CSO)) 


df_diag_use_England2022 <- read_csv("~/Miscellaneous/Diagnostics/NHS_England_month_diagnostic_activity2023.csv") %>%
        mutate(annual_23 = rowSums(select(., contains("23"))))

#______________________________________________________________________________#

# Future function arguments
data_population = df_CA_pop_byage
data_CSO_diagnostics = df_CSO_diagnostic_assumptions
data_CSO_probs = df_CSO_proportions
num_tests = 1000000
test_pos_rate = 0.01
GalleriPPV = 0.431
specify_CA = "All"
FP_2CSO_likelihood = 1.8
prob_TP1_CSO = 0.65
prob_1stcorrect_1_CSO = 0.925
prob_1stcorrect_2_CSO = 0.73
prob_2ndcorrect_2_CSO = 0.33
prob_incidental = 0.25

# Derived from function arguments
prob_TP2_CSO = 1 - prob_TP1_CSO
prob_incidental_2_CSO = (1-prob_2ndcorrect_2_CSO)*prob_incidental

prob_FP2_CSO = prob_TP2_CSO * FP_2CSO_likelihood
prob_FP1_CSO = 1 - prob_FP2_CSO

#______________________________________________________________________________#
# Visualisation of positive tests
total_elig <- pull(data_population %>%
                           filter(cancer_alliance == "All") %>%
                           select(total_eligible))

out1_filter_CA <- data_population %>%
        mutate(share_of_tests = total_eligible * (num_tests / total_elig)) %>%
        mutate(positives = share_of_tests * test_pos_rate) %>%
        mutate(negative = share_of_tests - positives) %>%
        mutate(true_positives = positives * GalleriPPV ) %>%
        mutate(false_positives = positives - true_positives ) %>%
        mutate(across(share_of_tests:false_positives, round)) %>%
        select(cancer_alliance, total_eligible, share_of_tests,
               positives, negative, true_positives, false_positives) %>%
        filter(cancer_alliance == specify_CA)

links <- data.frame(
        source=c("Eligible", "Eligible", "Tested", "Tested", "Positive", "Positive"), 
        target=c("Not tested", "Tested", "Negative", "Positive", "True positives", "False positives"), 
        value=c(out1_filter_CA$total_eligible - out1_filter_CA$share_of_tests, out1_filter_CA$share_of_tests,
                out1_filter_CA$negative, out1_filter_CA$positives, out1_filter_CA$true_positives, out1_filter_CA$false_positives))

nodes <- data.frame(
        name=c(as.character(links$source), 
               as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes,
                             Source = "IDsource", Target = "IDtarget",
                             Value = "value", NodeID = "name", 
                             sinksRight=FALSE)

#__________________________________________________________________#
#' Calculation of testing of positive patients.
#' Assumes uniform CSO accuracy and where the CSO call is incorrect that there 
#' is a uniform distribution of wrong CSO call, given the cancer type 
#' Assumes FPs have same CSO call distribution as TPs
out2 <- data_CSO_probs %>%
        mutate(total_true_positives = out1_filter_CA$true_positives,
               total_false_positives = out1_filter_CA$false_positives) %>%
        left_join(data_CSO_diagnostics, by = c("mapped_CSO" = "CSO_type")) %>%
        mutate(prop_1_CSO = prop_1_CSO,
               prop_2_CSO = prop_2_CSO,
               prob_1stcorrect_1_CSO = prob_1stcorrect_1_CSO,
               prob_1or2correct_2_CSO = prob_1or2correct_2_CSO,
               prob_1stcorrect_2_CSO = prob_1stcorrect_2_CSO,
               prob_2ndcorrect_2_CSO = prob_2ndcorrect_2_CSO,
               prob_incidental = prob_incidental) %>% 
        # True positives
        # Distribution by cancer types and number of CSO calls
        mutate(n_TP_distribution_cancertype_mapped_to_CSO = total_true_positives * prop_TP_cancertype_mapped_to_CSO,
               n_TP_1CSOs = n_TP_distribution_cancertype_mapped_to_CSO * prop_1_CSO,
               n_TP_2CSOs = n_TP_distribution_cancertype_mapped_to_CSO * prop_2_CSO) %>%
        # One CSO - how many of those diagnosed true positive had a wrong CSO call
        mutate(n_TP_1CSO_correct =  n_TP_1CSOs * prob_1stcorrect_1_CSO,
               n_TP_1CSO_incidental = (n_TP_1CSOs - n_TP_1CSO_correct) * prob_incidental,
               n_TP_1CSO_wrong =  n_TP_1CSOs - n_TP_1CSO_correct - n_TP_1CSO_incidental) %>%
        # two CSO - how many of those diagnosed true positive had a wrong CSO call
        mutate(n_TP_2CSO_1correct =  n_TP_2CSOs * prob_1stcorrect_2_CSO,
               n_TP_2CSO_2correct =  n_TP_2CSOs * prob_2ndcorrect_2_CSO,
               n_TP_2CSO_1incidental = (n_TP_2CSOs - n_TP_2CSO_1correct - n_TP_2CSO_2correct) * prob_incidental, # assumption
               n_TP_2CSO_wrong =  n_TP_2CSOs - n_TP_2CSO_1correct - n_TP_2CSO_2correct - n_TP_2CSO_1incidental) %>%
        # False positives
        # Distribution by cancer types and number of CSO calls
        # Assuming that the distribution of CSO calls for FPs matches the cancer type distribution 
        mutate(n_FP_distribution_CSO = total_false_positives * prop_TP_cancertype_mapped_to_CSO,
               n_FP_1CSOs = n_FP_distribution_CSO * prop_1_CSO,
               n_FP_2CSOs = n_FP_distribution_CSO * prop_2_CSO) %>%
        mutate(across(where(is.character), as.factor)) 

# Check numbers scenarios
check_numbers <- out2 %>%
  summarise_at(vars(height:mass), mean, na.rm = TRUE)

#__________________________________________________________________#
# List scenarios

#____ # Scenario 1 - True positive, 1 CSO, correct call - CSO type specific diagnostic and staging imaging
df_scenario_1 <- out2 %>%
        select(mapped_CSO, n_TP_1CSO_correct, diagnostic_imaging_1, diagnostic_imaging_2, staging_imaging_1 ) %>%
        pivot_longer(!c(mapped_CSO:n_TP_1CSO_correct), names_to = "imaging_reason", values_to = "imaging_type") %>%
        group_by(imaging_type) %>%
        summarise(scenario_1_total_use_byimagine_type = sum(n_TP_1CSO_correct))%>%
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
#' and that CSO is incorrect, assuming uniform probability of have one of the other CSO calls

f_diag_use_by_cancertype_1CSO1_incorrect <- function(x){
        
        n_cancer_minus_1 <- length(levels(out2$mapped_CSO)) - 1
        
        out3 <- out2 %>%
                filter(mapped_CSO != x) %>%
                select(mapped_CSO, diagnostic_imaging_1, diagnostic_imaging_2) %>%
                pivot_longer(!c(mapped_CSO), names_to = "imaging_reason", values_to = "imaging_type") %>%
                group_by(imaging_type, .drop = FALSE) %>%
                summarise(total_use_byimagine_type = n()) %>%
                mutate(proportional_use_byimagine_type = total_use_byimagine_type/n_cancer_minus_1) %>%
                filter(!is.na(imaging_type)) %>%
                mutate(cancer_type_CSO_mapped = x) %>%
                select(cancer_type_CSO_mapped, imaging_type, proportional_use_byimagine_type)
        
        return(out3)
}

df_scenario_2 <- map(levels(out2$mapped_CSO), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
        setNames(levels(out2$mapped_CSO)) %>%
        bind_rows() %>%
        left_join(out2 %>% select(mapped_CSO, n_TP_1CSO_incidental),
                  by = c( "cancer_type_CSO_mapped" = "mapped_CSO")) %>%
        # Add on the staging imaging used for the cancer type 
        left_join(out2 %>%
                          select(mapped_CSO, staging_imaging_1 ) %>%
                          mutate(staging_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "staging_imaging_1" )) %>%
        mutate(per_person_use_bycancer_type = rowSums(select(., proportional_use_byimagine_type, staging_imaging_1_quanity), na.rm=TRUE),
               total_use_bycancertype = per_person_use_bycancer_type * n_TP_1CSO_incidental) %>%
        group_by(imaging_type) %>%
        summarise(scenario_2_total_use_byimagine_type = sum(total_use_bycancertype)) %>%    
        ungroup()

     
#____ Scenario 3 - True positive, 1 CSO, incorrect call requires further investigation - Diagnostic imaging from other CSOs, CTCAP, and CSO type specific staging imaging

# Calculate the diagnostic use initially for first CSO call 
df_scenario_3 <- map(levels(out2$mapped_CSO), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
        setNames(levels(out2$mapped_CSO)) %>%
        bind_rows() %>%
        left_join(out2 %>% select(mapped_CSO, n_TP_1CSO_wrong),
                  by = c( "cancer_type_CSO_mapped" = "mapped_CSO")) %>%
        # Add on the CTCAP 
        left_join(out2 %>%
                          select(mapped_CSO, CSO_incorrect_imaging ) %>%
                          mutate(CSO_incorrect_imaging_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "CSO_incorrect_imaging" )) %>%
        # Add on the staging imaging used for the cancer type 
        left_join(out2 %>%
                          select(mapped_CSO, staging_imaging_1 ) %>%
                          mutate(staging_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "staging_imaging_1" )) %>%
        mutate(per_person_use_bycancer_type = rowSums(select(., proportional_use_byimagine_type, CSO_incorrect_imaging_quanity, staging_imaging_1_quanity), na.rm=TRUE),
               total_use_bycancertype = per_person_use_bycancer_type * n_TP_1CSO_wrong) %>%
        group_by(imaging_type) %>%
        summarise(scenario_3_total_use_byimagine_type = sum(total_use_bycancertype)) %>%
        ungroup()

#____ # Scenario 4 - True positive, 2 CSOs, 1st CSO correct call - CSO type specific diagnostic and staging imaging

df_scenario_4 <- out2 %>%
        select(mapped_CSO, n_TP_2CSO_1correct, diagnostic_imaging_1, diagnostic_imaging_2, staging_imaging_1 ) %>%
        pivot_longer(!c(mapped_CSO:n_TP_2CSO_1correct), names_to = "imaging_reason", values_to = "imaging_type") %>%
        group_by(imaging_type) %>%
        summarise(scenario_4_total_use_byimagine_type = sum(n_TP_2CSO_1correct))%>%
        ungroup() %>%
        filter(!is.na(imaging_type))
        
#____ # Scenario 5 - True positive, 2 CSOs, 1st CSO incorrect call but incidenatlly found - Diagnostic imaging from other CSOs, and CSO type specific staging imaging

df_scenario_5 <- map(levels(out2$mapped_CSO), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
        setNames(levels(out2$mapped_CSO)) %>%
        bind_rows() %>%
        left_join(out2 %>% select(mapped_CSO, n_TP_2CSO_1incidental),
                  by = c( "cancer_type_CSO_mapped" = "mapped_CSO")) %>%
        # Add on the staging imaging used for the cancer type 
        left_join(out2 %>%
                          select(mapped_CSO, staging_imaging_1 ) %>%
                          mutate(staging_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "staging_imaging_1" )) %>%
        mutate(per_person_use_bycancer_type = rowSums(select(., proportional_use_byimagine_type, staging_imaging_1_quanity), na.rm=TRUE),
               total_use_bycancertype = per_person_use_bycancer_type * n_TP_2CSO_1incidental) %>%
        group_by(imaging_type) %>%
        summarise(scenario_5_total_use_byimagine_type = sum(total_use_bycancertype)) %>%    
        ungroup()

#____ # Scenario 6 - True positive, 2 CSOs, 1st CSO incorrect call but 2nd correct - Diagnostic imaging from other CSOs, and CSO type specific diagnostic and staging imaging

df_scenario_6 <- map(levels(out2$mapped_CSO), f_diag_use_by_cancertype_1CSO1_incorrect) %>% 
        setNames(levels(out2$mapped_CSO)) %>%
        bind_rows() %>%
        left_join(out2 %>% select(mapped_CSO, n_TP_2CSO_1incidental),
                  by = c( "cancer_type_CSO_mapped" = "mapped_CSO")) %>%
        # Add on CSO type specific imaging (CSO 2 investigation which is correct)
        left_join(out2 %>%
                          select(mapped_CSO, diagnostic_imaging_1 ) %>%
                          mutate(diagnostic_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "diagnostic_imaging_1" )) %>%
        left_join(out2 %>%
                          select(mapped_CSO, diagnostic_imaging_2 ) %>%
                          mutate(diagnostic_imaging_2_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "diagnostic_imaging_2" )) %>%
        # Add on the staging imaging used for the cancer type 
        left_join(out2 %>%
                          select(mapped_CSO, staging_imaging_1 ) %>%
                          mutate(staging_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "staging_imaging_1" )) %>%
        mutate(per_person_use_bycancer_type = rowSums(select(., proportional_use_byimagine_type, diagnostic_imaging_1_quanity , diagnostic_imaging_2_quanity, staging_imaging_1_quanity), na.rm=TRUE),
               total_use_bycancertype = per_person_use_bycancer_type * n_TP_2CSO_1incidental) %>%
        group_by(imaging_type) %>%
        summarise(scenario_6_total_use_byimagine_type = sum(total_use_bycancertype)) %>%    
        ungroup()        
        
#____ # Scenario 7 - True positive, 2 CSOs, 1st and 2nd CSO incorrect - Diagnostic imaging from other CSOs x2, and CSO type specific diagnostic and staging imaging

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
        
        n_cancer_minus_1 <- length(levels(out2$mapped_CSO)) - 1
        n_cancer_minus_2 <- length(levels(out2$mapped_CSO)) - 2
        
        
        out4 <- out2 %>%
                # Create matrix of all possible 2 CSO combinations given actual cancer and that the CSOs can not be the same
                rename(first_CSO = mapped_CSO) %>%
                select(first_CSO) %>%
                mutate(second_CSO = first_CSO)  %>% 
                expand(first_CSO, second_CSO) %>%
                filter(first_CSO != x & second_CSO != x )%>%
                filter(first_CSO != second_CSO) %>%
                # Join on diagnostic use for first and second CSOs
                left_join(out2 %>% 
                                  select(mapped_CSO, diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
                                  rename(diagnostic_imaging_1_CSO1 = diagnostic_imaging_1, diagnostic_imaging_2_CSO1= diagnostic_imaging_2),
                          by = c("first_CSO" = "mapped_CSO")) %>%
                left_join(out2 %>% 
                                  select(mapped_CSO, diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
                                  rename(diagnostic_imaging_1_CSO2 = diagnostic_imaging_1, diagnostic_imaging_2_CSO2= diagnostic_imaging_2),
                          by = c("second_CSO" = "mapped_CSO")) %>%
                pivot_longer(!c(first_CSO:second_CSO), names_to = "imaging_reason", values_to = "imaging_type") %>%
                group_by(imaging_type, .drop = FALSE) %>%
                summarise(total_use_byimagine_type = n()) %>%
                mutate(proportional_use_byimagine_type = total_use_byimagine_type/(n_cancer_minus_1 * n_cancer_minus_2)) %>% # Would expect just left than 2 imaging events - checks out
                filter(!is.na(imaging_type)) %>%
                mutate(cancer_type_CSO_mapped = x) %>%
                select(cancer_type_CSO_mapped, imaging_type, proportional_use_byimagine_type)
        
        return(out4)
        
}

df_scenario_7 <- map(levels(out2$mapped_CSO), f_diag_use_by_cancertype_2CSOboth_incorrect) %>% 
        setNames(levels(out2$mapped_CSO)) %>%
        bind_rows() %>%
        left_join(out2 %>% select(mapped_CSO, n_TP_2CSO_wrong),
                  by = c( "cancer_type_CSO_mapped" = "mapped_CSO")) %>%
        # Add on the CTCAP 
        left_join(out2 %>%
                          select(mapped_CSO, CSO_incorrect_imaging ) %>%
                          mutate(CSO_incorrect_imaging_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "CSO_incorrect_imaging" )) %>%
        # Add on the staging imaging used for the cancer type 
        left_join(out2 %>%
                          select(mapped_CSO, staging_imaging_1 ) %>%
                          mutate(staging_imaging_1_quanity = 1),
                  by = c("cancer_type_CSO_mapped" = "mapped_CSO", "imaging_type" = "staging_imaging_1" )) %>%
        mutate(per_person_use_bycancer_type = rowSums(select(., proportional_use_byimagine_type, CSO_incorrect_imaging_quanity, staging_imaging_1_quanity), na.rm=TRUE),
               total_use_bycancertype = per_person_use_bycancer_type * n_TP_2CSO_wrong) %>%
        group_by(imaging_type) %>%
        summarise(scenario_7_total_use_byimagine_type = sum(total_use_bycancertype)) %>%
        ungroup()

#____ # Scenario 8 - False positive, 1 CSOs -  Diagnostic imaging from all proportional to cancer type CSo incidence and CTCAP

df_scenario_8 <- out2 %>%
        select(mapped_CSO, n_FP_1CSOs, diagnostic_imaging_1, diagnostic_imaging_2, CSO_incorrect_imaging ) %>%
        pivot_longer(!c(mapped_CSO:n_FP_1CSOs), names_to = "imaging_reason", values_to = "imaging_type") %>%
        group_by(imaging_type) %>%
        summarise(scenario_8_total_use_byimagine_type = sum(n_FP_1CSOs)) %>%
        ungroup() %>%
        filter(!is.na(imaging_type))

#____ # Scenario 9 - False positive, 2 CSOs -  Diagnostic imaging 1st and 2nd CSO proportional to cancer type CSO incidence and CTCAP

#' The problem here: how to assign probability distribution of the 2 CSOs. 
#' Will assume the distribution of CSO 1 is equal to cancer type incidence.
#' and the distribution of CSO 2, conditional on CSO 1, is equal to the cancer type incidence,
#' adjusted for the removal of the 1st CSO

df_scenario_9 <- out2 %>%
  rename(first_CSO = mapped_CSO) %>%
  select(first_CSO) %>%
  mutate(second_CSO = first_CSO)  %>% 
  expand(first_CSO, second_CSO) %>%
  filter(first_CSO != second_CSO) %>%
  left_join(out2 %>% select(mapped_CSO, n_FP_2CSOs),
            by = c( "first_CSO" = "mapped_CSO")) %>%
  left_join(out2 %>% select(mapped_CSO, prop_TP_cancertype_mapped_to_CSO),
            by = c( "second_CSO" = "mapped_CSO")) %>%
  # re-weight 2nd CSO proportions as there are now only 20 CSOs possible
  group_by(first_CSO) %>%
  mutate(total_prop_2ndCSO = sum(prop_TP_cancertype_mapped_to_CSO)) %>%
  ungroup() %>%
  mutate(prob_2ndCSO_adjusted = prop_TP_cancertype_mapped_to_CSO / total_prop_2ndCSO ) %>%
  # check = group_by(first_CSO) %>% mutate(check = sum(prob_2ndCSO)) = 1
  # calculate number with this 2 CSO combination
  group_by(first_CSO) %>%
  mutate(n_FP_2CSO_combination = prob_2ndCSO_adjusted * n_FP_2CSOs) %>%
  # check = mutate(check = sum(n_FP_2CSO_combination))
  ungroup() %>%
  # add on diagnotic use for 1st CSO 
  left_join(out2 %>% 
              select(mapped_CSO, diagnostic_imaging_1, diagnostic_imaging_2 ) %>%
              rename(diagnostic_imaging_1_CSO1 = diagnostic_imaging_1, diagnostic_imaging_2_CSO1= diagnostic_imaging_2),
            by = c("first_CSO" = "mapped_CSO")) %>%
  # add on diagnotic use for 2nd CSO and CTCAP
  left_join(out2 %>% 
              select(mapped_CSO, diagnostic_imaging_1, diagnostic_imaging_2, CSO_incorrect_imaging ) %>%
              rename(diagnostic_imaging_1_CSO2 = diagnostic_imaging_1, diagnostic_imaging_2_CSO2= diagnostic_imaging_2),
            by = c("second_CSO" = "mapped_CSO")) %>%
  pivot_longer(!c(first_CSO:n_FP_2CSO_combination), names_to = "imaging_reason", values_to = "imaging_type") %>%
  group_by(imaging_type, .drop = FALSE) %>%
  summarise(scenario_9_total_use_byimagine_type = sum(n_FP_2CSO_combination)) %>%
  ungroup() %>%
  filter(!is.na(imaging_type))
  

df_all_scenarios <- list(df_scenario_1, df_scenario_2, df_scenario_3, df_scenario_4,
                         df_scenario_5, df_scenario_6, df_scenario_7, df_scenario_8, df_scenario_9) %>%
  reduce(left_join, by = "imaging_type")

res_sum_byimagingtype <- df_all_scenarios %>%
  mutate(total_sum_byimagingtype = rowSums(select(., scenario_1_total_use_byimagine_type:scenario_9_total_use_byimagine_type), na.rm=TRUE)) %>%
  select(imaging_type, total_sum_byimagingtype) %>%
  mutate(total_imaging = sum(total_sum_byimagingtype))












f_generate_Galleri_outcomes <- function(data = df_CA_pop_byage,
                                       num_tests = 1000000,
                                       test_pos_rate = 0.01,
                                       GalleriPPV = 0.4,
                                       specify_CA = "All"
                                       ){
        
        total_elig <- pull(data %>%
                                   filter(cancer_alliance == "All") %>%
                                   select(total_eligible))
        
        out1 <- data %>%
                mutate(share_of_tests = total_eligible * (num_tests / total_elig)) %>%
                mutate(positives = share_of_tests * test_pos_rate) %>%
                mutate(negative = share_of_tests - positives) %>%
                mutate(true_positives = positives * GalleriPPV ) %>%
                mutate(false_positives = positives - true_positives ) %>%
                mutate(across(share_of_tests:false_positives, round)) %>%
                select(cancer_alliance, total_eligible, share_of_tests,
                       positives, negative, true_positives, false_positives)
        
        out1_filter_CA <- out1 %>%
                filter(cancer_alliance == specify_CA)
        
        links <- data.frame(
                source=c("Eligible", "Eligible", "Tested", "Tested", "Positive", "Positive"), 
                target=c("Not tested", "Tested", "Negative", "Positive", "True positives", "False positives"), 
                value=c(out1_filter_CA$total_eligible - out1_filter_CA$share_of_tests, out1_filter_CA$share_of_tests,
                        out1_filter_CA$negative, out1_filter_CA$positives, out1_filter_CA$true_positives, out1_filter_CA$false_positives))
        
        nodes <- data.frame(
                name=c(as.character(links$source), 
                       as.character(links$target)) %>% unique()
        )
        
        links$IDsource <- match(links$source, nodes$name)-1 
        links$IDtarget <- match(links$target, nodes$name)-1
        
        sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes,
                                     Source = "IDsource", Target = "IDtarget",
                                     Value = "value", NodeID = "name", 
                                     sinksRight=FALSE)
        
        return(list(out1, sankey_plot))
        
        
}

res_Galleri_outcome <- f_generate_Galleri_outcomes()










# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))

